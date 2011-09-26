//***************************************************************************************
// Power management (voltage + frequency)  routines. 
//***************************************************************************************
//
// Author: Rob F. Van der Wijngaart
//         Intel Corporation
// Date:   12/22/2010
//
//***************************************************************************************
//#define POWER_DEBUG 1
// 
// Copyright 2010 Intel Corporation
// 
//    Licensed under the Apache License, Version 2.0 (the "License");
//    you may not use this file except in compliance with the License.
//    You may obtain a copy of the License at
// 
//        http://www.apache.org/licenses/LICENSE-2.0
// 
//    Unless required by applicable law or agreed to in writing, software
//    distributed under the License is distributed on an "AS IS" BASIS,
//    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
//    See the License for the specific language governing permissions and
//    limitations under the License.
// 
#include "RCCE_lib.h"
#include "SCC_API.h"
#include "RCCE_lib_pwr.h"
 
//......................................................................................
// GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................
t_vintp frequency_change_virtual_address[RCCE_MAXNP/2]; // one frequency change register per
                                                    // tile, RCCE_MAXNP/2 tiles on chip
t_vintp RPC_virtual_address;                        // only one RPC on chip

// keep track of frequency domain masters inside voltage domain       
int       RCCE_ue_F_masters[4]; 
RCCE_COMM RCCE_V_COMM;
RCCE_COMM RCCE_F_COMM;

RCCE_COMM RCCE_P_COMM;

#ifndef SCC 
// the following structure is maintained in the MPB of one core, although space for it 
// needs to be claimed on all cores. Interpretation of queue array elements:
//     0:      no core owns this slot
//     (m+1):  core m owns this slot
//     -(m+1): core m owns this slot, and is the head of the queue
//   The queue is maintained as a circular buffer with a contiguous (in mod space) set 
// of non-zeroes, representing the RPC priority queue  
RCCE_RPC_REGULATOR *RCCE_RPC_regulator;
#endif

// the following array contains triples of voltage/VID value/max_frequency  
triple RC_V_MHz_cap[] = {
/* 0 */ {0.7, 0x70, 460},
/* 1 */ {0.8, 0x80, 598},
/* 2 */ {0.9, 0x90, 644},
/* 3 */ {1.0, 0xA0, 748},
/* 4 */ {1.1, 0xB0, 875},
/* 5 */ {1.2, 0xC0, 1024},
/* 6 */ {1.3, 0xD0, 1198}
};

int RCCE_set_power_active        = 0;
int RC_current_voltage_level     = RC_DEFAULT_VOLTAGE_LEVEL;
int RC_current_frequency_divider = RC_DEFAULT_FREQUENCY_DIVIDER;

#ifndef SCC 
long long RC_time_at_birth;
#endif
 
// tile clock change words, assuming constant router ratio of 2 
unsigned int RC_frequency_change_words[][2] = {
// rtr clock ratio, bits 25:8
/* NOP */ {    2,   0x00000000},
/*  1 */  {    2,   0x000038e0},
/*  2 */  {    2,   0x000070e1},
/*  3 */  {    2,   0x0000a8e2},
/*  4 */  {    2,   0x0000e0e3},
/*  5 */  {    2,   0x000118e4},
/*  6 */  {    2,   0x000150e5},
/*  7 */  {    2,   0x000188e6},
/*  8 */  {    2,   0x0001c0e7},
/*  9 */  {    2,   0x0001f8e8},
/* 10 */  {    2,   0x000230e9},
/* 11 */  {    2,   0x000268ea},
/* 12 */  {    2,   0x0002a0eb},
/* 13 */  {    2,   0x0002d8ec},
/* 14 */  {    2,   0x000310ed},
/* 15 */  {    2,   0x000348ee},
/* 16 */  {    2,   0x000380ef}
};

// RCCE_FDOM_masters[VDOM][i] = physical core ID of frequency domain master
// within RCCE power domain VDOM (logical power domain)
int RCCE_FDOM_masters[6][4] = {
    { 0,  2, 12, 14}, { 4,  6, 16, 18}, { 8, 10, 20, 22},
    {24, 26, 36, 38}, {28, 30, 40, 42}, {32, 34, 44, 46}
};
 
//......................................................................................
// END GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................

#ifndef SCC
// fake MallocConfigReg; just return the physical address that would be accessed on RCK
int *MallocConfigReg(unsigned int address) { return (int *)address;}
#endif
 
static int RCCE_VDOM(int coreID) {
  // back out true tile coordinates, then divide by 2 in both directions (vdoms are 2x2 
  // tiles each) and linearize to determine the logical voltage domain. That is not the
  // same as the physical voltage domain
  int x, y;
  x = X_PID(coreID);
  y = Y_PID(coreID);
 
  return ((x/2)+(y/2)*3);
}
 
static int RCCE_FDOM(int coreID) {
  // back out true tile coordinates, then linearize to detemine the logical frequency
  // domain, which equals the logical tile number. That is not the same as the
  // physical tile number
  int x, y;
  x = X_PID(coreID);
  y = Y_PID(coreID);
  return(6*y+x);
}
 
static int RCCE_voltage_domain(int ue) {
  return(RCCE_VDOM(RC_COREID[ue]));
}
 
int RCCE_power_domain() {
  return(RCCE_voltage_domain(RCCE_IAM));
}
 
static int RCCE_frequency_domain(int ue) {
  return(RCCE_FDOM(RC_COREID[ue]));
}
 
static int RCCE_voltage_domain_master() {
  return(RCCE_V_COMM.member[0]);
}
 
int RCCE_power_domain_master() {
  return(RCCE_voltage_domain_master());
}
int RCCE_power_domain_size() {
  return(RCCE_V_COMM.size);
}

#ifndef SCC
//////////////////////////////////////////////////////////////////////////////////////// 
// SUPPORT FUNCTIONS TO MAINTAIN RPC QUEUE
//////////////////////////////////////////////////////////////////////////////////////// 

static int RCCE_RPC_add_self_to_queue() {
  int ue, ueq, error, found_slot;
  t_vcharp RPC_buffer[REGULATOR_LENGTH];
  RCCE_RPC_REGULATOR *privp;
  volatile int *queue;

  // privp needs to point to space to hold the RPC regulator that is cache lined padded
  privp = (RCCE_RPC_REGULATOR *) RPC_buffer;
  queue = privp->queue;
  error = RCCE_SUCCESS;
  
  if (error=RCCE_get((t_vcharp)privp, (t_vcharp)RCCE_RPC_regulator, REGULATOR_LENGTH, RPC_ROOT))
  {
#ifdef POWER_DEBUG      
      printf("UE %d could not copy regulator from MPB\n", RCCE_IAM); fflush(NULL);
#endif POWER_DEBUG
      return(error);
  }
  else {
#ifdef POWER_DEBUG
      printf("UE %d read regulator %d, %d, %d, %d, %d, %d, clock %lld\n", RCCE_IAM,
             queue[0], queue[1], queue[2], queue[3], queue[4], queue[5], 
             privp->start_time); fflush(NULL);
#endif
  }
 
  // find the head of the queue 
  int head = -1;
  for (ue = 0; ue<RC_NUM_VOLTAGE_DOMAINS; ue++) {
    if (head==-1 && queue[ue]<0)  {
      head = ue;
      break;
    }
  }
  // if there was no head, the queue was empty; insert calling ue in first slot 
  if (head==-1) {
    queue[0] = -(RCCE_IAM+1);
  }
  else {
    // there was a head, so we add calling ue to end of queue 
    for (found_slot=0,ue = 1; ue<RC_NUM_VOLTAGE_DOMAINS; ue++) {
      ueq = (head+ue)%(RC_NUM_VOLTAGE_DOMAINS);
      if (queue[ueq]==0) {
        queue[ueq] = RCCE_IAM+1;
        found_slot = 1;
        break;
      }
    }
    if (!found_slot) {
      error = RCCE_ERROR_RPC_INTERNAL;
    }
  }
  error=RCCE_put((t_vcharp)RCCE_RPC_regulator, (t_vcharp)privp, REGULATOR_LENGTH, RPC_ROOT);
  return(error);
}
 
static long long RCCE_RPC_remove_self_from_queue_and_reset_start(int *error) {
  int ue;
  t_vcharp RPC_buffer[REGULATOR_LENGTH];
  RCCE_RPC_REGULATOR *privp;
  volatile int *queue;

  // privp needs to point to space to hold the RPC regulator that is cache lined padded
  privp = (RCCE_RPC_REGULATOR *) RPC_buffer;
  queue = privp->queue;
  *error = RCCE_SUCCESS;

  RCCE_get((t_vcharp)privp, (t_vcharp)RCCE_RPC_regulator, REGULATOR_LENGTH, RPC_ROOT);

  // find the head of the queue 
  int head = -1;
  for (ue = 0; ue<RC_NUM_VOLTAGE_DOMAINS; ue++) if (head==-1 && queue[ue]<0) {
    head = ue;
    break;
  }
 
  // if there was no head, or the head does not match the calling ue, emit error 
  if (head==-1 || queue[head] != -(RCCE_IAM+1)) {
    *error = RCCE_ERROR_RPC_INTERNAL;
    return(-1);
  }
  else {
    // found the head of the head of the queue; remove, mark the next queue item as 
    // the head, if present, and reset timer             
    queue[head] = 0;
    if (queue[(head+1)%(RC_NUM_VOLTAGE_DOMAINS)]>0) {
      queue[(head+1)%(RC_NUM_VOLTAGE_DOMAINS)] *= -1;
    }
    privp->start_time = RC_global_clock();
  }
  RCCE_put((t_vcharp)RCCE_RPC_regulator, (t_vcharp)privp, REGULATOR_LENGTH, RPC_ROOT);
  
  // return start time of request just issued, to be used in the RCCE_wait_power() call 
  return(privp->start_time);
}
 
static int RCCE_RPC_my_turn() {
  int ue;
  t_vcharp RPC_buffer[REGULATOR_LENGTH];
  RCCE_RPC_REGULATOR *privp;
  volatile int *queue;

  // privp needs to point to space to hold the RPC regulator that is cache lined padded
  privp = (RCCE_RPC_REGULATOR *) RPC_buffer;
  queue = privp->queue;

  RCCE_get((t_vcharp)privp, (t_vcharp)RCCE_RPC_regulator, REGULATOR_LENGTH, RPC_ROOT);
 
  // find the head of the queue 
  int head = -1;
  for (ue = 0; ue<(RC_NUM_VOLTAGE_DOMAINS); ue++) {
    if (head==-1 && queue[ue]<0)  {
      head = ue;
      break;
    }
  }
  // if there was no head, or the head does not match the calling ue, or time has not 
  // yet come, return failure 
  if (head==-1 || queue[head] != -(RCCE_IAM+1) ||
      RC_global_clock() < privp->start_time + RC_WAIT_CYCLES) return(0);
  else                                                  return(1);
}
 
//////////////////////////////////////////////////////////////////////////////////////// 
// END SUPPORT FUNCTIONS TO MAINTAIN RPC QUEUE
//////////////////////////////////////////////////////////////////////////////////////// 
#endif

 
// when setting new voltage, model competition for RPC by maintaining short queue of RPC 
// service requests, and by blocking the calling core if the queue is not empty 
static long long RC_set_voltage(int domain, int Vlevel, int *error){
  unsigned int vidwd;
  long long start_time = 0;
  int ready_confirmation;

//we only maintain the RPC queue on the simulator, and use the SCC auto-block feature
//for multiple outstanding RPC requests to make sure the target voltage is reached 
#ifndef SCC

  // because there can only be one power change request in flight at any time for each 
  // core, we know we can blindly add the calling core to the RPC queue; the queue 
  // update must be atomic, so we protect it with a lock                                */
 
  RCCE_acquire_lock(RPC_ROOT);
  *error = RCCE_RPC_add_self_to_queue();
  RCCE_release_lock(RPC_ROOT);
 
  // block until we have come to the front of the queue AND can start having our RPC 
  // request serviced; we keep checking whether we can remove ourself from the queue
  // without acquiring the RPC lock, for efficiency. When it looks like we're ready,
  // we acquire the lock and check again. If we mistakenly thought that our turn had
  // come because we checked the queue in the middle of an update by another core, we
  // can now confirm whether we are truly ready to have our RPC request processed.
  ready_confirmation = 0;
  while (!ready_confirmation) {
    while (!RCCE_RPC_my_turn());
    RCCE_acquire_lock(RPC_ROOT);
    if (ready_confirmation=RCCE_RPC_my_turn()) {
      start_time = RCCE_RPC_remove_self_from_queue_and_reset_start(error);
    }
    RCCE_release_lock(RPC_ROOT);
  }

#else  
//  FILE *power_file;
#ifdef POWER_DEBUG
   printf("UE %d trying to write VID word %x (level %d, V %lf) to address %x\n", 
	  RCCE_IAM, VID_word(Vlevel, domain), Vlevel, RC_V_MHz_cap[Vlevel].volt, 
          RPC_virtual_address); fflush(NULL);
#endif
   *RPC_virtual_address = VID_word(Vlevel, domain);  
   *error = RCCE_SUCCESS;
//   char name[50] = "/shared/DEMOS/ECO_Q/power"; 
//   char postfix[50]; 
//   sprintf(postfix, "%d", RCCE_power_domain()); 
//   strcat(name, postfix); 
//   printf("Core %d opens file %s and writes %d\n", RCCE_IAM, name, step); 
//   power_file = fopen(name,"w"); 
//   fprintf(power_file, "%d", step); 
//   fclose(power_file); 
#endif
  return(start_time);
}
 
 
// return for a given voltage domain the integral value that needs to be written 
// to the RPC to establish a new voltage. Bit 16 needs to be one. Shiftid is the 
// corrected value to be used to identify the physuical voltage domain. Domains 
// 2 and 6 are skipped, these are used for non-tile domains; the domain occupies 
// bits 8:10 in the control value 
int VID_control_value(int voltage_domain) {
  int VID_shift[] = {4,5,7,0,1,3};
  int shiftid = VID_shift[voltage_domain];
  return((1<<16) + (shiftid<<8));
}
 
// generate the complete word to be written to the RPC 
unsigned int VID_word(int Vlevel, int power_domain) {
    unsigned int cv, vid;
    cv = VID_control_value(power_domain);
    vid = RC_V_MHz_cap[Vlevel].VID;
  return(cv+vid);
}
 
// voltage level corresponding to the chosen frequency
int RC_voltage_level(int Fdiv) {
    int Vlevel, found, MHz;
  
    MHz = RC_GLOBAL_CLOCK_MHZ/Fdiv;
    found = 0;
    for (Vlevel=0; Vlevel<=RC_NUM_VOLTAGE_LEVELS; Vlevel++)
	if (RC_V_MHz_cap[Vlevel].MHz_cap >= MHz) {found=1; break;}
    if (found) return(Vlevel);
    else       return(-1);
}
 
// generate the complete word to be written to the CRB for tile clock freq 
unsigned int FID_word(int Fdiv, int tile_ID) {
#ifdef SCC
   unsigned int lower_bits = (*(frequency_change_virtual_address[tile_ID]))&((1<<8)-1);
#else
   unsigned int lower_bits = 0x0;
#endif
   return(((RC_frequency_change_words[Fdiv][1])<<8)+lower_bits);
}

// get the complete word from the CRB for tile clock frequency
int get_divider(int tile_ID) {
#ifdef SCC
    int step;
    unsigned int word;
    // shift to the right to get the correct bits
    word = (*(frequency_change_virtual_address[tile_ID]))>>8;
    for (step=0; step<=16; step++) {
	if (word==RC_frequency_change_words[step][1]) break;
    }
    if (word==RC_frequency_change_words[step][1]) return(step);
    else                                          return(-1);
#else
    return(0);
#endif
}

// print all tile clock dividers
void print_dividers(void) {
    int tile_ID;
    for (tile_ID=0; tile_ID<24; tile_ID++)
	printf("Clock divider for tile %d is %d\n", tile_ID, get_divider(tile_ID));
}

#ifndef SCC 
long long RC_global_clock(void){
  return(_rdtsc()-RC_time_at_birth);
}
#endif
 
// use these color functions for the power domain communicators          
int RCCE_V_color(int rank, void *nothing) {return(RCCE_VDOM(RC_COREID[rank]));}
int RCCE_F_color(int rank, void *nothing) {return(RCCE_FDOM(RC_COREID[rank]));}
 
int RCCE_init_RPC(int *RC_COREID, int RCCE_IAM, int RCCE_NP) {

  int ue, tile, i, x, y;
  void *nothing = NULL;
 
  // set the frequency domain masters of power domain containing the calling core
  for (tile=0; tile<4; tile++)
  RCCE_ue_F_masters[tile] = RCCE_FDOM_masters[RCCE_VDOM(RC_COREID[RCCE_IAM])][tile];

  // create communicators that span the local voltage (8 cores) and local frequency
  // domains, respectively 
  RCCE_comm_split(RCCE_V_color, nothing, &RCCE_V_COMM);
  RCCE_comm_split(RCCE_F_color, nothing, &RCCE_F_COMM);

  // copy the voltage domain communicator to the externally visible power communicator
  RCCE_P_COMM = RCCE_V_COMM;
 
  // compute virtual addresses of RPC register and tile clock dividers 
  RPC_virtual_address = (t_vintp) MallocConfigReg(RPC_PHYSICAL_ADDRESS);
  for (tile=0; tile<RCCE_MAXNP/2; tile++) {
    x = tile%6; y = tile/6; 
    frequency_change_virtual_address[tile] = 
         (t_vintp) MallocConfigReg(CRB_ADDR(x,y)+TILEDIVIDER); 
  }
 
#ifndef SCC
  // space for RPC regulator has been preallocated; while all UEs use the regulator
  // in RPC_ROOT's MPB, they need only compute the offset in their own MPB
  RCCE_RPC_regulator = (RCCE_RPC_REGULATOR *)(RCCE_comm_buffer[RCCE_IAM]-REGULATOR_LENGTH);
  // we do not need to initialize the regulator, zeroes are fine   
 
  // create semblance of global time (i.e. across different cores): place barrier that
  //  roughly synchronizes all cores and store value of local clock as offset 
  RCCE_barrier(&RCCE_COMM_WORLD);
  RC_time_at_birth = _rdtsc();
#endif

  printf("UE %d, Core ID %d; size of V dom %d is %d, size of F dom %d is %d\n",
         RCCE_IAM, RC_MY_COREID, RCCE_VDOM(RC_COREID[RCCE_IAM]),
         RCCE_V_COMM.size, RCCE_FDOM(RC_COREID[RCCE_IAM]), RCCE_F_COMM.size);
  fflush(NULL);
  return(RCCE_SUCCESS);
}
 
// this function takes as an input a requested new frequency divider for the
// globally distributed clock, as applied to all the tiles in the voltage
// domain containing the calling core. It automatically selects the right
// minimum voltage so support the new target frequency. The return value is
// the actual frequency divider applied.
int RCCE_iset_power(int Fdiv, RCCE_REQUEST *req, int *new_Fdiv, int *new_Vlevel) {
 
  // Fdiv: requested clock divider
    int error, ue, Vlevel;
 
  // only the domain master executes the actual request 
  if (!(RCCE_IAM == RCCE_voltage_domain_master())) return(RCCE_SUCCESS);
 
  // Cannot have more than one power stepping in flight 
  if (RCCE_set_power_active) 
    return(RCCE_error_return(RCCE_debug_RPC, RCCE_ERROR_MULTIPLE_RPC_REQUESTS));

  if (Fdiv > RC_MAX_FREQUENCY_DIVIDER) Fdiv = RC_MAX_FREQUENCY_DIVIDER;
  else 
  if (Fdiv < RC_MIN_FREQUENCY_DIVIDER) Fdiv = RC_MIN_FREQUENCY_DIVIDER;
  *new_Fdiv = Fdiv;

  // determine the voltage level for the requested frequency divider
  Vlevel = RC_voltage_level(Fdiv);
  *new_Vlevel = Vlevel;

  // if the voltage could not be adjusted to accommodate the requested frequency
  // divider, exit the function
  if (Vlevel <0) return(RCCE_error_return(RCCE_debug_RPC, RCCE_ERROR_FDIVIDER));
 
  req->release = 0;
  req->old_voltage_level   = RC_current_voltage_level;
  req->new_voltage_level   = Vlevel;
  req->old_frequency_divider = RC_current_frequency_divider;
  req->new_frequency_divider = Fdiv;

  // if new frequency divider greater than current, adjust frequency immediately;
  // this can always be done safely if the current power state is feasible
  if (req->new_frequency_divider >= req->old_frequency_divider){
    // need to set frequency divider on all tiles of the voltage domain    
    RCCE_set_frequency_divider(Fdiv, new_Fdiv);
    RC_current_frequency_divider = req->new_frequency_divider;
  } 
  
  RCCE_set_power_active = 1;
  req->start_cycle = RC_set_voltage(RCCE_voltage_domain(RCCE_IAM),
                                    Vlevel, &error);
 
  return(RCCE_error_return(RCCE_debug_RPC, error));
}

// set frequency divider on a single tile 
static int RC_set_frequency_divider(int tile_ID, int Fdiv) {

// we don't have to do anything for frequency in the emulator     
//  printf("UE %d writes FID_word %x to address %x on tile %d\n", RCCE_IAM,
//      (int *)(FID_word(Fdiv, tile_ID)), frequency_change_virtual_address[tile_ID], 
//      tile_ID);
#ifdef SCC
  *(frequency_change_virtual_address[tile_ID]) = FID_word(Fdiv, tile_ID);
#endif
 
  return(RCCE_SUCCESS);
}
 
int RCCE_wait_power(RCCE_REQUEST *req) {
 
  int new_Fdiv, ue, Fdiv;

  Fdiv = req->new_frequency_divider;
  // only the domain master executes the actual request 
  if (!(RCCE_IAM == RCCE_voltage_domain_master())) return(RCCE_SUCCESS);
 
  // if no power change request in flight, don't wait 
  if (!RCCE_set_power_active) {
    return(RCCE_error_return(RCCE_debug_RPC, RCCE_ERROR_NO_ACTIVE_RPC_REQUEST));
  }
  // unset flag indicating power change is in flight
  RCCE_set_power_active = 0;
    
  if (req->release) {
    return(RCCE_error_return(RCCE_debug_RPC, RCCE_ERROR_STALE_RPC_REQUEST));
  }
 
  // wait until target voltage has been reached
  RC_wait_voltage(req);
  RC_current_voltage_level = req->new_voltage_level;    
  // if we asked for a decrease in the clock divider, apply it now, after the
  // required target voltage has been reached.
  if (req->new_frequency_divider < req->old_frequency_divider) {
    // need to set frequency divider on all tiles of the voltage domain    
    RCCE_set_frequency_divider(Fdiv, &new_Fdiv);
    RC_current_frequency_divider = req->new_frequency_divider;    
  }
#ifdef POWER_DEBUG
  printf("UE %d at voltage level %d, frequency divider %d\n", 
     RCCE_IAM, RC_current_voltage_level, RC_current_frequency_divider);
#endif
  req->release = 1;
 
  return(RCCE_SUCCESS);
}
 
int RC_wait_voltage(RCCE_REQUEST *req) {

//COULD USE SOME ERROR HANDLING CODE HERE
#ifndef SCC 
// on the emulator we need to wait for a certain amount of time to elapse
  long long target_cycles = RC_WAIT_CYCLES + req->start_cycle;
  while (RC_global_clock() < target_cycles);
#else
// on SCC we simply reissue the RPC command. It will block until the previous 
// command has been processed
#ifdef POWER_DEBUG
  printf("UE %d writes VID word again\n", RCCE_IAM);
#endif
  // do it twice, see findings by Nikolias Ioannou
   *RPC_virtual_address = VID_word(req->new_voltage_level, RCCE_voltage_domain(RCCE_IAM));  
   *RPC_virtual_address = VID_word(req->new_voltage_level, RCCE_voltage_domain(RCCE_IAM));  
#endif

  return(RCCE_SUCCESS);
}
 
int RCCE_set_frequency_divider(int Fdiv, int *new_Fdiv) {
  int ue, Vlevel; 

  // only the domain master executes the actual request 
  if (!(RCCE_IAM == RCCE_voltage_domain_master())) return(RCCE_SUCCESS);

  if (RCCE_set_power_active) {
    return(RCCE_error_return(RCCE_debug_RPC, RCCE_ERROR_MULTIPLE_RPC_REQUESTS));
  }
 
  if (Fdiv > RC_MAX_FREQUENCY_DIVIDER) Fdiv = RC_MAX_FREQUENCY_DIVIDER;
  else 
  if (Fdiv < RC_MIN_FREQUENCY_DIVIDER) Fdiv = RC_MIN_FREQUENCY_DIVIDER;
  
  // check to see if the new frequency divider is valid
  Vlevel = RC_voltage_level(Fdiv);
  if (RC_current_voltage_level < Vlevel || Vlevel < 0)
      return(RCCE_error_return(RCCE_debug_RPC, RCCE_ERROR_FDIVIDER));
  *new_Fdiv = Fdiv;

  // need to set frequency divider on all tiles of the voltage domain    
  for (ue=0; ue<4; ue++) {
    RC_set_frequency_divider(RCCE_FDOM(RCCE_ue_F_masters[ue]), Fdiv);
  }
  return(RCCE_SUCCESS);  
}
