/* this synthetic application assumes a three-dimensional
   domain of  nx*ny*nz points that is decomposed into chunks
   of different size, and that require different amounts
   of computational work.
*/
 
#include "RCCE.h"
#include "RCCE_pwr_wq.h"
#include <stdio.h>
 
#define min(x,y) ( (x) < (y) ? (x) : (y) )
#define max(x,y) ( (x) > (y) ? (x) : (y) )
 
int power_change = 1;
int BASE_F = 5; /* baseline clock divider (320 MHz) */
int HIGH_F = 3; /* high CPU clock divider (533 MHz) */
void read_and_prep_data(int, int, int, int, double*);
void do_work(int, int, int, int, int, int, int, 
             double*, double*, double*, double*, RCCE_REQUEST*);
 
#define NX     200
#define NY     200
#define NZ     100
#define NCOMP  5
#define NITER  10
#define XZONEJAGS 4
#define YZONEJAGS 4
#define STEP 3
 
typedef struct {
  struct {
    int seq_number;
  } dynamic_part;
  int npx;
  int npy;
  int kstart;
  int kend;
  int kwidth;
  int left;
  int right;
  int *isize;
  int *jsize;
  int *ksize;
  RCCE_REQUEST *request;
} WORK_ITEM;
 
int RCCE_WI_size(void *work_item) {
  return(sizeof(((WORK_ITEM *)work_item)->dynamic_part));
}
 
int RCCE_WI_valid(void *work_item) {
  return(((WORK_ITEM *)work_item)->dynamic_part.seq_number>=0);
}
 
void *RCCE_WI_address(void *work_item) {
  return((void *)(&(((WORK_ITEM *)work_item)->dynamic_part)));
}
int RCCE_APP(int argc, char **argv){
 
  int       *isize, *jsize, *ksize; 
  int       ID, NP;
  int       npx, npy, ix, iy, kstart, kend, kwidth, nrounds;
  int       i, j, k, mem, ue, iter, fdiv, vlevel;
  int       *team_member, team_size, team_lead, size, local_rank, 
      left, right, master, master_number, *master_list;
  QUEUE_PARMS wq_pars;
  WORK_ITEM work_item;
  RCCE_REQUEST request;
 
  RCCE_init(&argc, &argv);
  NP = wq_pars.NP = RCCE_num_ues();
  ID = wq_pars.ID = RCCE_ue();
 
  if (argc < 4) {
    if (ID==0) printf("Error: Need two parameters, x & y tiles, plus # rounds\n");
    return(1);
  }
 
/* read the number of subdomains (x & y-direction) from the command line        */
  npx = work_item.npx = atoi(*++argv);
  npy = work_item.npy =  atoi(*++argv);
 
/* test validity of the requested tiling; each tile must be large enough to
   divide the z-dimension amoung the members of the team                       */
  if (npx <= 0 || npy <= 0 || npx > NX || npy > NY) {
    if (ID==0) printf("Illegal tiling: %d, %d\n", npx, npy);
    RCCE_finalize();
    return(1);
  }
  nrounds = atoi(*++argv);
  if (nrounds <= 0) {power_change=0; nrounds = -nrounds;}
  
  RCCE_debug_set(RCCE_DEBUG_ALL);
  /* lower power req until we need it                                           */
  if (power_change) RCCE_iset_power(BASE_F, &request, &fdiv, &vlevel);
 
  /* form teams; copy results to local variables                                */
  RCCE_setup_work_queue_teams(&wq_pars); 
  master      = wq_pars.master;
  team_lead   = wq_pars.team_lead;
  local_rank  = wq_pars.local_rank;
  team_size   = wq_pars.team_size;
  team_member = wq_pars.team_member;
  master_list = wq_pars.master_list;  
 
  if (team_size > NZ) {
    if (ID==0) printf("Error: NZ too small: %d\n", NZ);
    RCCE_finalize();
    return(1);
  }
 
  /* define left and right neighbors                                            */
  if (local_rank>0)           work_item.left  = team_member[local_rank-1];
  else                        work_item.left  = -1;
  if (local_rank<team_size-1) work_item.right = team_member[local_rank+1];
  else                        work_item.right = -1;
 
  if (ID != master) {
    /* allocate space for the sizes of the subdomains                           */
    isize = (int *) malloc(sizeof(int)*npx);
    jsize = (int *) malloc(sizeof(int)*npy);  
    ksize = (int *) malloc(sizeof(int)*team_size);
    if (!isize || !jsize || !ksize) {
      printf("Could not allocate space for tile sizes\n");
      return(1);
    }
 
    for (k=0; k<team_size; k++) {
      ksize[k] = NZ/team_size;
      /* adjust for any leftover points                                         */
      if (k<(NZ%team_size)) ksize[k]++;
    }
    for (kstart=0, k=0; k<local_rank; k++) kstart += ksize[k];
    kend = kstart + ksize[local_rank] -1;
    kwidth = work_item.kwidth = ksize[local_rank]+2;
    work_item.kstart = kstart;
    work_item.kend   = kend;
 
    /* introduce load imbalance among subdomains by perturbing their sizes      */
    for (i=0; i<npx-1; i++) isize[i] = NX/npx;
    isize[npx-1] = NX-(NX/npx)*(npx-1);
    for (iter=0; iter<XZONEJAGS; iter++) 
    for (i=1; i<npx; i+=2) if (isize[i-1] > i) {
      isize[i-1] -= i;
      isize[i]   += i;
    }
    for (j=0; j<npy-1; j++) jsize[j] = NY/npy;
    jsize[npy-1] = NY-(NY/npy)*(npy-1);
    for (iter=0; iter<YZONEJAGS; iter++) 
    for (j=1; j<npy; j+=2) if (jsize[j-1] > j) {
      jsize[j-1] -= j;
      jsize[j]   += j;
    }
  }
 
  work_item.dynamic_part.seq_number = 0;
  work_item.request = &request;
  work_item.isize = isize;
  work_item.jsize = jsize;
  work_item.ksize = ksize;
 
  WORK_ITEM *wi = &work_item;
 
/* master goes into a loop, servicing work requests                             */
  if (ID==master) {
    int tasks_completed = 0;
    while (tasks_completed<nrounds) {
      tasks_completed += RCCE_queue_master_loop((void *)&work_item, &wq_pars);
    }
    /* master creates one more work loop to end all teams                       */
    work_item.dynamic_part.seq_number = -1;
    RCCE_queue_master_loop((void *)&work_item, &wq_pars);
  }
 
/* teams go into an endless loop, executing tasks and asking for new 
   ones when they are done                                                      */
 
  else {
    int error = 0;
    while (!error) {
      error=RCCE_queue_member_loop((void *)(&work_item), &wq_pars);
    }
  }
  
  RCCE_finalize();
  return (0);
}
 
int RCCE_execute_work_item(void *work_item, QUEUE_PARMS *wq_pars) {
 
  int ix, iy, words, fdiv, vlevel;
  double *data_frame, *flux_x, *flux_y, *flux_z;
  WORK_ITEM *wi;
  wi = (WORK_ITEM *)work_item;
    
  ix = (wi->dynamic_part.seq_number)%(wi->npx);
  iy = (wi->dynamic_part.seq_number)/(wi->npx);
  words = wi->isize[ix]*wi->jsize[iy]*(wi->kwidth)*NCOMP;
  data_frame = (double *) malloc(4*words*sizeof(double));
  if (!data_frame) {
    printf("Could not allocate %d words on UE %d\n", words, RCCE_ue());
    return(1);
  }
  flux_x = data_frame + 1*words;
  flux_y = data_frame + 2*words;
  flux_z = data_frame + 3*words;
  read_and_prep_data(wi->isize[ix], wi->jsize[iy], wi->kstart, wi->kend, data_frame);
  /* entering a high-cpu-intensity segment of the code  */
  if (power_change) RCCE_wait_power(wi->request);
  if (power_change) RCCE_iset_power(HIGH_F, wi->request, &fdiv, &vlevel);
  do_work(wi->isize[ix], wi->jsize[iy], wi->kstart, wi->kend, wi->left, wi->right, 
          wq_pars->local_rank, data_frame, flux_x, flux_y, flux_z, wi->request);
  free(data_frame);
  return(0);
}
 
 
#define FR(c,i,j,k) data_frame[(c)+NCOMP*((i)+in*((j)+(k-kstart+1)*jn))]
 
void read_and_prep_data(int in, int jn, int kstart, int kend, double *data_frame) {
  int i, j, k, c;
 
  /* initialize with smooth data */
  for (k=kstart; k<=kend; k++) for (j=0; j<jn; j++) for (i=0; i<in; i++) {
    FR(0,i,j,k) = 1.0;
    FR(1,i,j,k) = (double)(k-j)+10.0;
    FR(2,i,j,k) = (double)(i-k)+20.0;
    FR(3,i,j,k) = (double)(j-i)+30.0;
    FR(4,i,j,k) = 100.0;
  }
 
  /* add jaggedness */
  for (k=kstart; k<=kend; k++) {
    for (j=0; j<jn; j+=2) {
      for (i=0; i<in; i+=2) for (c=0; c<NCOMP; c++) FR(c,i,j,k) -= 1.0;
      for (i=1; i<in; i+=2) for (c=0; c<NCOMP; c++) FR(c,i,j,k) += 1.0;
    }
    for (j=1; j<jn; j+=2) {
      for (i=0; i<in; i+=2) for (c=0; c<NCOMP; c++) FR(c,i,j,k) -= 1.0;
      for (i=1; i<in; i+=2) for (c=0; c<NCOMP; c++) FR(c,i,j,k) += 1.0;
    }
  }
  return;
}
 
#define FLUX_X(c,i,j,k) flux_x[(c)+NCOMP*((i)+in*((j)+(k-kstart+1)*jn))]
#define FLUX_Y(c,i,j,k) flux_y[(c)+NCOMP*((i)+in*((j)+(k-kstart+1)*jn))]
#define FLUX_Z(c,i,j,k) flux_z[(c)+NCOMP*((i)+in*((j)+(k-kstart+1)*jn))]
 
void do_work(int in, int jn, int kstart, int kend, int left, int right, int rank,
             double *data_frame, double *flux_x, double *flux_y, double *flux_z,
             RCCE_REQUEST *request) {
 
  int i, j, k, c, iter, phase, fdiv, vlevel;
  double vx = 1.0, vy = 1.0, vz = 1.0;
  double dt = 0.0001;
  double mu = 1.0;
 
  for (iter=0; iter<NITER; iter++) {
 
    if (iter==2 && power_change) {
      RCCE_wait_power(request);
    }
    if (iter==NITER-2 & power_change) {
      RCCE_iset_power(BASE_F, request, &fdiv, &vlevel);
    }
    /* before each iteration we need to fill ghost points with neighbor data */
    for (phase=0; phase<2; phase++) {
      if (right != -1 && (rank+phase+1)%2) {
         RCCE_send((char *)(&FR(0,0,0,kend)),in*jn*NCOMP*sizeof(double), right);
      }
      if (left  != -1 && (rank+phase)%2) {
         RCCE_recv((char *)(&FR(0,0,0,kstart-1)),in*jn*NCOMP*sizeof(double), left);
      }
    }
    for (phase=0; phase<2; phase++) {
      if (left != -1 && (rank+phase+1)%2)
         RCCE_send((char *)(&FR(0,0,0,kstart)),in*jn*NCOMP*sizeof(double), left);
      if (right  != -1 && (rank+phase)%2) 
         RCCE_recv((char *)(&FR(0,0,0,kend+1)),in*jn*NCOMP*sizeof(double), right);
    }
    for (k=max(kstart,1); k<=min(NZ-2,kend); k++) for (j=1; j<jn-1; j++) 
    for (i=1; i<in-1; i++) 
    for (c=0; c<NCOMP; c++){
      FLUX_X(c,i,j,k) = 
        (3.0*FR(c,i+1,j+1,k  ) - 4.0*FR(c,i,j+1,k  ) + FR(c,i-1,j+1,k  ))/16.0 +
        (3.0*FR(c,i+1,j  ,k+1) - 4.0*FR(c,i,j,  k+1) + FR(c,i-1,j,  k+1))/16.0 +
        (3.0*FR(c,i+1,j+1,k+1) - 4.0*FR(c,i,j+1,k+1) + FR(c,i-1,j+1,k+1))/32.0 +
        (3.0*FR(c,i+1,j-1,k  ) - 4.0*FR(c,i,j-1,k  ) + FR(c,i-1,j-1,k  ))/16.0 +
        (3.0*FR(c,i+1,j  ,k-1) - 4.0*FR(c,i,j,  k-1) + FR(c,i-1,j,  k-1))/16.0 +
        (3.0*FR(c,i+1,j-1,k-1) - 4.0*FR(c,i,j-1,k-1) + FR(c,i-1,j-1,k-1))/32.0 +
        (3.0*FR(c,i+1,j-1,k+1) - 4.0*FR(c,i,j-1,k+1) + FR(c,i-1,j-1,k+1))/32.0 +
        (3.0*FR(c,i+1,j+1,k-1) - 4.0*FR(c,i,j+1,k-1) + FR(c,i-1,j+1,k-1))/32.0 +
        (3.0*FR(c,i+1,j  ,k  ) - 4.0*FR(c,i,j,  k  ) + FR(c,i-1,j,  k  ))/8.0;
  
      FLUX_Y(c,i,j,k) = 
        (3.0*FR(c,i+1,j+1,k  ) - 4.0*FR(c,i+1,j,k  ) + FR(c,i+1,j-1,k  ))/16.0 +
        (3.0*FR(c,i  ,j+1,k+1) - 4.0*FR(c,i  ,j,k+1) + FR(c,i  ,j-1,k+1))/16.0 +
        (3.0*FR(c,i+1,j+1,k+1) - 4.0*FR(c,i+1,j,k+1) + FR(c,i+1,j-1,k+1))/32.0 +
        (3.0*FR(c,i-1,j+1,k  ) - 4.0*FR(c,i-1,j,k  ) + FR(c,i-1,j-1,k  ))/16.0 +
        (3.0*FR(c,i  ,j+1,k-1) - 4.0*FR(c,i  ,j,k-1) + FR(c,i  ,j-1,k-1))/16.0 +
        (3.0*FR(c,i-1,j+1,k-1) - 4.0*FR(c,i-1,j,k-1) + FR(c,i-1,j-1,k-1))/32.0 +
        (3.0*FR(c,i-1,j+1,k+1) - 4.0*FR(c,i-1,j,k+1) + FR(c,i-1,j-1,k+1))/32.0 +
        (3.0*FR(c,i+1,j+1,k-1) - 4.0*FR(c,i+1,j,k-1) + FR(c,i+1,j-1,k-1))/32.0 +
        (3.0*FR(c,i  ,j+1,k  ) - 4.0*FR(c,i  ,j,k  ) + FR(c,i  ,j-1,k  ))/8.0;
  
      FLUX_Y(c,i,j,k) = 
        (3.0*FR(c,i+1,j  ,k+1) - 4.0*FR(c,i+1,j  ,k) + FR(c,i+1,j  ,k-1))/16.0 +
        (3.0*FR(c,i  ,j+1,k+1) - 4.0*FR(c,i  ,j+1,k) + FR(c,i  ,j+1,k-1))/16.0 +
        (3.0*FR(c,i+1,j+1,k+1) - 4.0*FR(c,i+1,j+1,k) + FR(c,i+1,j+1,k-1))/32.0 +
        (3.0*FR(c,i-1,j  ,k+1) - 4.0*FR(c,i-1,j  ,k) + FR(c,i-1,j  ,k-1))/16.0 +
        (3.0*FR(c,i  ,j-1,k+1) - 4.0*FR(c,i  ,j-1,k) + FR(c,i  ,j-1,k-1))/16.0 +
        (3.0*FR(c,i-1,j-1,k+1) - 4.0*FR(c,i-1,j-1,k) + FR(c,i-1,j-1,k-1))/32.0 +
        (3.0*FR(c,i-1,j+1,k+1) - 4.0*FR(c,i-1,j+1,k) + FR(c,i-1,j+1,k-1))/32.0 +
        (3.0*FR(c,i+1,j-1,k+1) - 4.0*FR(c,i+1,j-1,k) + FR(c,i+1,j-1,k-1))/32.0 +
        (3.0*FR(c,i  ,j  ,k+1) - 4.0*FR(c,i  ,j  ,k) + FR(c,i  ,j  ,k-1))/8.0;
  
      FR(c,i,j,k) += dt*(
         -1.0*(vx*FLUX_X(c,i,j,k) + vy*FLUX_Y(c,i,j,k) + vz*FLUX_Z(c,i,j,k)) + 
              FR(c,i+1,j,k) -2.0*FR(c,i,j,k) + FR(c,i-1,j,k) +
              FR(c,i,j+1,k) -2.0*FR(c,i,j,k) + FR(c,i,j-1,k) +
              FR(c,i,j,k+1) -2.0*FR(c,i,j,k) + FR(c,i,j,k-1));
    }
  }
  return;
}
 
int RCCE_new_work_item(void *work_item, QUEUE_PARMS *wq_pars) {
  WORK_ITEM *wi = (WORK_ITEM *)work_item;
  wi->dynamic_part.seq_number = (wi->dynamic_part.seq_number+1)%(wi->npx*wi->npy);
  return(RCCE_SUCCESS);
}
