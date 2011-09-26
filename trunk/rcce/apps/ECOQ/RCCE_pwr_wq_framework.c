#include "RCCE.h"
#include "RCCE_pwr_wq.h"
#include <stdio.h>
 
int RCCE_WI_valid(void *);
int  RCCE_qsort(char *, size_t, size_t, int (*)(const void*, const void*));
/* comparison function used in routine to sort core IDs                  */
int id_compare(const void *e1, const void *e2);
 
int RCCE_setup_work_queue_teams(QUEUE_PARMS *wq_pars){ 
 
  int NP, ID, ue, size, mem, master, team_lead, team_size, local_rank;
  int test, isleader;
  int *team_member, *master_list;
 
  NP = wq_pars->NP = RCCE_num_ues();
  ID = wq_pars->ID = RCCE_ue();
  team_member = wq_pars->team_member;
  master_list = wq_pars->master_list;
 
/* determine the number of UEs in the local power domain and form teams         */
  wq_pars->team_size = team_size = RCCE_power_domain_size();
  wq_pars->team_lead = team_lead = RCCE_power_domain_master();
  if (team_lead == ID) {
    /* the team lead is the first team member                                   */
    team_member[0] = team_lead;
    size = 1;
    /* the team leads collects IDs from its team members ...                    */
    while (size<team_size) for (ue=0; ue<NP; ue++) if (ue != team_lead) {
      RCCE_recv_test((char *)(&(team_member[size])), sizeof(int), ue, &test);
      if (test) team_member[size++] = ue;
    }
    /* ... and sends the list to all other team members, after sorting it       */
    RCCE_qsort((char *)team_member, team_size, sizeof(int), id_compare);
    for (ue=1; ue<team_size; ue++) 
      RCCE_send((char *)team_member, team_size*sizeof(int), team_member[ue]);
  }
  else {
    /* team members check in with the team lead ...                             */
    RCCE_send((char *)(&ID), sizeof(int), team_lead);
    /* ... and receive the complete list of team members                        */
    RCCE_recv((char *)team_member, team_size*sizeof(int), team_lead);
  }
 
  /* we assign the UE with the highest rank the role of master. We know that
     this UE is either in a power domain by itself, or there is another UE  
     in the same power domain who is the power domain master, because the
     power domain master is always the UE in that domain with the lowest rank   */
  master = wq_pars->master = NP-1;
 
/* the team containing the overall master must remove it from its member list   */
  if (team_member[team_size-1] == master) wq_pars->team_size = --team_size;
 
  /* the overall master is not in any team                                      */
  if (ID==master) team_size = wq_pars->team_size = 0;
 
/* each UE determines its rank within the team                                  */
  local_rank = wq_pars->local_rank = 0;
  for (ue=0; ue<team_size; ue++) if (ID==team_member[ue]) 
    local_rank = wq_pars->local_rank = ue;
 
/* this code determines number of power domain leads, plus list of UEs          */
  if (ID == master) {
    wq_pars->master_number = 0;
    for (int ue=0; ue<RCCE_num_ues()-1; ue++) {
      /* ask each core whether it is a team lead or not                         */
      RCCE_recv((char *)(&isleader), sizeof(int), ue);
      if (isleader) {
        master_list[wq_pars->master_number] = ue;
        (wq_pars->master_number)++;
      }
    }
  }
  else {
    /* all cores let the master know their team lead status                     */
    isleader = (ID == team_lead);
    RCCE_send((char *)(&isleader), sizeof(int), master);
  }
 
/* all UEs report their team size and memberships                               */
//  for (ue=0; ue<NP; ue++) {
//    RCCE_barrier(&RCCE_COMM_WORLD);
//    if (ID==ue) {
//      printf("UE %d (%d) is in a team with %d members: ", ID, 
//             local_rank, team_size);
//      for (mem=0; mem<team_size; mem++) printf("%d ", team_member[mem]);
//      printf("\n");
//    }
//  }
  return (RCCE_SUCCESS);
}
 
int RCCE_queue_master_loop(void *work_item, QUEUE_PARMS *wq_pars){
 
  int ue, ignore, test, count;
 
  int size = RCCE_WI_size(work_item);
  void *address = RCCE_WI_address(work_item);
  count = 0; 
 
  if (RCCE_WI_valid(work_item)) {
 
    /* service work requests from any UE; first come, first served                */
    for (ue=0; ue<wq_pars->master_number; ue++) {
      RCCE_recv_test((char *)(&ignore), sizeof(int), wq_pars->master_list[ue], &test);
      if (test) {
//        printf("Master sends work to UE %d\n", wq_pars->master_list[ue]);
        RCCE_send((char *)address, size, wq_pars->master_list[ue]);
        count++;
        /* generate the next work item                                            */
        RCCE_new_work_item(work_item, wq_pars);
      }
    }
  }
  else {
    /*  this loop ends all teams, so must insist each team checks in              */
    for (ue=0; ue<wq_pars->master_number; ue++) {
      RCCE_recv((char *)(&ignore), sizeof(int), wq_pars->master_list[ue]);
//      printf("Master sends end of work message to UE %d\n", ue);
      RCCE_send((char *)address, size,  wq_pars->master_list[ue]);
    }
  }
    
  return(count);
}
 
int RCCE_queue_member_loop(void *work_item, QUEUE_PARMS *wq_pars) {
 
  int gimme_work, mem;
  int size = RCCE_WI_size(work_item);
  void *address = RCCE_WI_address(work_item);
 
  /* ask for work if I am a team lead                                          */
  if (wq_pars->ID == wq_pars->team_lead) {
    RCCE_send((char *)(&gimme_work), sizeof(int), wq_pars->master);
    RCCE_recv((char *)address, size, wq_pars->master);
    /* team leads parcel out the work to the workers */
    for (mem=1; mem<(wq_pars->team_size); mem++) {
        printf("Team lead %d sends work to UE %d\n", RCCE_ue(), wq_pars->team_member[mem]);
        fflush(0);
      RCCE_send((char *)address, size, wq_pars->team_member[mem]);
    }
  }
  else {
    RCCE_recv((char *)address, size, wq_pars->team_lead);
  }
  if (RCCE_WI_valid(work_item)) {
    RCCE_execute_work_item(work_item, wq_pars);
//    printf("UE %d executed work item\n", wq_pars->ID);
  }
  else {
//    printf("UE %d received stop queue task\n", RCCE_ue());
    return(1);
  }
  return(RCCE_SUCCESS);
}
