typedef struct {
  int NP;
  int ID;
  int master;
  int team_lead;
  int local_rank;
  int team_size;
  int team_member[RCCE_MAXNP];
  int master_list[RCCE_MAXNP];
  int master_number;
} QUEUE_PARMS;
 
int RCCE_execute_work_item(void *, QUEUE_PARMS *);
int RCCE_setup_work_queue_teams(QUEUE_PARMS *);
int RCCE_queue_master_loop(void *, QUEUE_PARMS *);
int RCCE_new_work_item(void *, QUEUE_PARMS *);
int RCCE_queue_member_loop(void *, QUEUE_PARMS *);
int RCCE_WI_size(void *);
void *RCCE_WI_address(void *);
 
#ifdef OPENMP_
#pragma omp threadprivate(power_change)
#endif
