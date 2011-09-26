#include <string.h>
#include <stdio.h>
#include "RCCE.h"

int RCCE_APP(int argc, char **argv){
  int iam, bufsize=1024*64, size, i, receiver, sender,
    count_receiver, count_sender, count1, count2;
  volatile int *buffer;

  RCCE_init(&argc, &argv);

  iam      = RCCE_ue();
  receiver =1;
  sender   =0;
  size   = bufsize*sizeof(int);
  buffer = (int *) RCCE_shmalloc(size);
  count_receiver = count_sender = 0;

/**********************************************************
The sender initializes its data.
Now this is shared data so value is "seen" by both cores.
The receiver flushes its cache.
***********************************************************/
     if(iam==sender) {
        for(i=0;i<bufsize; i++) { buffer[i]=1; }
     }

     if(iam==receiver) {RCCE_DCMflush();}
  RCCE_barrier(&RCCE_COMM_WORLD);

/**********************************************************
The sender reads its data. 
It reads by creating count_sender. 
count_sender (on the sender) is 64K = 65536.
count_sender (on the receiver) is 0.

The sender modifies its data.
Now these data are in the sender's cache. So the data may not be seen by the receiver.
It might be seen by the receiver. We have no control when data from the cache are evicted.

The sender flushes its cache 
This guarantees that the receiver sees the data from the sender.
***********************************************************/
     if(iam==sender) {
        for(i=0;i<bufsize; i++) {
           count_sender +=buffer[i];
           buffer[i]++;
        }
        RCCE_DCMflush();
     }
  RCCE_barrier(&RCCE_COMM_WORLD);


/**********************************************************
The receiver reads the data.
It should see the data from  the sender.
count_receiver (on the receiver) should be 128K= 131072
count_receiver (on the sender is 0).
***********************************************************/
     if(iam==receiver) {
        for(i=0;i<bufsize; i++) { 
           count_receiver +=buffer[i]; 
        }
     }
  RCCE_barrier(&RCCE_COMM_WORLD);

/**********************************************************
count1 and count 2 are on both cores.

count2 contains the number of buffer entries that are 2 (which
should be all of them). So count2 should be 64K.

count1 should be 0.

***********************************************************/

     count1= count2= 0;
     for(i=0;i<bufsize; i++) {
        if(buffer[i]==2) count2++;
        if(buffer[i]==1) count1++;
     }

     printf("LINE %d: Core %d: count_sender: %d  count_receiver: %d  count1: %d   count2: %d\n",
       __LINE__,iam,count_sender, count_receiver,count1,count2);

  RCCE_barrier(&RCCE_COMM_WORLD);
     RCCE_shfree((t_vcharp)buffer);
     RCCE_finalize();
     return(0);
}
