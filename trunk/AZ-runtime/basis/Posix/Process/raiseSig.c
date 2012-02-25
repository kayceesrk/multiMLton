#include "platform.h"

C_Errno_t(C_Int_t) Posix_Process_raiseSig (C_Signal_t s) {
    return raise (s);
}
