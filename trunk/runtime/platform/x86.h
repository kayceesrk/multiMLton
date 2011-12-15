#define MLton_Platform_Arch_host "x86"

#define rdtscll(val) __asm__ volatile("rdtsc" : "=A" (val));
