#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

#ifdef _WIN32
/* R >= 4.5 requires modern Windows. Use the Kernel32 entry point without
   adding a PSAPI import-library dependency or changing compiler flags. */
#if !defined(_WIN32_WINNT) || _WIN32_WINNT < 0x0601
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0601
#endif
#ifdef PSAPI_VERSION
#undef PSAPI_VERSION
#endif
#define PSAPI_VERSION 2
#include <windows.h>
#include <psapi.h>
#elif defined(__APPLE__) && defined(__MACH__)
#include <mach/mach.h>
#elif defined(__linux__)
#include <stdio.h>
#include <unistd.h>
#endif

/* Current resident memory of this R process, including loaded libraries and
   native allocations. This is neither virtual memory nor a lifetime peak.
   The counters can include shared resident pages and are OS estimates. */
SEXP C_process_memory(void) {
    double bytes = NA_REAL;
#ifdef _WIN32
    PROCESS_MEMORY_COUNTERS counters = {0};
    counters.cb = (DWORD) sizeof(counters);
    if (GetProcessMemoryInfo(GetCurrentProcess(), &counters,
                             (DWORD) sizeof(counters)))
        bytes = (double) counters.WorkingSetSize;
#elif defined(__APPLE__) && defined(__MACH__)
    mach_task_basic_info_data_t info;
    mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
    if (task_info(mach_task_self(), MACH_TASK_BASIC_INFO,
                  (task_info_t) &info, &count) == KERN_SUCCESS &&
        count >= MACH_TASK_BASIC_INFO_COUNT)
        bytes = (double) info.resident_size;
#elif defined(__linux__)
    /* statm's second field is resident pages. Unlike smaps, this fixed-size
       kernel counter does not scan the process's memory mappings. */
    long page_size = sysconf(_SC_PAGESIZE);
    if (page_size > 0) {
        FILE *stream = fopen("/proc/self/statm", "r");
        if (stream != NULL) {
            unsigned long long total = 0, resident = 0;
            int fields = fscanf(stream, "%llu %llu", &total, &resident);
            (void) fclose(stream);
            if (fields == 2)
                bytes = (double) resident * (double) page_size;
        }
    }
#endif
    if (!R_FINITE(bytes) || bytes < 0)
        bytes = NA_REAL;
    return Rf_ScalarReal(bytes);
}
