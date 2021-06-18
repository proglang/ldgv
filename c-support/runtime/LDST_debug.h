//
// LDST_debug.h
//
// Debug macros.  This provides formatted output to stderr, colorized on which
// thread it is running and with support for colorized pointer/address output
// to differentiate the pointer values better.
//

#ifndef LDST_DEBUG_H__
#define LDST_DEBUG_H__

#include <inttypes.h>
#include <pthread.h>
#include <stdint.h>
#include <stdio.h>

#ifdef LDST_DEBUG

#define LDST_DBG_COLOR(n)     (int)((n) * ((n) + 3) % (228L - 21L) + 21L)
#define LDST_DBG_COLOR_FMT    "\033[38;5;%dm"
#define LDST_DBG_COLOR_RESET  "\033[m"

#define PTR_FMT     LDST_DBG_COLOR_FMT "%p" LDST_DBG_COLOR_RESET
#define PTR_VAL(p)  LDST_DBG_COLOR((uintptr_t)(p)), p

#ifdef __APPLE__
#define LDST_DBG_THREADID(var) pthread_threadid_np(0, &var)
#else
// `pthread` is a pointer, just use this value.
#define LDST_DBG_THREADID(var) var = (uintptr_t)pthread_self();
#endif

#define LOG(fmt, ...) do {                                  \
  uint64_t tid__ ## __LINE__;                               \
  LDST_DBG_THREADID(tid__ ## __LINE__);                     \
  printf(                                                   \
    LDST_DBG_COLOR_FMT                                      \
      "[%" PRId64 "] %s:%d  " fmt                           \
    LDST_DBG_COLOR_RESET "\n",                              \
      LDST_DBG_COLOR(tid__ ## __LINE__),                    \
      tid__ ## __LINE__, __func__, __LINE__,                \
      ##__VA_ARGS__);                                       \
} while (0)

#else  // LDST_DEBUG

#define LOG(fmt, ...)

#endif  // LDST_DEBUG

#endif  // LDST_DEBUG_H__
