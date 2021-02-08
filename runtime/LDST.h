//
// LDST.h
//
// Public interface to the LDST backends.

#ifndef LDST_H__
#define LDST_H__

#include <stdlib.h>
#include <stddef.h>

#ifdef LDST_USE_GC
# define LDST__ALLOC(sz)  ldst__gc_alloc(sz)
# define LDST__FREE(p)    ((void)p)
# define LDST__DO_GC(f)   ldst__gc(f)
#else
# define LDST__ALLOC(sz)  malloc(sz)
# define LDST__FREE(p)    free(p)
# define LDST__DO_GC(f)   ((void)f)
#endif  // LDST_USE_GC

union LDST_t;
struct LDST_lam_t;
struct LDST_cont_t;
struct LDST_chan_t;

enum LDST_res_t {
  LDST__ok,
  LDST__no_mem,
  LDST__deadlock,
  LDST__unmatched_label,
  LDST__err_unknown,
};

typedef enum LDST_res_t (*LDST_fp0_t)(struct LDST_cont_t *);
typedef enum LDST_res_t (*LDST_fp_t)(struct LDST_cont_t *, void *, union LDST_t);

struct LDST_lam_t {
  LDST_fp_t            lam_fp;
  void                *lam_closure;
};

struct LDST_cont_t {
  struct LDST_lam_t    k_lam;
  struct LDST_cont_t  *k_next;
};

union LDST_t {
  int                  val_int;
  struct LDST_lam_t    val_lam;
  union LDST_t        *val_pair;
  struct LDST_chan_t  *val_chan;
  const char          *val_label;
};

// Backend interface.

/// Creates a new channel.
enum LDST_res_t ldst__chan_new(struct LDST_chan_t **chan);

/// Sends a value down a channel.
///
/// The `channel` should be a channel obtained through a call to
/// `ldst__chan_new`. It is of type `void*` to fit the `LDST_fp_t` prototype.
///
/// The continuation `k` will be invoked with a value of `val_chan` passing the
/// `channel` argument given to this function.
enum LDST_res_t ldst__chan_send(struct LDST_cont_t *k, void *channel, union LDST_t value);

/// Receives a value from a channel.
///
/// The continuation `k` will be invoked with a pair of the passed channel and the
/// recieved value.
enum LDST_res_t ldst__chan_recv(struct LDST_cont_t *k, struct LDST_chan_t *channel);

/// Forks execution of the given lambda.
///
/// If currently no thread is executing this will start execution and only
/// return when all threads forked inside `op` and `op` itsel have completed.
enum LDST_res_t ldst__fork(struct LDST_lam_t op, union LDST_t value);


// Supporting functions.

// Runs the given top level function `f` by applying the `n` arguments in
// `args` and storing the result in `result`.
//
// `result` should point to a valid memory location.
//
// If `n` is zero, `args` may be the null pointer. If the result is a lambda
// function, channel or pair containing either of these, `args` has to be valid
// for as long as `result` is valid. Otherwise `args` must only be valid for
// the duration of the call to `ldst__run`.
enum LDST_res_t ldst__run(union LDST_t *result, LDST_fp0_t f, int n, union LDST_t *args);

// Implementation detail of `natrec`.
enum LDST_res_t ldst__nat_fold(struct LDST_cont_t *k, void *closure, union LDST_t value);


/// Invokes the given continuation.
static inline enum LDST_res_t ldst__invoke(struct LDST_cont_t *k, union LDST_t value) {
  if (!k)
    return LDST__ok;

  struct LDST_lam_t lam = k->k_lam;
  struct LDST_cont_t *next = k->k_next;
  LDST__FREE(k);
  return lam.lam_fp(next, lam.lam_closure, value);
}

// Garbage collector.

/// A pointer to a function used for enumerating GC roots.
///
/// The value of `*context` is at the functions disposal, the first time it is
/// called it will be the `NULL` pointer.
typedef void *(*gc_root_visitor_t)(void **context);

/// Allocates memory of `size` bytes.
///
/// The memory will be automatically freed during a call to `ldst__gc` when
/// it isn't reachable from any of the visited roots.
///
/// If allocation fails the result will be the NULL pointer.
void *ldst__gc_alloc(size_t size);

/// Run the garbage collector.
///
/// The `visitor` is responsible for enumerating all current roots. It will be
/// called for new roots as long as it does not return a `NULL` pointer.
void ldst__gc(gc_root_visitor_t visitor);

#endif  // LDST_H__
