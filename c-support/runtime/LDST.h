//
// LDST.h
//
// Public interface to the LDST backends.

#ifndef LDST_H__
#define LDST_H__

#include <stdlib.h>
#include <stddef.h>

typedef union  LDST_val  LDST_t;
typedef struct LDST_lam  LDST_lam_t;
typedef struct LDST_cont LDST_cont_t;
typedef struct LDST_chan LDST_chan_t;
typedef struct LDST_ctxt LDST_ctxt_t;

typedef enum LDST_res {
  LDST__ok,
  LDST__no_mem,
  LDST__deadlock,
  LDST__unmatched_label,
  LDST__err_unknown,
} LDST_res_t;

typedef LDST_res_t (*LDST_fp0_t)(LDST_cont_t *k);
typedef LDST_res_t (*LDST_fp_t)(LDST_cont_t *k, void *closure, LDST_t arg);

struct LDST_lam {
  LDST_fp_t            lam_fp;
  void                *lam_closure;
};

struct LDST_cont {
  LDST_lam_t    k_lam;
  LDST_cont_t  *k_next;
};

union LDST_val {
  int            val_int;
  LDST_lam_t     val_lam;
  LDST_t        *val_pair;
  LDST_chan_t   *val_chan;
  const char    *val_label;
};

// Backend interface.

/// Creates a new channel.
LDST_res_t ldst_chan_new(LDST_chan_t **chan);

/// Sends a value down a channel.
///
/// The `channel` should be a channel obtained through a call to
/// `ldst_chan_new`. It is of type `void*` to fit the `LDST_fp_t` prototype.
///
/// The continuation `k` will be invoked with a value of `val_chan` passing the
/// `channel` argument given to this function.
LDST_res_t ldst_chan_send(LDST_cont_t *k, void *channel, LDST_t value);

/// Receives a value from a channel.
///
/// The continuation `k` will be invoked with a pair of the passed channel and the
/// recieved value.
LDST_res_t ldst_chan_recv(LDST_cont_t *k, LDST_chan_t *channel);

/// Forks execution of the given lambda.
///
/// If currently no thread is executing this will start execution and only
/// return when all threads forked inside `op` and `op` itsel have completed.
LDST_res_t ldst_fork(LDST_lam_t op, LDST_t value);


// Supporting functions.

/// Runs the given top level function `f` by applying the `n` arguments in
/// `args` and storing the result in `result`.
///
/// `result` should point to a valid memory location.
///
/// If `n` is zero, `args` may be the null pointer. If the result is a lambda
/// function, channel or pair containing either of these, `args` has to be valid
/// for as long as `result` is valid. Otherwise `args` must only be valid for
/// the duration of the call to `ldst_run`.
LDST_res_t ldst_run(LDST_t *result, LDST_fp0_t f, int n, LDST_t *args);

/// Runs the given top level function, no arguments are applied and the result
/// is returned.
///
/// If an error occurs this function will call exit(3) with the error code
/// after printing an error description.
LDST_t ldst_main(LDST_fp0_t f);

/// Implementation detail of `natrec`.
LDST_res_t ldst_nat_fold(LDST_cont_t *k, void *closure, LDST_t value);

/// Implementation detail of channel operations.
LDST_res_t ldst_make_recv_result(LDST_chan_t *chan, LDST_t value, LDST_t *result);


/// Invokes the given continuation.
static inline LDST_res_t ldst_invoke(LDST_cont_t *k, LDST_t value) {
  if (!k)
    return LDST__ok;

  LDST_lam_t lam = k->k_lam;
  LDST_cont_t *next = k->k_next;
  free(k);
  return lam.lam_fp(next, lam.lam_closure, value);
}

#endif  // LDST_H__
