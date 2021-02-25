//
// LDST.h
//
// Public interface to the LDST backends.

#ifndef LDST_H__
#define LDST_H__

#include <stdlib.h>
#include <stddef.h>


/*****************************************************************************
 * Type Definitions                                                          *
 *****************************************************************************/

typedef union  LDST_val  LDST_t;
typedef struct LDST_lam  LDST_lam_t;
typedef struct LDST_cont LDST_cont_t;
typedef struct LDST_chan LDST_chan_t;
typedef struct LDST_ctxt LDST_ctxt_t;

typedef enum LDST_res {
  LDST_OK,
  LDST_NO_MEM,            // Allocation failure
  LDST_DEADLOCK,          // Deadlock detected
  LDST_UNMATCHED_LABEL,   // Match on an unhandled label
  LDST_NO_RESULT,         // The continuation was never invoked for `LDST_sync`
  LDST_ERR_UNKNOWN,       // Some other error occured
} LDST_res_t;

typedef LDST_res_t (*LDST_fp0_t)(LDST_cont_t *k, LDST_ctxt_t *ctxt);
typedef LDST_res_t (*LDST_fp_t)(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *closure, LDST_t arg);

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


/*****************************************************************************
 * Context Handling                                                          *
 *****************************************************************************/

// Creates a new context which can be used to run LDST functions.
//
// If this function returns `NULL` context creation should be considered
// failed.
LDST_ctxt_t *LDST_context_create(void);

// Destroys the given context.
//
// After a context has been destroyed it may not be used as an argument to any
// of the `LDST_...` functions or any generated `ldst_...` function, including
// `LDST_context_destroy`. It is safe to pass `NULL` to this function.
//
// This also invalidates all values and channels created with this context or
// returned from running functions using this context.
void LDST_context_destroy(LDST_ctxt_t *context);


/*****************************************************************************
 * Backend Interface                                                         *
 *****************************************************************************/

// Creates a new channel.
LDST_res_t LDST_chan_new(LDST_ctxt_t *ctxt, LDST_chan_t **chan);

// Sends a value down a channel.
//
// The `channel` should be a channel obtained through a call to
// `ldst_chan_new`. It is of type `void*` to fit the `LDST_fp_t` prototype.
//
// The continuation `k` will be invoked with a value of `val_chan` passing the
// `channel` argument given to this function.
LDST_res_t LDST_chan_send(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *channel, LDST_t value);

// Receives a value from a channel.
//
// The continuation `k` will be invoked with a pair of the passed channel and the
// recieved value.
LDST_res_t LDST_chan_recv(LDST_cont_t *k, LDST_ctxt_t *ctxt, LDST_chan_t *channel);

// Forks execution of the given lambda.
LDST_res_t LDST_fork(LDST_ctxt_t *ctxt, LDST_lam_t op, LDST_t value);


/*****************************************************************************
 * Supporting Functions                                                      *
 *****************************************************************************/

// Runs the given top level function `f` by applying the `n` arguments in
// `args` and storing the result in `result`.
//
// `result` should point to a valid memory location.
//
// If `n` is zero, `args` may be the null pointer. If the result is a lambda
// function, channel or pair containing either of these, `args` has to be valid
// for as long as `result` is valid. Otherwise `args` must only be valid for
// the duration of the call to `ldst_run`.
LDST_res_t LDST_run(LDST_ctxt_t *ctxt, LDST_t *result, LDST_fp0_t f, int n, LDST_t *args);

// A synchronous version of `LDST_fork`.
//
// When this function exits an the return value is `LDST_OK` then `*ctxt` will
// contain the result of applying `op` to `arg`.
LDST_res_t LDST_sync(LDST_ctxt_t *ctxt, LDST_t *result, LDST_lam_t op, LDST_t arg);

// Runs the given top level function, no arguments are applied and the result
// is returned.
//
// If an error occurs this function will call exit(3) with the error code
// after printing an error description.
//
// This function creates a new context and uses it to run the function. The
// context won't be destroyed but since it is inaccessible to the caller the
// returned value should not be used further other than inspecting it.
LDST_t LDST_main(LDST_fp0_t f);

// Implementation detail of `natrec`.
LDST_res_t LDST_nat_fold(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *closure, LDST_t value);

// Implementation detail of channel operations.
LDST_res_t LDST_make_recv_result(LDST_chan_t *chan, LDST_t value, LDST_t *result);

// Invokes the given continuation and frees its memory.
static inline LDST_res_t LDST_invoke(LDST_cont_t *k, LDST_ctxt_t *ctxt, LDST_t value) {
  if (!k)
    return LDST_OK;

  LDST_lam_t lam = k->k_lam;
  LDST_cont_t *next = k->k_next;
  free(k);
  return lam.lam_fp(next, ctxt, lam.lam_closure, value);
}

#endif  // LDST_H__
