//
// LDST_concurrent.c
//
// Concurrent LDST backend.
//

#include <pthread.h>
#include <stdatomic.h>
#include <stdbool.h>
#include "LDST.h"
#include "LDST_debug.h"
#include "thpool.h"

#ifndef LDST_THREADPOOL_SIZE
#define LDST_THREADPOOL_SIZE 4
#endif

struct LDST_chan {
  pthread_mutex_t  chan_mut;
  LDST_t           chan_value;
  LDST_cont_t     *chan_cont;
};

struct LDST_ctxt {
  threadpool  pool;
  LDST_res_t  error;
  atomic_int  blocked_count;
};

struct Operation {
  LDST_ctxt_t     *ctxt;
  LDST_cont_t     *cont;
  LDST_t           val;
};

LDST_ctxt_t *LDST_context_create(void) {
  LDST_ctxt_t *ctxt = malloc(sizeof *ctxt);
  if (!ctxt) {
    return 0;
  }

  ctxt->pool = thpool_init(LDST_THREADPOOL_SIZE);
  if (!ctxt->pool) {
    free(ctxt);
    return 0;
  }

  ctxt->error = LDST_OK;
  atomic_init(&ctxt->blocked_count, 0);
  return ctxt;
}

LDST_res_t LDST_context_wait(LDST_ctxt_t *ctxt) {
  LOG("waiting on " PTR_FMT, PTR_VAL(ctxt));
  thpool_wait(ctxt->pool);
  LOG("finished   " PTR_FMT, PTR_VAL(ctxt));
  LDST_res_t res = ctxt->error;
  return res == LDST_OK && atomic_load_explicit(&ctxt->blocked_count, memory_order_relaxed) > 0
    ? LDST_DEADLOCK
    : res;
}

void LDST_context_destroy(LDST_ctxt_t *ctxt) {
  thpool_destroy(ctxt->pool);
  free(ctxt);
}

LDST_res_t LDST_chan_new(LDST_ctxt_t *ctxt, LDST_chan_t **chan) {
  LDST_chan_t *new_chan = malloc(sizeof *new_chan);
  LOG("created channel " PTR_FMT, PTR_VAL(new_chan));
  if (!new_chan)
    return LDST_NO_MEM;

  if (pthread_mutex_init(&new_chan->chan_mut, 0) != 0) {
    free(new_chan);
    return LDST_ERR_UNKNOWN;
  }

  new_chan->chan_cont = 0;
  *chan = new_chan;
  return LDST_OK;
}

static void run_work(void *vop) {
  // Unpack the information about what to actually run.
  struct Operation *op = vop;
  LDST_ctxt_t *ctxt = op->ctxt;
  LDST_cont_t *k = op->cont;
  LDST_t val = op->val;
  free(op);

  if (ctxt->error != LDST_OK) {
    // Don't do anything if there is already an error.
    return;
  }

  LOG("starting work " PTR_FMT " (k=" PTR_FMT ", ctxt=" PTR_FMT ")", PTR_VAL(op), PTR_VAL(k), PTR_VAL(ctxt));
  LDST_res_t res = LDST_invoke(k, ctxt, val);
  LOG("work done " PTR_FMT " (res=%d)", PTR_VAL(vop), res);

  // We don't keep track of all errors but only care about remembering any one
  // if there is one.
  if (res != LDST_OK)
    ctxt->error = res;
}

static LDST_res_t enqueue(LDST_cont_t *k, LDST_ctxt_t *ctxt, LDST_t arg) {
  struct Operation *op = malloc(sizeof *op);
  if (!op)
    return LDST_NO_MEM;

  op->cont = k;
  op->val = arg;
  op->ctxt = ctxt;

  LOG("enqueuing work " PTR_FMT " (k=" PTR_FMT ", ctxt=" PTR_FMT ")", PTR_VAL(op), PTR_VAL(k), PTR_VAL(ctxt));
  bool ok = thpool_add_work(ctxt->pool, run_work, op) == 0;
  return ok ? LDST_OK : LDST_ERR_UNKNOWN;
}

/// Returns the continuation in `chan` and sets it to `NULL` or returns `NULL`
/// and stores `k` if there is none.
///
/// REQUIRES: chan->chan_mut is locked.
static LDST_cont_t *should_suspend(LDST_cont_t *k, LDST_ctxt_t *ctxt, LDST_chan_t *chan) {
  LDST_cont_t *stored = chan->chan_cont;
  if (stored) {
    chan->chan_cont = 0;
    atomic_fetch_sub_explicit(&ctxt->blocked_count, 1, memory_order_relaxed);
  } else {
    chan->chan_cont = k;
    atomic_fetch_add_explicit(&ctxt->blocked_count, 1, memory_order_relaxed);
  }
  return stored;
}

static LDST_res_t suspend_if_needed(
    LDST_cont_t *k,
    LDST_ctxt_t *ctxt,
    LDST_chan_t *chan,
    LDST_cont_t **stored_k,
    LDST_t *value)
{
  pthread_mutex_t *mutex = &chan->chan_mut;
  if (pthread_mutex_lock(mutex) != 0) {
    return LDST_ERR_UNKNOWN;
  }

  *stored_k = should_suspend(k, ctxt, chan);
  if (*stored_k == 0 && value != 0) {
      chan->chan_value = *value;
  }

  if (pthread_mutex_unlock(mutex) != 0) {
    return LDST_ERR_UNKNOWN;
  }

  return LDST_OK;
}

LDST_res_t LDST_chan_send(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *channel, LDST_t value) {
  LDST_res_t res;
  LDST_cont_t *recv_k;
  LDST_chan_t *chan = channel;

  res = suspend_if_needed(k, ctxt, chan, &recv_k, &value);
  if (res != LDST_OK) {
    return res;
  }

  LOG("send on " PTR_FMT "%s", PTR_VAL(chan), recv_k ? "" : " [blocked]");
  if (!recv_k) {
    // We suspend the current thread because the receiving side has not arrived yet.
    return LDST_OK;
  }

  // Enqueue the receiving side.
  res = LDST_make_recv_result(chan, value, &value);
  if (res != LDST_OK) {
    return res;
  }

  res = enqueue(recv_k, ctxt, value);
  if (res != LDST_OK) {
    free(value.val_pair);
    return res;
  }

  // Continue the current thread.
  value.val_chan = chan;
  return LDST_invoke(k, ctxt, value);
}

LDST_res_t LDST_chan_recv(LDST_cont_t *k, LDST_ctxt_t *ctxt, LDST_chan_t *chan) {
  LDST_res_t res;
  LDST_cont_t *send_k;

  res = suspend_if_needed(k, ctxt, chan, &send_k, 0);
  if (res != LDST_OK) {
    return res;
  }

  LOG("recv on " PTR_FMT "%s", PTR_VAL(chan), send_k ? "" : " [blocked]");
  if (!send_k) {
    // We suspend the current thread because the sending side has not arrived yet.
    return LDST_OK;
  }

  // Create the result value for the calling thread. This either must happen
  // before the `enqueue` to benefit from the implicit channel lock or when
  // the channel mutex is still held.
  LDST_t value;
  res = LDST_make_recv_result(chan, chan->chan_value, &value);
  if (res != LDST_OK) {
    return res;
  }

  // Enqueue the sending side.
  LDST_t chan_value = { .val_chan = chan };
  res = enqueue(send_k, ctxt, chan_value);
  if (res != LDST_OK) {
    free(value.val_pair);
    return res;
  }

  // Continue the current thread.
  return LDST_invoke(k, ctxt, value);
}

LDST_res_t LDST_fork(LDST_ctxt_t *ctxt, LDST_lam_t op, LDST_t value) {
  LOG("forking execution");
  LDST_cont_t *k = malloc(sizeof(LDST_cont_t));
  if (!k) {
    return LDST_NO_MEM;
  }

  k->k_lam = op;
  k->k_next = 0;

  LDST_res_t res = enqueue(k, ctxt, value);
  if (res != LDST_OK) {
    free(k);
  }

  return res;
}

/*
struct SyncInfo {
  LDST_lam_t        op;
  LDST_t           *result;
  bool              has_result;
};

static LDST_res_t assign_k(LDST_cont_t *k, LDST_ctxt_t *ctxt, void *vinfo, LDST_t val) {
  struct SyncInfo *info = vinfo;
  *info->result = val;
  LOG("result assigned, posting");
  //uv_sem_post(&info->sem);
  return LDST_invoke(k, ctxt, val);
}

static LDST_res_t assign(LDST_cont_t *then_k, LDST_ctxt_t *ctxt, void *vinfo, LDST_t arg) {
  struct SyncInfo *info = vinfo;
  LDST_cont_t *k = malloc(sizeof(LDST_cont_t));
  if (!k)
    return LDST_NO_MEM;

  k->k_lam.lam_fp = assign_k;
  k->k_lam.lam_closure = vinfo;
  k->k_next = then_k;
  return info->op.lam_fp(k, ctxt, info->op.lam_closure, arg);
}

LDST_res_t LDST_sync(LDST_ctxt_t *ctxt, LDST_t *result, LDST_lam_t op, LDST_t value) {
  struct SyncInfo info = { .op = op, .result = result };
  if (pthread_mutex_init(&info.result_mutex, 0) != 0)
    return LDST_ERR_UNKNOWN;

  if (pthread_cond_init(&info.result_cond, 0) != 0) {
    pthread_mutex_destroy(&info.result_mutex);
    return LDST_ERR_UNKNOWN;
  }

  LOG("initial fork");
  LDST_lam_t storing_op = { assign, &info };
  LDST_res_t res = LDST_fork(ctxt, storing_op, value);

  LOG("waiting for completion, fork=%d", res);
  thpool_wait(ctxt->pool);


  LOG("result available");
//  uv_sem_destroy(&info.sem);
//  uv_loop_close(&sync_loop);

  res = LDST_merge_res(atomic_load_explicit(&detached_res, memory_order_relaxed), res);
  return atomic_load_explicit(&BlockedCount, memory_order_relaxed) > 0
    ? LDST_merge_res(LDST_DEADLOCK, res)
    : res;
}
*/
