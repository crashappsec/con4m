#include <stdlib.h>
#include <stdbool.h>
#include <pthread.h>
#include <stdalign.h>
#include <stdatomic.h>

#ifndef HATRACK_THREADS_MAX
#define HATRACK_THREADS_MAX 4096
#endif

#if HATRACK_THREADS_MAX > 32768
#error "Vector assumes HATRACK_THREADS_MAX is no higher than 32768"
#endif

typedef void (*mmm_cleanup_func)(void *, void *);

typedef struct mmm_header_st    mmm_header_t;
typedef struct mmm_free_tids_st mmm_free_tids_t;

extern __thread int64_t        mmm_mytid;
extern __thread pthread_once_t mmm_inited;
extern _Atomic  uint64_t       mmm_epoch;
extern          uint64_t       mmm_reservations[HATRACK_THREADS_MAX];

struct mmm_header_st {
    alignas(16)
    mmm_header_t    *next;
    _Atomic uint64_t create_epoch;
    _Atomic uint64_t write_epoch;
    uint64_t         retire_epoch;
    mmm_cleanup_func cleanup;
    void            *cleanup_aux; // Data needed for cleanup, usually the object
    alignas(16)
    uint8_t          data[];
};

struct mmm_free_tids_st {
    mmm_free_tids_t *next;
    uint64_t         tid;
};

typedef struct {
    uint64_t w1;
    uint64_t w2;
} hatrack_hash_t;

typedef struct {
    void    *item;
    uint64_t info;
} crown_record_t;

typedef struct {
    _Atomic hatrack_hash_t hv;
    _Atomic crown_record_t record;
    _Atomic uint64_t       neighbor_map;

} crown_bucket_t;

typedef struct crown_store_st crown_store_t;

struct crown_store_st {
    alignas(8)
    uint64_t                 last_slot;
    uint64_t                 threshold;
    _Atomic uint64_t         used_count;
    _Atomic(crown_store_t *) store_next;
    _Atomic bool             claimed;
    alignas(16)
    crown_bucket_t           buckets[];
};

typedef struct {
    alignas(8)
    _Atomic(crown_store_t *) store_current;
    _Atomic uint64_t         item_count;
    _Atomic uint64_t         help_needed;
            uint64_t         next_epoch;
} crown_t;

typedef struct {
    void *key;
    void *value;
} hatrack_dict_item_t;


typedef struct hatrack_dict_st hatrack_dict_t;

typedef void *hatrack_dict_key_t;
typedef void *hatrack_dict_value_t;

typedef hatrack_hash_t (*hatrack_hash_func_t)(void *);
typedef void (*hatrack_mem_hook_t)(void *, void *);

typedef union {
    int64_t               offset_info;
    hatrack_hash_func_t   custom_hash;
} hatrack_hash_info_t;

struct hatrack_dict_st {
    crown_t               crown_instance;
    hatrack_hash_info_t   hash_info;
    hatrack_mem_hook_t    free_handler;
    hatrack_mem_hook_t    key_return_hook;
    hatrack_mem_hook_t    val_return_hook;
    uint32_t              key_type;
    bool                  slow_views;
    bool                  sorted_views;
};
