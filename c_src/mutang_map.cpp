#include <functional> // TODO wkpo needed? comment to explain?
#include <limits.h>
#include <unordered_map>

#include "erl_nif.h"

#include "mutang_common.h"

#include <time.h>

static ErlNifResourceType* mutang_map_resource;

static ERL_NIF_TERM atom_badmap;
static ERL_NIF_TERM atom_badkey;

static ErlNifUInt64 unpack_and_check_map_nsec = 0;
static ErlNifUInt64 unpack_int_nsec = 0;
static ErlNifUInt64 map_set_nsec = 0;
static ErlNifUInt64 bump_and_pack_obj_version_nsec = 0;
static ErlNifUInt64 make_tuple_nsec = 0;

// static const clockid_t wkpoid = CLOCK_PROCESS_CPUTIME_ID;
static const clockid_t wkpoid = CLOCK_MONOTONIC;
// static const clockid_t wkpoid = CLOCK_REALTIME;

void incr_nsec(const struct timespec a, const struct timespec b, ErlNifUInt64* nsec) {
  const time_t sec_diff = b.tv_sec - a.tv_sec;
  const long nsec_diff = b.tv_nsec - a.tv_nsec;
  const long diff = nsec_diff + sec_diff * 1000000000;
  (*nsec) += diff;
}

// TODO wkpo better code structure??

using namespace std;

typedef struct mutang_map_t {
  unordered_map<int, int>* map;
  // provides a (non-fool proof, but sufficient to protect against
  // non-intentional errors) mechanism of ensuring that the erlang code does not
  // assume the immutability of mutang_map references
  unsigned int version;
} mutang_map_t;

static ERL_NIF_TERM mutang_map_nif_get_nsec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  return enif_make_tuple5(env,
    enif_make_uint64(env, unpack_and_check_map_nsec),
    enif_make_uint64(env, unpack_int_nsec),
    enif_make_uint64(env, map_set_nsec),
    enif_make_uint64(env, bump_and_pack_obj_version_nsec),
    enif_make_uint64(env, make_tuple_nsec));
}

static ERL_NIF_TERM mutang_map_nif_reset_nsec(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unpack_and_check_map_nsec = 0;
  unpack_int_nsec = 0;
  map_set_nsec = 0;
  bump_and_pack_obj_version_nsec = 0;
  make_tuple_nsec = 0;

  return atom_ok;
}

// TODO wkpo take the type of terms as an optional arg!
static ERL_NIF_TERM mutang_map_nif_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  mutang_map_t* map = (mutang_map_t*) enif_alloc_resource(mutang_map_resource, sizeof(mutang_map_t));
  // TODO wkpo handle alloc failure?
  map->map = new ::unordered_map<int, int>(); // TODO wkpo enif_alloc?
  map->version = 0;

  ERL_NIF_TERM resource = enif_make_resource(env, map);
  enif_release_resource(map);

  return enif_make_tuple2(env, resource, enif_make_uint(env, map->version));
}

static ERL_NIF_TERM mutang_map_nif_put(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  struct timespec a, b;

  clock_gettime(wkpoid, &a);
  UNPACK_MUTANG_OBJ_AND_CHECK_VERSION(map)
  clock_gettime(wkpoid, &b);
  incr_nsec(a, b, &unpack_and_check_map_nsec);

  clock_gettime(wkpoid, &a);
  int key, value;
  enif_get_int(env, argv[2], &key);
  enif_get_int(env, argv[3], &value);
  clock_gettime(wkpoid, &b);
  incr_nsec(a, b, &unpack_int_nsec);

  clock_gettime(wkpoid, &a);
  (*map->map)[key] = value;
  clock_gettime(wkpoid, &b);
  incr_nsec(a, b, &map_set_nsec);

  clock_gettime(wkpoid, &a);
  BUMP_AND_PACK_MUTANG_OBJ_VERSION(map)
  clock_gettime(wkpoid, &b);
  incr_nsec(a, b, &bump_and_pack_obj_version_nsec);

  clock_gettime(wkpoid, &a);
  ERL_NIF_TERM result = enif_make_tuple2(env, atom_ok, version_term);
  clock_gettime(wkpoid, &b);
  incr_nsec(a, b, &make_tuple_nsec);
  
  return result;
}

static ERL_NIF_TERM mutang_map_nif_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  UNPACK_MUTANG_OBJ_AND_CHECK_VERSION(map)

  int key;
  unordered_map<int, int>::const_iterator iterator;

  if (!enif_get_int(env, argv[2], &key)) {
    return enif_make_badarg(env);
  }

  iterator = map->map->find(key);
  if (iterator == map->map->cend()) {
    return enif_make_tuple2(env, atom_error, atom_badkey);
  } else {
    return enif_make_tuple2(env, atom_ok, enif_make_int(env, iterator->second));
  }
}

static int mutang_map_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info)
{
  BUILD_COMMON_ATOMS
  BUILD_ATOM(badmap)
  BUILD_ATOM(badkey)

  ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  mutang_map_resource = enif_open_resource_type(env, NULL, "mutang_map",
                                                NULL, // TODO wkpo dtor!
                                                flags,
                                                NULL);
  return mutang_map_resource ? 0 : 1;
}

extern "C" {
  static ErlNifFunc mutang_map_nif_funcs[] = {
    {"nif_new", 0, mutang_map_nif_new},
    {"nif_put", 4, mutang_map_nif_put},
    // TODO wkpo next get/3
    {"nif_get", 3, mutang_map_nif_get},
    {"get_nsec", 0, mutang_map_nif_get_nsec},
    {"reset_nsec", 0, mutang_map_nif_reset_nsec}
  };

  ERL_NIF_INIT(mutang_map, mutang_map_nif_funcs, mutang_map_load, NULL, NULL, NULL)
} // end of extern C
