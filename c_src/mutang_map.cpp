#include <functional> // TODO wkpo needed? comment to explain?
#include <limits.h>
#include <unordered_map>

#include "erl_nif.h"

#include "mutang_common.h"

static ErlNifResourceType* mutang_map_resource;

static ERL_NIF_TERM atom_badmap;
static ERL_NIF_TERM atom_badkey;

// TODO wkpo better code structure??

using namespace std;

typedef struct mutang_map_t {
  unordered_map<int, int>* map;
  // provides a (non-fool proof, but
  // sufficient to protect against non-intentional errors) mechanism of
  // ensuring that the erlang code does not assume the immutability of
  // mutang_map references
  unsigned int version;
} mutang_map_t;

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
  UNPACK_MUTANG_OBJ_AND_CHECK_VERSION(map)

  int key, value;

  if (!enif_get_int(env, argv[2], &key) || !enif_get_int(env, argv[3], &value)) {
    return enif_make_badarg(env);
  }

  (*map->map)[key] = value;

  BUMP_AND_PACK_MUTANG_OBJ_VERSION(map)
  return enif_make_tuple2(env, atom_ok, version_term);
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
    {"nif_get", 3, mutang_map_nif_get}
  };

  ERL_NIF_INIT(mutang_map, mutang_map_nif_funcs, mutang_map_load, NULL, NULL, NULL)
} // end of extern C
