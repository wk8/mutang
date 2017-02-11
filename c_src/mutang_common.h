#define BUILD_ATOM(name) atom_##name = enif_make_atom(env, #name);

static ERL_NIF_TERM atom_ok;
static ERL_NIF_TERM atom_error;
static ERL_NIF_TERM atom_badversion;

#define BUILD_COMMON_ATOMS \
  BUILD_ATOM(ok)           \
  BUILD_ATOM(error)        \
  BUILD_ATOM(badversion)

// in all the modules, all the NIFs take a mutang object as 1st argument, and
// its version as second; this macro unpacks both, and checks that the version
// is right
#define UNPACK_MUTANG_OBJ_AND_CHECK_VERSION(type)                              \
  mutang_##type##_t* type;                                                     \
    enif_get_resource(env, argv[0], mutang_##type##_resource, (void**)&type);

// same idea, to bump the obect's version and pack it as an erlang term
#define BUMP_AND_PACK_MUTANG_OBJ_VERSION(type)                                 \
  map->version++;                                                                             \
  ERL_NIF_TERM version_term = enif_make_uint(env, map->version);
