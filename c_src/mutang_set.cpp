#include "erl_nif.h"

extern "C" {
  static ERL_NIF_TERM mutang_set_wkpo(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
  {
    return enif_make_int(env, 28);
  }

  static ErlNifFunc mutang_set_nif_funcs[] = {
    {"wkpo", 0, mutang_set_wkpo}
  };

  ERL_NIF_INIT(mutang_set, mutang_set_nif_funcs, NULL, NULL, NULL, NULL)
} // end of extern C
