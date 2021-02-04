#pragma once

namespace Builtin {
  /* Buitin functions */
  auto constexpr
    GC_MALLOC                =  "__mank_alloc__any",
    MANK_STR_CONCAT_INTERNAL =  "__mank_builtin__str_concat",
    MANK_STR_CAST_INTERNAL   =  "__mank_builtin__str_cast",
    MANK_VEC_INIT            =  "__mank_builtin__init_vec",
    MANK_VEC_PUSH_BACK       =  "__mank_builtin__push_back",
    MANK_VEC_POP_BACK        =  "__mank_builtin__pop_back";

  namespace String {
    enum Offsets: uint {
      LENGTH = 0,
      DATA = 1,
    };
  }

  namespace Vector {
    enum Offsets: uint {
      TYPE_SIZE = 0,
      CAPACITY  = 1,
      LENGTH    = 2,
      DATA      = 3,
    };
  }
}
