/* Area:	ffi_call
   Purpose:	Check return value long double.
   Limitations:	none.
   PR:		none.
   Originator:	<andreast@gcc.gnu.org> 20071113  */
<<<<<<< HEAD   (1246a0 Merge "Remove redundant NOTICE copied from LICENSE.")

/* { dg-do run { xfail x86_64-*-mingw* x86_64-*-cygwin* } } */
=======
/* { dg-do run } */

>>>>>>> BRANCH (5dcb74 Move nested_struct3 test to closures directory)
#include "ffitest.h"

static long double return_ldl(long double ldl)
{
  return 2*ldl;
}
int main (void)
{
  ffi_cif cif;
  ffi_type *args[MAX_ARGS];
  void *values[MAX_ARGS];
  long double ldl, rldl;

  args[0] = &ffi_type_longdouble;
  values[0] = &ldl;

  /* Initialize the cif */
  CHECK(ffi_prep_cif(&cif, FFI_DEFAULT_ABI, 1,
		     &ffi_type_longdouble, args) == FFI_OK);

  for (ldl = -127.0; ldl <  127.0; ldl++)
    {
      ffi_call(&cif, FFI_FN(return_ldl), &rldl, values);
      CHECK(rldl ==  2 * ldl);
    }
  exit(0);
}
