/* Copyright (c) 2009, 2010, 2011, 2012 ARM Ltd.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
``Software''), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.  */

<<<<<<< HEAD   (1246a0 Merge "Remove redundant NOTICE copied from LICENSE.")
#include <stdio.h>

#include <ffi.h>
#include <ffi_common.h>

#include <stdlib.h>

/* Stack alignment requirement in bytes */
#if defined (__APPLE__)
#define AARCH64_STACK_ALIGN 1
#else
#define AARCH64_STACK_ALIGN 16
#endif

#define N_X_ARG_REG 8
#define N_V_ARG_REG 8

#define AARCH64_FFI_WITH_V (1 << AARCH64_FFI_WITH_V_BIT)

union _d
{
  UINT64 d;
  UINT32 s[2];
};

struct call_context
{
  UINT64 x [AARCH64_N_XREG];
  struct
  {
    union _d d[2];
  } v [AARCH64_N_VREG];
};

#if defined (__clang__) && defined (__APPLE__)
extern void
sys_icache_invalidate (void *start, size_t len);
#endif

static inline void
ffi_clear_cache (void *start, void *end)
{
#if defined (__clang__) && defined (__APPLE__)
	sys_icache_invalidate (start, (char *)end - (char *)start);
#elif defined (__GNUC__)
	__builtin___clear_cache (start, end);
#else
#error "Missing builtin to flush instruction cache"
#endif
}

static void *
get_x_addr (struct call_context *context, unsigned n)
{
  return &context->x[n];
}

static void *
get_s_addr (struct call_context *context, unsigned n)
{
#if defined __AARCH64EB__
  return &context->v[n].d[1].s[1];
#else
  return &context->v[n].d[0].s[0];
#endif
}

static void *
get_d_addr (struct call_context *context, unsigned n)
{
#if defined __AARCH64EB__
  return &context->v[n].d[1];
#else
  return &context->v[n].d[0];
#endif
}

static void *
get_v_addr (struct call_context *context, unsigned n)
{
  return &context->v[n];
}

/* Return the memory location at which a basic type would reside
   were it to have been stored in register n.  */

static void *
get_basic_type_addr (unsigned short type, struct call_context *context,
		     unsigned n)
{
  switch (type)
    {
    case FFI_TYPE_FLOAT:
      return get_s_addr (context, n);
    case FFI_TYPE_DOUBLE:
      return get_d_addr (context, n);
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
      return get_v_addr (context, n);
#endif
    case FFI_TYPE_UINT8:
    case FFI_TYPE_SINT8:
    case FFI_TYPE_UINT16:
    case FFI_TYPE_SINT16:
    case FFI_TYPE_UINT32:
    case FFI_TYPE_SINT32:
    case FFI_TYPE_INT:
    case FFI_TYPE_POINTER:
    case FFI_TYPE_UINT64:
    case FFI_TYPE_SINT64:
      return get_x_addr (context, n);
    case FFI_TYPE_VOID:
      return NULL;
    default:
      FFI_ASSERT (0);
      return NULL;
    }
}

/* Return the alignment width for each of the basic types.  */

static size_t
get_basic_type_alignment (unsigned short type)
{
  switch (type)
    {
    case FFI_TYPE_FLOAT:
#if defined (__APPLE__)
      return sizeof (UINT32);
#endif
    case FFI_TYPE_DOUBLE:
      return sizeof (UINT64);
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
      return sizeof (long double);
#endif
    case FFI_TYPE_UINT8:
    case FFI_TYPE_SINT8:
#if defined (__APPLE__)
	  return sizeof (UINT8);
#endif
    case FFI_TYPE_UINT16:
    case FFI_TYPE_SINT16:
#if defined (__APPLE__)
	  return sizeof (UINT16);
#endif
    case FFI_TYPE_UINT32:
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT32:
#if defined (__APPLE__)
	  return sizeof (UINT32);
#endif
    case FFI_TYPE_POINTER:
    case FFI_TYPE_UINT64:
    case FFI_TYPE_SINT64:
      return sizeof (UINT64);

    default:
      FFI_ASSERT (0);
      return 0;
    }
}

/* Return the size in bytes for each of the basic types.  */

static size_t
get_basic_type_size (unsigned short type)
{
  switch (type)
    {
    case FFI_TYPE_FLOAT:
      return sizeof (UINT32);
    case FFI_TYPE_DOUBLE:
      return sizeof (UINT64);
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
      return sizeof (long double);
#endif
    case FFI_TYPE_UINT8:
      return sizeof (UINT8);
    case FFI_TYPE_SINT8:
      return sizeof (SINT8);
    case FFI_TYPE_UINT16:
      return sizeof (UINT16);
    case FFI_TYPE_SINT16:
      return sizeof (SINT16);
    case FFI_TYPE_UINT32:
      return sizeof (UINT32);
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT32:
      return sizeof (SINT32);
    case FFI_TYPE_POINTER:
    case FFI_TYPE_UINT64:
      return sizeof (UINT64);
    case FFI_TYPE_SINT64:
      return sizeof (SINT64);

    default:
      FFI_ASSERT (0);
      return 0;
    }
}

extern void
ffi_call_SYSV (unsigned (*)(struct call_context *context, unsigned char *,
			    extended_cif *),
               struct call_context *context,
               extended_cif *,
               size_t,
               void (*fn)(void));

extern void
ffi_closure_SYSV (ffi_closure *);

/* Test for an FFI floating point representation.  */

static unsigned
is_floating_type (unsigned short type)
{
  return (type == FFI_TYPE_FLOAT || type == FFI_TYPE_DOUBLE
	  || type == FFI_TYPE_LONGDOUBLE);
}

/* Test for a homogeneous structure.  */

static unsigned short
get_homogeneous_type (ffi_type *ty)
{
  if (ty->type == FFI_TYPE_STRUCT && ty->elements)
    {
      unsigned i;
      unsigned short candidate_type
	= get_homogeneous_type (ty->elements[0]);
      for (i =1; ty->elements[i]; i++)
	{
	  unsigned short iteration_type = 0;
	  /* If we have a nested struct, we must find its homogeneous type.
	     If that fits with our candidate type, we are still
	     homogeneous.  */
	  if (ty->elements[i]->type == FFI_TYPE_STRUCT
	      && ty->elements[i]->elements)
	    {
	      iteration_type = get_homogeneous_type (ty->elements[i]);
	    }
	  else
	    {
	      iteration_type = ty->elements[i]->type;
	    }

	  /* If we are not homogeneous, return FFI_TYPE_STRUCT.  */
	  if (candidate_type != iteration_type)
	    return FFI_TYPE_STRUCT;
	}
      return candidate_type;
    }

  /* Base case, we have no more levels of nesting, so we
     are a basic type, and so, trivially homogeneous in that type.  */
  return ty->type;
}

/* Determine the number of elements within a STRUCT.

   Note, we must handle nested structs.

   If ty is not a STRUCT this function will return 0.  */

static unsigned
element_count (ffi_type *ty)
{
  if (ty->type == FFI_TYPE_STRUCT && ty->elements)
    {
      unsigned n;
      unsigned elems = 0;
      for (n = 0; ty->elements[n]; n++)
	{
	  if (ty->elements[n]->type == FFI_TYPE_STRUCT
	      && ty->elements[n]->elements)
	    elems += element_count (ty->elements[n]);
	  else
	    elems++;
	}
      return elems;
    }
  return 0;
}

/* Test for a homogeneous floating point aggregate.

   A homogeneous floating point aggregate is a homogeneous aggregate of
   a half- single- or double- precision floating point type with one
   to four elements.  Note that this includes nested structs of the
   basic type.  */

static int
is_hfa (ffi_type *ty)
{
  if (ty->type == FFI_TYPE_STRUCT
      && ty->elements[0]
      && is_floating_type (get_homogeneous_type (ty)))
    {
      unsigned n = element_count (ty);
      return n >= 1 && n <= 4;
    }
  return 0;
}

/* Test if an ffi_type is a candidate for passing in a register.

   This test does not check that sufficient registers of the
   appropriate class are actually available, merely that IFF
   sufficient registers are available then the argument will be passed
   in register(s).

   Note that an ffi_type that is deemed to be a register candidate
   will always be returned in registers.

   Returns 1 if a register candidate else 0.  */

static int
is_register_candidate (ffi_type *ty)
{
  switch (ty->type)
    {
    case FFI_TYPE_VOID:
    case FFI_TYPE_FLOAT:
    case FFI_TYPE_DOUBLE:
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
#endif
    case FFI_TYPE_UINT8:
    case FFI_TYPE_UINT16:
    case FFI_TYPE_UINT32:
    case FFI_TYPE_UINT64:
    case FFI_TYPE_POINTER:
    case FFI_TYPE_SINT8:
    case FFI_TYPE_SINT16:
    case FFI_TYPE_SINT32:
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT64:
      return 1;

    case FFI_TYPE_STRUCT:
      if (is_hfa (ty))
        {
          return 1;
        }
      else if (ty->size > 16)
        {
          /* Too large. Will be replaced with a pointer to memory. The
             pointer MAY be passed in a register, but the value will
             not. This test specifically fails since the argument will
             never be passed by value in registers. */
          return 0;
        }
      else
        {
          /* Might be passed in registers depending on the number of
             registers required. */
          return (ty->size + 7) / 8 < N_X_ARG_REG;
        }
      break;

    default:
      FFI_ASSERT (0);
      break;
    }

  return 0;
}

/* Test if an ffi_type argument or result is a candidate for a vector
   register.  */

static int
is_v_register_candidate (ffi_type *ty)
{
  return is_floating_type (ty->type)
	   || (ty->type == FFI_TYPE_STRUCT && is_hfa (ty));
}

/* Representation of the procedure call argument marshalling
   state.

   The terse state variable names match the names used in the AARCH64
   PCS. */

struct arg_state
{
  unsigned ngrn;                /* Next general-purpose register number. */
  unsigned nsrn;                /* Next vector register number. */
  size_t nsaa;                  /* Next stack offset. */

#if defined (__APPLE__)
  unsigned allocating_variadic;
#endif
};

/* Initialize a procedure call argument marshalling state.  */
static void
arg_init (struct arg_state *state, size_t call_frame_size)
{
  state->ngrn = 0;
  state->nsrn = 0;
  state->nsaa = 0;

#if defined (__APPLE__)
  state->allocating_variadic = 0;
#endif
}

/* Return the number of available consecutive core argument
   registers.  */

static unsigned
available_x (struct arg_state *state)
{
  return N_X_ARG_REG - state->ngrn;
}

/* Return the number of available consecutive vector argument
   registers.  */

static unsigned
available_v (struct arg_state *state)
{
  return N_V_ARG_REG - state->nsrn;
}

static void *
allocate_to_x (struct call_context *context, struct arg_state *state)
{
  FFI_ASSERT (state->ngrn < N_X_ARG_REG);
  return get_x_addr (context, (state->ngrn)++);
}

static void *
allocate_to_s (struct call_context *context, struct arg_state *state)
{
  FFI_ASSERT (state->nsrn < N_V_ARG_REG);
  return get_s_addr (context, (state->nsrn)++);
}

static void *
allocate_to_d (struct call_context *context, struct arg_state *state)
{
  FFI_ASSERT (state->nsrn < N_V_ARG_REG);
  return get_d_addr (context, (state->nsrn)++);
}

static void *
allocate_to_v (struct call_context *context, struct arg_state *state)
{
  FFI_ASSERT (state->nsrn < N_V_ARG_REG);
  return get_v_addr (context, (state->nsrn)++);
}

/* Allocate an aligned slot on the stack and return a pointer to it.  */
static void *
allocate_to_stack (struct arg_state *state, void *stack, size_t alignment,
		   size_t size)
{
  void *allocation;

  /* Round up the NSAA to the larger of 8 or the natural
     alignment of the argument's type.  */
  state->nsaa = ALIGN (state->nsaa, alignment);
  state->nsaa = ALIGN (state->nsaa, alignment);
#if defined (__APPLE__)
  if (state->allocating_variadic)
    state->nsaa = ALIGN (state->nsaa, 8);
#else
  state->nsaa = ALIGN (state->nsaa, 8);
#endif

  allocation = stack + state->nsaa;

  state->nsaa += size;
  return allocation;
}

static void
copy_basic_type (void *dest, void *source, unsigned short type)
{
  /* This is necessary to ensure that basic types are copied
     sign extended to 64-bits as libffi expects.  */
  switch (type)
    {
    case FFI_TYPE_FLOAT:
      *(float *) dest = *(float *) source;
      break;
    case FFI_TYPE_DOUBLE:
      *(double *) dest = *(double *) source;
      break;
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
      *(long double *) dest = *(long double *) source;
      break;
#endif
    case FFI_TYPE_UINT8:
      *(ffi_arg *) dest = *(UINT8 *) source;
      break;
    case FFI_TYPE_SINT8:
      *(ffi_sarg *) dest = *(SINT8 *) source;
      break;
    case FFI_TYPE_UINT16:
      *(ffi_arg *) dest = *(UINT16 *) source;
      break;
    case FFI_TYPE_SINT16:
      *(ffi_sarg *) dest = *(SINT16 *) source;
      break;
    case FFI_TYPE_UINT32:
      *(ffi_arg *) dest = *(UINT32 *) source;
      break;
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT32:
      *(ffi_sarg *) dest = *(SINT32 *) source;
      break;
    case FFI_TYPE_POINTER:
    case FFI_TYPE_UINT64:
      *(ffi_arg *) dest = *(UINT64 *) source;
      break;
    case FFI_TYPE_SINT64:
      *(ffi_sarg *) dest = *(SINT64 *) source;
      break;
    case FFI_TYPE_VOID:
      break;

    default:
      FFI_ASSERT (0);
    }
}

static void
copy_hfa_to_reg_or_stack (void *memory,
			  ffi_type *ty,
			  struct call_context *context,
			  unsigned char *stack,
			  struct arg_state *state)
{
  unsigned elems = element_count (ty);
  if (available_v (state) < elems)
    {
      /* There are insufficient V registers. Further V register allocations
	 are prevented, the NSAA is adjusted (by allocate_to_stack ())
	 and the argument is copied to memory at the adjusted NSAA.  */
      state->nsrn = N_V_ARG_REG;
      memcpy (allocate_to_stack (state, stack, ty->alignment, ty->size),
	      memory,
	      ty->size);
    }
  else
    {
      int i;
      unsigned short type = get_homogeneous_type (ty);
      for (i = 0; i < elems; i++)
	{
	  void *reg = allocate_to_v (context, state);
	  copy_basic_type (reg, memory, type);
	  memory += get_basic_type_size (type);
	}
    }
}

/* Either allocate an appropriate register for the argument type, or if
   none are available, allocate a stack slot and return a pointer
   to the allocated space.  */

static void *
allocate_to_register_or_stack (struct call_context *context,
			       unsigned char *stack,
			       struct arg_state *state,
			       unsigned short type)
{
  size_t alignment = get_basic_type_alignment (type);
  size_t size = alignment;
  switch (type)
    {
    case FFI_TYPE_FLOAT:
      /* This is the only case for which the allocated stack size
	 should not match the alignment of the type.  */
      size = sizeof (UINT32);
      /* Fall through.  */
    case FFI_TYPE_DOUBLE:
      if (state->nsrn < N_V_ARG_REG)
	return allocate_to_d (context, state);
      state->nsrn = N_V_ARG_REG;
      break;
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
      if (state->nsrn < N_V_ARG_REG)
	return allocate_to_v (context, state);
      state->nsrn = N_V_ARG_REG;
      break;
#endif
    case FFI_TYPE_UINT8:
    case FFI_TYPE_SINT8:
    case FFI_TYPE_UINT16:
    case FFI_TYPE_SINT16:
    case FFI_TYPE_UINT32:
    case FFI_TYPE_SINT32:
    case FFI_TYPE_INT:
    case FFI_TYPE_POINTER:
    case FFI_TYPE_UINT64:
    case FFI_TYPE_SINT64:
      if (state->ngrn < N_X_ARG_REG)
	return allocate_to_x (context, state);
      state->ngrn = N_X_ARG_REG;
      break;
    default:
      FFI_ASSERT (0);
    }

    return allocate_to_stack (state, stack, alignment, size);
}

/* Copy a value to an appropriate register, or if none are
   available, to the stack.  */

static void
copy_to_register_or_stack (struct call_context *context,
			   unsigned char *stack,
			   struct arg_state *state,
			   void *value,
			   unsigned short type)
{
  copy_basic_type (
	  allocate_to_register_or_stack (context, stack, state, type),
	  value,
	  type);
}

/* Marshall the arguments from FFI representation to procedure call
   context and stack.  */

static unsigned
aarch64_prep_args (struct call_context *context, unsigned char *stack,
		   extended_cif *ecif)
{
  int i;
  struct arg_state state;

  arg_init (&state, ALIGN(ecif->cif->bytes, 16));

  for (i = 0; i < ecif->cif->nargs; i++)
    {
      ffi_type *ty = ecif->cif->arg_types[i];
      switch (ty->type)
	{
	case FFI_TYPE_VOID:
	  FFI_ASSERT (0);
	  break;

	/* If the argument is a basic type the argument is allocated to an
	   appropriate register, or if none are available, to the stack.  */
	case FFI_TYPE_FLOAT:
	case FFI_TYPE_DOUBLE:
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
	case FFI_TYPE_LONGDOUBLE:
#endif
	case FFI_TYPE_UINT8:
	case FFI_TYPE_SINT8:
	case FFI_TYPE_UINT16:
	case FFI_TYPE_SINT16:
	case FFI_TYPE_UINT32:
	case FFI_TYPE_INT:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_POINTER:
	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
	  copy_to_register_or_stack (context, stack, &state,
				     ecif->avalue[i], ty->type);
	  break;

	case FFI_TYPE_STRUCT:
	  if (is_hfa (ty))
	    {
	      copy_hfa_to_reg_or_stack (ecif->avalue[i], ty, context,
					stack, &state);
	    }
	  else if (ty->size > 16)
	    {
	      /* If the argument is a composite type that is larger than 16
		 bytes, then the argument has been copied to memory, and
		 the argument is replaced by a pointer to the copy.  */

	      copy_to_register_or_stack (context, stack, &state,
					 &(ecif->avalue[i]), FFI_TYPE_POINTER);
	    }
	  else if (available_x (&state) >= (ty->size + 7) / 8)
	    {
	      /* If the argument is a composite type and the size in
		 double-words is not more than the number of available
		 X registers, then the argument is copied into consecutive
		 X registers.  */
	      int j;
	      for (j = 0; j < (ty->size + 7) / 8; j++)
		{
		  memcpy (allocate_to_x (context, &state),
			  &(((UINT64 *) ecif->avalue[i])[j]),
			  sizeof (UINT64));
		}
	    }
	  else
	    {
	      /* Otherwise, there are insufficient X registers. Further X
		 register allocations are prevented, the NSAA is adjusted
		 (by allocate_to_stack ()) and the argument is copied to
		 memory at the adjusted NSAA.  */
	      state.ngrn = N_X_ARG_REG;

	      memcpy (allocate_to_stack (&state, stack, ty->alignment,
					 ty->size), ecif->avalue + i, ty->size);
	    }
	  break;

	default:
	  FFI_ASSERT (0);
	  break;
	}

#if defined (__APPLE__)
      if (i + 1 == ecif->cif->aarch64_nfixedargs)
	{
	  state.ngrn = N_X_ARG_REG;
	  state.nsrn = N_V_ARG_REG;

	  state.allocating_variadic = 1;
	}
#endif
    }

  return ecif->cif->aarch64_flags;
}

ffi_status
ffi_prep_cif_machdep (ffi_cif *cif)
{
  /* Round the stack up to a multiple of the stack alignment requirement. */
  cif->bytes =
    (cif->bytes + (AARCH64_STACK_ALIGN - 1)) & ~ (AARCH64_STACK_ALIGN - 1);

  /* Initialize our flags. We are interested if this CIF will touch a
     vector register, if so we will enable context save and load to
     those registers, otherwise not. This is intended to be friendly
     to lazy float context switching in the kernel.  */
  cif->aarch64_flags = 0;

  if (is_v_register_candidate (cif->rtype))
    {
      cif->aarch64_flags |= AARCH64_FFI_WITH_V;
    }
  else
    {
      int i;
      for (i = 0; i < cif->nargs; i++)
        if (is_v_register_candidate (cif->arg_types[i]))
          {
            cif->aarch64_flags |= AARCH64_FFI_WITH_V;
            break;
          }
    }

#if defined (__APPLE__)
  cif->aarch64_nfixedargs = 0;
#endif

  return FFI_OK;
}

#if defined (__APPLE__)

/* Perform Apple-specific cif processing for variadic calls */
ffi_status ffi_prep_cif_machdep_var(ffi_cif *cif,
				    unsigned int nfixedargs,
				    unsigned int ntotalargs)
{
  ffi_status status;

  status = ffi_prep_cif_machdep (cif);

  cif->aarch64_nfixedargs = nfixedargs;

  return status;
}

#endif

/* Call a function with the provided arguments and capture the return
   value.  */
void
ffi_call (ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue)
{
  extended_cif ecif;

  ecif.cif = cif;
  ecif.avalue = avalue;
  ecif.rvalue = rvalue;

  switch (cif->abi)
    {
    case FFI_SYSV:
      {
        struct call_context context;
	size_t stack_bytes;

	/* Figure out the total amount of stack space we need, the
	   above call frame space needs to be 16 bytes aligned to
	   ensure correct alignment of the first object inserted in
	   that space hence the ALIGN applied to cif->bytes.*/
	stack_bytes = ALIGN(cif->bytes, 16);

	memset (&context, 0, sizeof (context));
        if (is_register_candidate (cif->rtype))
          {
            ffi_call_SYSV (aarch64_prep_args, &context, &ecif, stack_bytes, fn);
            switch (cif->rtype->type)
              {
              case FFI_TYPE_VOID:
              case FFI_TYPE_FLOAT:
              case FFI_TYPE_DOUBLE:
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
              case FFI_TYPE_LONGDOUBLE:
#endif
              case FFI_TYPE_UINT8:
              case FFI_TYPE_SINT8:
              case FFI_TYPE_UINT16:
              case FFI_TYPE_SINT16:
              case FFI_TYPE_UINT32:
              case FFI_TYPE_SINT32:
              case FFI_TYPE_POINTER:
              case FFI_TYPE_UINT64:
              case FFI_TYPE_INT:
              case FFI_TYPE_SINT64:
		{
		  void *addr = get_basic_type_addr (cif->rtype->type,
						    &context, 0);
		  copy_basic_type (rvalue, addr, cif->rtype->type);
		  break;
		}

              case FFI_TYPE_STRUCT:
                if (is_hfa (cif->rtype))
		  {
		    int j;
		    unsigned short type = get_homogeneous_type (cif->rtype);
		    unsigned elems = element_count (cif->rtype);
		    for (j = 0; j < elems; j++)
		      {
			void *reg = get_basic_type_addr (type, &context, j);
			copy_basic_type (rvalue, reg, type);
			rvalue += get_basic_type_size (type);
		      }
		  }
                else if ((cif->rtype->size + 7) / 8 < N_X_ARG_REG)
                  {
                    size_t size = ALIGN (cif->rtype->size, sizeof (UINT64));
                    memcpy (rvalue, get_x_addr (&context, 0), size);
                  }
                else
                  {
                    FFI_ASSERT (0);
                  }
                break;

              default:
                FFI_ASSERT (0);
                break;
              }
          }
        else
          {
            memcpy (get_x_addr (&context, 8), &rvalue, sizeof (UINT64));
            ffi_call_SYSV (aarch64_prep_args, &context, &ecif,
			   stack_bytes, fn);
          }
        break;
      }

    default:
      FFI_ASSERT (0);
      break;
    }
}

static unsigned char trampoline [] =
{ 0x70, 0x00, 0x00, 0x58,	/* ldr	x16, 1f	*/
  0x91, 0x00, 0x00, 0x10,	/* adr	x17, 2f	*/
  0x00, 0x02, 0x1f, 0xd6	/* br	x16	*/
};

/* Build a trampoline.  */

#define FFI_INIT_TRAMPOLINE(TRAMP,FUN,CTX,FLAGS)			\
  ({unsigned char *__tramp = (unsigned char*)(TRAMP);			\
    UINT64  __fun = (UINT64)(FUN);					\
    UINT64  __ctx = (UINT64)(CTX);					\
    UINT64  __flags = (UINT64)(FLAGS);					\
    memcpy (__tramp, trampoline, sizeof (trampoline));			\
    memcpy (__tramp + 12, &__fun, sizeof (__fun));			\
    memcpy (__tramp + 20, &__ctx, sizeof (__ctx));			\
    memcpy (__tramp + 28, &__flags, sizeof (__flags));			\
    ffi_clear_cache(__tramp, __tramp + FFI_TRAMPOLINE_SIZE);		\
  })

ffi_status
ffi_prep_closure_loc (ffi_closure* closure,
                      ffi_cif* cif,
                      void (*fun)(ffi_cif*,void*,void**,void*),
                      void *user_data,
                      void *codeloc)
{
  if (cif->abi != FFI_SYSV)
    return FFI_BAD_ABI;

  FFI_INIT_TRAMPOLINE (&closure->tramp[0], &ffi_closure_SYSV, codeloc,
		       cif->aarch64_flags);

  closure->cif  = cif;
  closure->user_data = user_data;
  closure->fun  = fun;

  return FFI_OK;
}

/* Primary handler to setup and invoke a function within a closure.

   A closure when invoked enters via the assembler wrapper
   ffi_closure_SYSV(). The wrapper allocates a call context on the
   stack, saves the interesting registers (from the perspective of
   the calling convention) into the context then passes control to
   ffi_closure_SYSV_inner() passing the saved context and a pointer to
   the stack at the point ffi_closure_SYSV() was invoked.

   On the return path the assembler wrapper will reload call context
   registers.

   ffi_closure_SYSV_inner() marshalls the call context into ffi value
   descriptors, invokes the wrapped function, then marshalls the return
   value back into the call context.  */

void FFI_HIDDEN
ffi_closure_SYSV_inner (ffi_closure *closure, struct call_context *context,
			void *stack)
{
  ffi_cif *cif = closure->cif;
  void **avalue = (void**) alloca (cif->nargs * sizeof (void*));
  void *rvalue = NULL;
  int i;
  struct arg_state state;

  arg_init (&state, ALIGN(cif->bytes, 16));

  for (i = 0; i < cif->nargs; i++)
    {
      ffi_type *ty = cif->arg_types[i];

      switch (ty->type)
	{
	case FFI_TYPE_VOID:
	  FFI_ASSERT (0);
	  break;

	case FFI_TYPE_UINT8:
	case FFI_TYPE_SINT8:
	case FFI_TYPE_UINT16:
	case FFI_TYPE_SINT16:
	case FFI_TYPE_UINT32:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_INT:
	case FFI_TYPE_POINTER:
	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
	case  FFI_TYPE_FLOAT:
	case  FFI_TYPE_DOUBLE:
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
	case  FFI_TYPE_LONGDOUBLE:
	  avalue[i] = allocate_to_register_or_stack (context, stack,
						     &state, ty->type);
	  break;
#endif

	case FFI_TYPE_STRUCT:
	  if (is_hfa (ty))
	    {
	      unsigned n = element_count (ty);
	      if (available_v (&state) < n)
		{
		  state.nsrn = N_V_ARG_REG;
		  avalue[i] = allocate_to_stack (&state, stack, ty->alignment,
						 ty->size);
		}
	      else
		{
		  switch (get_homogeneous_type (ty))
		    {
		    case FFI_TYPE_FLOAT:
		      {
			/* Eeek! We need a pointer to the structure,
			   however the homogeneous float elements are
			   being passed in individual S registers,
			   therefore the structure is not represented as
			   a contiguous sequence of bytes in our saved
			   register context. We need to fake up a copy
			   of the structure laid out in memory
			   correctly. The fake can be tossed once the
			   closure function has returned hence alloca()
			   is sufficient. */
			int j;
			UINT32 *p = avalue[i] = alloca (ty->size);
			for (j = 0; j < element_count (ty); j++)
			  memcpy (&p[j],
				  allocate_to_s (context, &state),
				  sizeof (*p));
			break;
		      }

		    case FFI_TYPE_DOUBLE:
		      {
			/* Eeek! We need a pointer to the structure,
			   however the homogeneous float elements are
			   being passed in individual S registers,
			   therefore the structure is not represented as
			   a contiguous sequence of bytes in our saved
			   register context. We need to fake up a copy
			   of the structure laid out in memory
			   correctly. The fake can be tossed once the
			   closure function has returned hence alloca()
			   is sufficient. */
			int j;
			UINT64 *p = avalue[i] = alloca (ty->size);
			for (j = 0; j < element_count (ty); j++)
			  memcpy (&p[j],
				  allocate_to_d (context, &state),
				  sizeof (*p));
			break;
		      }

#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
		    case FFI_TYPE_LONGDOUBLE:
			  memcpy (&avalue[i],
				  allocate_to_v (context, &state),
				  sizeof (*avalue));
		      break;
#endif

		    default:
		      FFI_ASSERT (0);
		      break;
		    }
		}
	    }
	  else if (ty->size > 16)
	    {
	      /* Replace Composite type of size greater than 16 with a
		 pointer.  */
	      memcpy (&avalue[i],
		      allocate_to_register_or_stack (context, stack,
						     &state, FFI_TYPE_POINTER),
		      sizeof (avalue[i]));
	    }
	  else if (available_x (&state) >= (ty->size + 7) / 8)
	    {
	      avalue[i] = get_x_addr (context, state.ngrn);
	      state.ngrn += (ty->size + 7) / 8;
	    }
	  else
	    {
	      state.ngrn = N_X_ARG_REG;

	      avalue[i] = allocate_to_stack (&state, stack, ty->alignment,
					     ty->size);
	    }
	  break;

	default:
	  FFI_ASSERT (0);
	  break;
	}
    }

  /* Figure out where the return value will be passed, either in
     registers or in a memory block allocated by the caller and passed
     in x8.  */

  if (is_register_candidate (cif->rtype))
    {
      /* Register candidates are *always* returned in registers. */

      /* Allocate a scratchpad for the return value, we will let the
         callee scrible the result into the scratch pad then move the
         contents into the appropriate return value location for the
         call convention.  */
      rvalue = alloca (cif->rtype->size);
      (closure->fun) (cif, rvalue, avalue, closure->user_data);

      /* Copy the return value into the call context so that it is returned
         as expected to our caller.  */
      switch (cif->rtype->type)
        {
        case FFI_TYPE_VOID:
          break;

        case FFI_TYPE_UINT8:
        case FFI_TYPE_UINT16:
        case FFI_TYPE_UINT32:
        case FFI_TYPE_POINTER:
        case FFI_TYPE_UINT64:
        case FFI_TYPE_SINT8:
        case FFI_TYPE_SINT16:
        case FFI_TYPE_INT:
        case FFI_TYPE_SINT32:
        case FFI_TYPE_SINT64:
        case FFI_TYPE_FLOAT:
        case FFI_TYPE_DOUBLE:
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
        case FFI_TYPE_LONGDOUBLE:
#endif
	  {
	    void *addr = get_basic_type_addr (cif->rtype->type, context, 0);
	    copy_basic_type (addr, rvalue, cif->rtype->type);
            break;
	  }
        case FFI_TYPE_STRUCT:
          if (is_hfa (cif->rtype))
	    {
	      int j;
	      unsigned short type = get_homogeneous_type (cif->rtype);
	      unsigned elems = element_count (cif->rtype);
	      for (j = 0; j < elems; j++)
		{
		  void *reg = get_basic_type_addr (type, context, j);
		  copy_basic_type (reg, rvalue, type);
		  rvalue += get_basic_type_size (type);
		}
	    }
          else if ((cif->rtype->size + 7) / 8 < N_X_ARG_REG)
            {
              size_t size = ALIGN (cif->rtype->size, sizeof (UINT64)) ;
              memcpy (get_x_addr (context, 0), rvalue, size);
            }
          else
            {
              FFI_ASSERT (0);
            }
          break;
        default:
          FFI_ASSERT (0);
          break;
        }
    }
  else
    {
      memcpy (&rvalue, get_x_addr (context, 8), sizeof (UINT64));
      (closure->fun) (cif, rvalue, avalue, closure->user_data);
    }
}

=======
#if defined(__aarch64__) || defined(__arm64__)|| defined (_M_ARM64)
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <fficonfig.h>
#include <ffi.h>
#include <ffi_common.h>
#include "internal.h"
#ifdef _M_ARM64
#include <windows.h> /* FlushInstructionCache */
#endif

/* Force FFI_TYPE_LONGDOUBLE to be different than FFI_TYPE_DOUBLE;
   all further uses in this file will refer to the 128-bit type.  */
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
# if FFI_TYPE_LONGDOUBLE != 4
#  error FFI_TYPE_LONGDOUBLE out of date
# endif
#else
# undef FFI_TYPE_LONGDOUBLE
# define FFI_TYPE_LONGDOUBLE 4
#endif

union _d
{
  UINT64 d;
  UINT32 s[2];
};

struct _v
{
  union _d d[2] __attribute__((aligned(16)));
};

struct call_context
{
  struct _v v[N_V_ARG_REG];
  UINT64 x[N_X_ARG_REG];
};

#if FFI_EXEC_TRAMPOLINE_TABLE

#ifdef __MACH__
#include <mach/vm_param.h>
#endif

#else

#if defined (__clang__) && defined (__APPLE__)
extern void sys_icache_invalidate (void *start, size_t len);
#endif

static inline void
ffi_clear_cache (void *start, void *end)
{
#if defined (__clang__) && defined (__APPLE__)
  sys_icache_invalidate (start, (char *)end - (char *)start);
#elif defined (__GNUC__)
  __builtin___clear_cache (start, end);
#elif defined (_M_ARM64)
  FlushInstructionCache(GetCurrentProcess(), start, (char*)end - (char*)start);
#else
#error "Missing builtin to flush instruction cache"
#endif
}

#endif

/* A subroutine of is_vfp_type.  Given a structure type, return the type code
   of the first non-structure element.  Recurse for structure elements.
   Return -1 if the structure is in fact empty, i.e. no nested elements.  */

static int
is_hfa0 (const ffi_type *ty)
{
  ffi_type **elements = ty->elements;
  int i, ret = -1;

  if (elements != NULL)
    for (i = 0; elements[i]; ++i)
      {
        ret = elements[i]->type;
        if (ret == FFI_TYPE_STRUCT || ret == FFI_TYPE_COMPLEX)
          {
            ret = is_hfa0 (elements[i]);
            if (ret < 0)
              continue;
          }
        break;
      }

  return ret;
}

/* A subroutine of is_vfp_type.  Given a structure type, return true if all
   of the non-structure elements are the same as CANDIDATE.  */

static int
is_hfa1 (const ffi_type *ty, int candidate)
{
  ffi_type **elements = ty->elements;
  int i;

  if (elements != NULL)
    for (i = 0; elements[i]; ++i)
      {
        int t = elements[i]->type;
        if (t == FFI_TYPE_STRUCT || t == FFI_TYPE_COMPLEX)
          {
            if (!is_hfa1 (elements[i], candidate))
              return 0;
          }
        else if (t != candidate)
          return 0;
      }

  return 1;
}

/* Determine if TY may be allocated to the FP registers.  This is both an
   fp scalar type as well as an homogenous floating point aggregate (HFA).
   That is, a structure consisting of 1 to 4 members of all the same type,
   where that type is an fp scalar.

   Returns non-zero iff TY is an HFA.  The result is the AARCH64_RET_*
   constant for the type.  */

static int
is_vfp_type (const ffi_type *ty)
{
  ffi_type **elements;
  int candidate, i;
  size_t size, ele_count;

  /* Quickest tests first.  */
  candidate = ty->type;
  switch (candidate)
    {
    default:
      return 0;
    case FFI_TYPE_FLOAT:
    case FFI_TYPE_DOUBLE:
    case FFI_TYPE_LONGDOUBLE:
      ele_count = 1;
      goto done;
    case FFI_TYPE_COMPLEX:
      candidate = ty->elements[0]->type;
      switch (candidate)
	{
	case FFI_TYPE_FLOAT:
	case FFI_TYPE_DOUBLE:
	case FFI_TYPE_LONGDOUBLE:
	  ele_count = 2;
	  goto done;
	}
      return 0;
    case FFI_TYPE_STRUCT:
      break;
    }

  /* No HFA types are smaller than 4 bytes, or larger than 64 bytes.  */
  size = ty->size;
  if (size < 4 || size > 64)
    return 0;

  /* Find the type of the first non-structure member.  */
  elements = ty->elements;
  candidate = elements[0]->type;
  if (candidate == FFI_TYPE_STRUCT || candidate == FFI_TYPE_COMPLEX)
    {
      for (i = 0; ; ++i)
        {
          candidate = is_hfa0 (elements[i]);
          if (candidate >= 0)
            break;
        }
    }

  /* If the first member is not a floating point type, it's not an HFA.
     Also quickly re-check the size of the structure.  */
  switch (candidate)
    {
    case FFI_TYPE_FLOAT:
      ele_count = size / sizeof(float);
      if (size != ele_count * sizeof(float))
        return 0;
      break;
    case FFI_TYPE_DOUBLE:
      ele_count = size / sizeof(double);
      if (size != ele_count * sizeof(double))
        return 0;
      break;
    case FFI_TYPE_LONGDOUBLE:
      ele_count = size / sizeof(long double);
      if (size != ele_count * sizeof(long double))
        return 0;
      break;
    default:
      return 0;
    }
  if (ele_count > 4)
    return 0;

  /* Finally, make sure that all scalar elements are the same type.  */
  for (i = 0; elements[i]; ++i)
    {
      int t = elements[i]->type;
      if (t == FFI_TYPE_STRUCT || t == FFI_TYPE_COMPLEX)
        {
          if (!is_hfa1 (elements[i], candidate))
            return 0;
        }
      else if (t != candidate)
        return 0;
    }

  /* All tests succeeded.  Encode the result.  */
 done:
  return candidate * 4 + (4 - (int)ele_count);
}

/* Representation of the procedure call argument marshalling
   state.

   The terse state variable names match the names used in the AARCH64
   PCS. */

struct arg_state
{
  unsigned ngrn;                /* Next general-purpose register number. */
  unsigned nsrn;                /* Next vector register number. */
  size_t nsaa;                  /* Next stack offset. */

#if defined (__APPLE__)
  unsigned allocating_variadic;
#endif
};

/* Initialize a procedure call argument marshalling state.  */
static void
arg_init (struct arg_state *state)
{
  state->ngrn = 0;
  state->nsrn = 0;
  state->nsaa = 0;
#if defined (__APPLE__)
  state->allocating_variadic = 0;
#endif
}

/* Allocate an aligned slot on the stack and return a pointer to it.  */
static void *
allocate_to_stack (struct arg_state *state, void *stack,
		   size_t alignment, size_t size)
{
  size_t nsaa = state->nsaa;

  /* Round up the NSAA to the larger of 8 or the natural
     alignment of the argument's type.  */
#if defined (__APPLE__)
  if (state->allocating_variadic && alignment < 8)
    alignment = 8;
#else
  if (alignment < 8)
    alignment = 8;
#endif
    
  nsaa = FFI_ALIGN (nsaa, alignment);
  state->nsaa = nsaa + size;

  return (char *)stack + nsaa;
}

static ffi_arg
extend_integer_type (void *source, int type)
{
  switch (type)
    {
    case FFI_TYPE_UINT8:
      return *(UINT8 *) source;
    case FFI_TYPE_SINT8:
      return *(SINT8 *) source;
    case FFI_TYPE_UINT16:
      return *(UINT16 *) source;
    case FFI_TYPE_SINT16:
      return *(SINT16 *) source;
    case FFI_TYPE_UINT32:
      return *(UINT32 *) source;
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT32:
      return *(SINT32 *) source;
    case FFI_TYPE_UINT64:
    case FFI_TYPE_SINT64:
      return *(UINT64 *) source;
      break;
    case FFI_TYPE_POINTER:
      return *(uintptr_t *) source;
    default:
      abort();
    }
}

#if defined(_MSC_VER)
void extend_hfa_type (void *dest, void *src, int h);
#else
static void
extend_hfa_type (void *dest, void *src, int h)
{
  ssize_t f = h - AARCH64_RET_S4;
  void *x0;

  asm volatile (
	"adr	%0, 0f\n"
"	add	%0, %0, %1\n"
"	br	%0\n"
"0:	ldp	s16, s17, [%3]\n"	/* S4 */
"	ldp	s18, s19, [%3, #8]\n"
"	b	4f\n"
"	ldp	s16, s17, [%3]\n"	/* S3 */
"	ldr	s18, [%3, #8]\n"
"	b	3f\n"
"	ldp	s16, s17, [%3]\n"	/* S2 */
"	b	2f\n"
"	nop\n"
"	ldr	s16, [%3]\n"		/* S1 */
"	b	1f\n"
"	nop\n"
"	ldp	d16, d17, [%3]\n"	/* D4 */
"	ldp	d18, d19, [%3, #16]\n"
"	b	4f\n"
"	ldp	d16, d17, [%3]\n"	/* D3 */
"	ldr	d18, [%3, #16]\n"
"	b	3f\n"
"	ldp	d16, d17, [%3]\n"	/* D2 */
"	b	2f\n"
"	nop\n"
"	ldr	d16, [%3]\n"		/* D1 */
"	b	1f\n"
"	nop\n"
"	ldp	q16, q17, [%3]\n"	/* Q4 */
"	ldp	q18, q19, [%3, #32]\n"
"	b	4f\n"
"	ldp	q16, q17, [%3]\n"	/* Q3 */
"	ldr	q18, [%3, #32]\n"
"	b	3f\n"
"	ldp	q16, q17, [%3]\n"	/* Q2 */
"	b	2f\n"
"	nop\n"
"	ldr	q16, [%3]\n"		/* Q1 */
"	b	1f\n"
"4:	str	q19, [%2, #48]\n"
"3:	str	q18, [%2, #32]\n"
"2:	str	q17, [%2, #16]\n"
"1:	str	q16, [%2]"
    : "=&r"(x0)
    : "r"(f * 12), "r"(dest), "r"(src)
    : "memory", "v16", "v17", "v18", "v19");
}
#endif

#if defined(_MSC_VER)
void* compress_hfa_type (void *dest, void *src, int h);
#else
static void *
compress_hfa_type (void *dest, void *reg, int h)
{
  switch (h)
    {
    case AARCH64_RET_S1:
      if (dest == reg)
	{
#ifdef __AARCH64EB__
	  dest += 12;
#endif
	}
      else
	*(float *)dest = *(float *)reg;
      break;
    case AARCH64_RET_S2:
      asm ("ldp q16, q17, [%1]\n\t"
	   "st2 { v16.s, v17.s }[0], [%0]"
	   : : "r"(dest), "r"(reg) : "memory", "v16", "v17");
      break;
    case AARCH64_RET_S3:
      asm ("ldp q16, q17, [%1]\n\t"
	   "ldr q18, [%1, #32]\n\t"
	   "st3 { v16.s, v17.s, v18.s }[0], [%0]"
	   : : "r"(dest), "r"(reg) : "memory", "v16", "v17", "v18");
      break;
    case AARCH64_RET_S4:
      asm ("ldp q16, q17, [%1]\n\t"
	   "ldp q18, q19, [%1, #32]\n\t"
	   "st4 { v16.s, v17.s, v18.s, v19.s }[0], [%0]"
	   : : "r"(dest), "r"(reg) : "memory", "v16", "v17", "v18", "v19");
      break;

    case AARCH64_RET_D1:
      if (dest == reg)
	{
#ifdef __AARCH64EB__
	  dest += 8;
#endif
	}
      else
	*(double *)dest = *(double *)reg;
      break;
    case AARCH64_RET_D2:
      asm ("ldp q16, q17, [%1]\n\t"
	   "st2 { v16.d, v17.d }[0], [%0]"
	   : : "r"(dest), "r"(reg) : "memory", "v16", "v17");
      break;
    case AARCH64_RET_D3:
      asm ("ldp q16, q17, [%1]\n\t"
	   "ldr q18, [%1, #32]\n\t"
	   "st3 { v16.d, v17.d, v18.d }[0], [%0]"
	   : : "r"(dest), "r"(reg) : "memory", "v16", "v17", "v18");
      break;
    case AARCH64_RET_D4:
      asm ("ldp q16, q17, [%1]\n\t"
	   "ldp q18, q19, [%1, #32]\n\t"
	   "st4 { v16.d, v17.d, v18.d, v19.d }[0], [%0]"
	   : : "r"(dest), "r"(reg) : "memory", "v16", "v17", "v18", "v19");
      break;

    default:
      if (dest != reg)
	return memcpy (dest, reg, 16 * (4 - (h & 3)));
      break;
    }
  return dest;
}
#endif

/* Either allocate an appropriate register for the argument type, or if
   none are available, allocate a stack slot and return a pointer
   to the allocated space.  */

static void *
allocate_int_to_reg_or_stack (struct call_context *context,
			      struct arg_state *state,
			      void *stack, size_t size)
{
  if (state->ngrn < N_X_ARG_REG)
    return &context->x[state->ngrn++];

  state->ngrn = N_X_ARG_REG;
  return allocate_to_stack (state, stack, size, size);
}

ffi_status FFI_HIDDEN
ffi_prep_cif_machdep (ffi_cif *cif)
{
  ffi_type *rtype = cif->rtype;
  size_t bytes = cif->bytes;
  int flags, i, n;

  switch (rtype->type)
    {
    case FFI_TYPE_VOID:
      flags = AARCH64_RET_VOID;
      break;
    case FFI_TYPE_UINT8:
      flags = AARCH64_RET_UINT8;
      break;
    case FFI_TYPE_UINT16:
      flags = AARCH64_RET_UINT16;
      break;
    case FFI_TYPE_UINT32:
      flags = AARCH64_RET_UINT32;
      break;
    case FFI_TYPE_SINT8:
      flags = AARCH64_RET_SINT8;
      break;
    case FFI_TYPE_SINT16:
      flags = AARCH64_RET_SINT16;
      break;
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT32:
      flags = AARCH64_RET_SINT32;
      break;
    case FFI_TYPE_SINT64:
    case FFI_TYPE_UINT64:
      flags = AARCH64_RET_INT64;
      break;
    case FFI_TYPE_POINTER:
      flags = (sizeof(void *) == 4 ? AARCH64_RET_UINT32 : AARCH64_RET_INT64);
      break;

    case FFI_TYPE_FLOAT:
    case FFI_TYPE_DOUBLE:
    case FFI_TYPE_LONGDOUBLE:
    case FFI_TYPE_STRUCT:
    case FFI_TYPE_COMPLEX:
      flags = is_vfp_type (rtype);
      if (flags == 0)
	{
	  size_t s = rtype->size;
	  if (s > 16)
	    {
	      flags = AARCH64_RET_VOID | AARCH64_RET_IN_MEM;
	      bytes += 8;
	    }
	  else if (s == 16)
	    flags = AARCH64_RET_INT128;
	  else if (s == 8)
	    flags = AARCH64_RET_INT64;
	  else
	    flags = AARCH64_RET_INT128 | AARCH64_RET_NEED_COPY;
	}
      break;

    default:
      abort();
    }

  for (i = 0, n = cif->nargs; i < n; i++)
    if (is_vfp_type (cif->arg_types[i]))
      {
	flags |= AARCH64_FLAG_ARG_V;
	break;
      }

  /* Round the stack up to a multiple of the stack alignment requirement. */
  cif->bytes = (unsigned) FFI_ALIGN(bytes, 16);
  cif->flags = flags;
#if defined (__APPLE__)
  cif->aarch64_nfixedargs = 0;
#endif

  return FFI_OK;
}

#if defined (__APPLE__)
/* Perform Apple-specific cif processing for variadic calls */
ffi_status FFI_HIDDEN
ffi_prep_cif_machdep_var(ffi_cif *cif, unsigned int nfixedargs,
			 unsigned int ntotalargs)
{
  ffi_status status = ffi_prep_cif_machdep (cif);
  cif->aarch64_nfixedargs = nfixedargs;
  return status;
}
#endif /* __APPLE__ */

extern void ffi_call_SYSV (struct call_context *context, void *frame,
			   void (*fn)(void), void *rvalue, int flags,
			   void *closure) FFI_HIDDEN;

/* Call a function with the provided arguments and capture the return
   value.  */
static void
ffi_call_int (ffi_cif *cif, void (*fn)(void), void *orig_rvalue,
	      void **avalue, void *closure)
{
  struct call_context *context;
  void *stack, *frame, *rvalue;
  struct arg_state state;
  size_t stack_bytes, rtype_size, rsize;
  int i, nargs, flags;
  ffi_type *rtype;

  flags = cif->flags;
  rtype = cif->rtype;
  rtype_size = rtype->size;
  stack_bytes = cif->bytes;

  /* If the target function returns a structure via hidden pointer,
     then we cannot allow a null rvalue.  Otherwise, mash a null
     rvalue to void return type.  */
  rsize = 0;
  if (flags & AARCH64_RET_IN_MEM)
    {
      if (orig_rvalue == NULL)
	rsize = rtype_size;
    }
  else if (orig_rvalue == NULL)
    flags &= AARCH64_FLAG_ARG_V;
  else if (flags & AARCH64_RET_NEED_COPY)
    rsize = 16;

  /* Allocate consectutive stack for everything we'll need.  */
  context = alloca (sizeof(struct call_context) + stack_bytes + 32 + rsize);
  stack = context + 1;
  frame = (void*)((uintptr_t)stack + (uintptr_t)stack_bytes);
  rvalue = (rsize ? (void*)((uintptr_t)frame + 32) : orig_rvalue);

  arg_init (&state);
  for (i = 0, nargs = cif->nargs; i < nargs; i++)
    {
      ffi_type *ty = cif->arg_types[i];
      size_t s = ty->size;
      void *a = avalue[i];
      int h, t;

      t = ty->type;
      switch (t)
	{
	case FFI_TYPE_VOID:
	  FFI_ASSERT (0);
	  break;

	/* If the argument is a basic type the argument is allocated to an
	   appropriate register, or if none are available, to the stack.  */
	case FFI_TYPE_INT:
	case FFI_TYPE_UINT8:
	case FFI_TYPE_SINT8:
	case FFI_TYPE_UINT16:
	case FFI_TYPE_SINT16:
	case FFI_TYPE_UINT32:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
	case FFI_TYPE_POINTER:
	do_pointer:
	  {
	    ffi_arg ext = extend_integer_type (a, t);
	    if (state.ngrn < N_X_ARG_REG)
	      context->x[state.ngrn++] = ext;
	    else
	      {
		void *d = allocate_to_stack (&state, stack, ty->alignment, s);
		state.ngrn = N_X_ARG_REG;
		/* Note that the default abi extends each argument
		   to a full 64-bit slot, while the iOS abi allocates
		   only enough space. */
#ifdef __APPLE__
		memcpy(d, a, s);
#else
		*(ffi_arg *)d = ext;
#endif
	      }
	  }
	  break;

	case FFI_TYPE_FLOAT:
	case FFI_TYPE_DOUBLE:
	case FFI_TYPE_LONGDOUBLE:
	case FFI_TYPE_STRUCT:
	case FFI_TYPE_COMPLEX:
	  {
	    void *dest;

	    h = is_vfp_type (ty);
	    if (h)
	      {
		int elems = 4 - (h & 3);
#ifdef _M_ARM64 /* for handling armasm calling convention */
                if (cif->is_variadic)
                  {
                    if (state.ngrn + elems <= N_X_ARG_REG)
                      {
                        dest = &context->x[state.ngrn];
                        state.ngrn += elems;
                        extend_hfa_type(dest, a, h);
                        break;
                      }
                    state.nsrn = N_X_ARG_REG;
                    dest = allocate_to_stack(&state, stack, ty->alignment, s);
                  }
                else
                  {
#endif /* for handling armasm calling convention */
	        if (state.nsrn + elems <= N_V_ARG_REG)
		  {
		    dest = &context->v[state.nsrn];
		    state.nsrn += elems;
		    extend_hfa_type (dest, a, h);
		    break;
		  }
		state.nsrn = N_V_ARG_REG;
		dest = allocate_to_stack (&state, stack, ty->alignment, s);
#ifdef _M_ARM64 /* for handling armasm calling convention */
	      }
#endif /* for handling armasm calling convention */
	      }
	    else if (s > 16)
	      {
		/* If the argument is a composite type that is larger than 16
		   bytes, then the argument has been copied to memory, and
		   the argument is replaced by a pointer to the copy.  */
		a = &avalue[i];
		t = FFI_TYPE_POINTER;
		s = sizeof (void *);
		goto do_pointer;
	      }
	    else
	      {
		size_t n = (s + 7) / 8;
		if (state.ngrn + n <= N_X_ARG_REG)
		  {
		    /* If the argument is a composite type and the size in
		       double-words is not more than the number of available
		       X registers, then the argument is copied into
		       consecutive X registers.  */
		    dest = &context->x[state.ngrn];
                    state.ngrn += (unsigned int)n;
		  }
		else
		  {
		    /* Otherwise, there are insufficient X registers. Further
		       X register allocations are prevented, the NSAA is
		       adjusted and the argument is copied to memory at the
		       adjusted NSAA.  */
		    state.ngrn = N_X_ARG_REG;
		    dest = allocate_to_stack (&state, stack, ty->alignment, s);
		  }
		}
	      memcpy (dest, a, s);
	    }
	  break;

	default:
	  abort();
	}

#if defined (__APPLE__)
      if (i + 1 == cif->aarch64_nfixedargs)
	{
	  state.ngrn = N_X_ARG_REG;
	  state.nsrn = N_V_ARG_REG;
	  state.allocating_variadic = 1;
	}
#endif
    }

  ffi_call_SYSV (context, frame, fn, rvalue, flags, closure);

  if (flags & AARCH64_RET_NEED_COPY)
    memcpy (orig_rvalue, rvalue, rtype_size);
}

void
ffi_call (ffi_cif *cif, void (*fn) (void), void *rvalue, void **avalue)
{
  ffi_call_int (cif, fn, rvalue, avalue, NULL);
}

#ifdef FFI_GO_CLOSURES
void
ffi_call_go (ffi_cif *cif, void (*fn) (void), void *rvalue,
	     void **avalue, void *closure)
{
  ffi_call_int (cif, fn, rvalue, avalue, closure);
}
#endif /* FFI_GO_CLOSURES */

/* Build a trampoline.  */

extern void ffi_closure_SYSV (void) FFI_HIDDEN;
extern void ffi_closure_SYSV_V (void) FFI_HIDDEN;

ffi_status
ffi_prep_closure_loc (ffi_closure *closure,
                      ffi_cif* cif,
                      void (*fun)(ffi_cif*,void*,void**,void*),
                      void *user_data,
                      void *codeloc)
{
  if (cif->abi != FFI_SYSV)
    return FFI_BAD_ABI;

  void (*start)(void);
  
  if (cif->flags & AARCH64_FLAG_ARG_V)
    start = ffi_closure_SYSV_V;
  else
    start = ffi_closure_SYSV;

#if FFI_EXEC_TRAMPOLINE_TABLE
#ifdef __MACH__
  void **config = (void **)((uint8_t *)codeloc - PAGE_MAX_SIZE);
  config[0] = closure;
  config[1] = start;
#endif
#else
  static const unsigned char trampoline[16] = {
    0x90, 0x00, 0x00, 0x58,	/* ldr	x16, tramp+16	*/
    0xf1, 0xff, 0xff, 0x10,	/* adr	x17, tramp+0	*/
    0x00, 0x02, 0x1f, 0xd6	/* br	x16		*/
  };
  char *tramp = closure->tramp;
  
  memcpy (tramp, trampoline, sizeof(trampoline));
  
  *(UINT64 *)(tramp + 16) = (uintptr_t)start;

  ffi_clear_cache(tramp, tramp + FFI_TRAMPOLINE_SIZE);

  /* Also flush the cache for code mapping.  */
#ifdef _M_ARM64
  // Not using dlmalloc.c for Windows ARM64 builds
  // so calling ffi_data_to_code_pointer() isn't necessary
  unsigned char *tramp_code = tramp;
  #else
  unsigned char *tramp_code = ffi_data_to_code_pointer (tramp);
  #endif
  ffi_clear_cache (tramp_code, tramp_code + FFI_TRAMPOLINE_SIZE);
#endif

  closure->cif = cif;
  closure->fun = fun;
  closure->user_data = user_data;

  return FFI_OK;
}

#ifdef FFI_GO_CLOSURES
extern void ffi_go_closure_SYSV (void) FFI_HIDDEN;
extern void ffi_go_closure_SYSV_V (void) FFI_HIDDEN;

ffi_status
ffi_prep_go_closure (ffi_go_closure *closure, ffi_cif* cif,
                     void (*fun)(ffi_cif*,void*,void**,void*))
{
  void (*start)(void);

  if (cif->abi != FFI_SYSV)
    return FFI_BAD_ABI;

  if (cif->flags & AARCH64_FLAG_ARG_V)
    start = ffi_go_closure_SYSV_V;
  else
    start = ffi_go_closure_SYSV;

  closure->tramp = start;
  closure->cif = cif;
  closure->fun = fun;

  return FFI_OK;
}
#endif /* FFI_GO_CLOSURES */

/* Primary handler to setup and invoke a function within a closure.

   A closure when invoked enters via the assembler wrapper
   ffi_closure_SYSV(). The wrapper allocates a call context on the
   stack, saves the interesting registers (from the perspective of
   the calling convention) into the context then passes control to
   ffi_closure_SYSV_inner() passing the saved context and a pointer to
   the stack at the point ffi_closure_SYSV() was invoked.

   On the return path the assembler wrapper will reload call context
   registers.

   ffi_closure_SYSV_inner() marshalls the call context into ffi value
   descriptors, invokes the wrapped function, then marshalls the return
   value back into the call context.  */

int FFI_HIDDEN
ffi_closure_SYSV_inner (ffi_cif *cif,
			void (*fun)(ffi_cif*,void*,void**,void*),
			void *user_data,
			struct call_context *context,
			void *stack, void *rvalue, void *struct_rvalue)
{
  void **avalue = (void**) alloca (cif->nargs * sizeof (void*));
  int i, h, nargs, flags;
  struct arg_state state;

  arg_init (&state);

  for (i = 0, nargs = cif->nargs; i < nargs; i++)
    {
      ffi_type *ty = cif->arg_types[i];
      int t = ty->type;
      size_t n, s = ty->size;

      switch (t)
	{
	case FFI_TYPE_VOID:
	  FFI_ASSERT (0);
	  break;

	case FFI_TYPE_INT:
	case FFI_TYPE_UINT8:
	case FFI_TYPE_SINT8:
	case FFI_TYPE_UINT16:
	case FFI_TYPE_SINT16:
	case FFI_TYPE_UINT32:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_UINT64:
	case FFI_TYPE_SINT64:
	case FFI_TYPE_POINTER:
	  avalue[i] = allocate_int_to_reg_or_stack (context, &state, stack, s);
	  break;

	case FFI_TYPE_FLOAT:
	case FFI_TYPE_DOUBLE:
	case FFI_TYPE_LONGDOUBLE:
	case FFI_TYPE_STRUCT:
	case FFI_TYPE_COMPLEX:
	  h = is_vfp_type (ty);
	  if (h)
	    {
	      n = 4 - (h & 3);
#ifdef _M_ARM64  /* for handling armasm calling convention */
              if (cif->is_variadic)
                {
                  if (state.ngrn + n <= N_X_ARG_REG)
                    {
                      void *reg = &context->x[state.ngrn];
                      state.ngrn += (unsigned int)n;
    
                      /* Eeek! We need a pointer to the structure, however the
                       homogeneous float elements are being passed in individual
                       registers, therefore for float and double the structure
                       is not represented as a contiguous sequence of bytes in
                       our saved register context.  We don't need the original
                       contents of the register storage, so we reformat the
                       structure into the same memory.  */
                      avalue[i] = compress_hfa_type(reg, reg, h);
                    }
                  else
                    {
                      state.ngrn = N_X_ARG_REG;
                      state.nsrn = N_V_ARG_REG;
                      avalue[i] = allocate_to_stack(&state, stack,
                             ty->alignment, s);
                    }
                }
              else
                {
#endif  /* for handling armasm calling convention */
                  if (state.nsrn + n <= N_V_ARG_REG)
                    {
                      void *reg = &context->v[state.nsrn];
                      state.nsrn += (unsigned int)n;
                      avalue[i] = compress_hfa_type(reg, reg, h);
                    }
                  else
                    {
                      state.nsrn = N_V_ARG_REG;
                      avalue[i] = allocate_to_stack(&state, stack,
                                                   ty->alignment, s);
                    }
#ifdef _M_ARM64  /* for handling armasm calling convention */
                }
#endif  /* for handling armasm calling convention */
            }
          else if (s > 16)
            {
              /* Replace Composite type of size greater than 16 with a
                  pointer.  */
              avalue[i] = *(void **)
              allocate_int_to_reg_or_stack (context, &state, stack,
                                         sizeof (void *));
            }
          else
            {
              n = (s + 7) / 8;
              if (state.ngrn + n <= N_X_ARG_REG)
                {
                  avalue[i] = &context->x[state.ngrn];
                  state.ngrn += (unsigned int)n;
                }
              else
                {
                  state.ngrn = N_X_ARG_REG;
                  avalue[i] = allocate_to_stack(&state, stack,
                                           ty->alignment, s);
                }
            }
          break;

        default:
          abort();
      }

#if defined (__APPLE__)
      if (i + 1 == cif->aarch64_nfixedargs)
	{
	  state.ngrn = N_X_ARG_REG;
	  state.nsrn = N_V_ARG_REG;
	  state.allocating_variadic = 1;
	}
#endif
    }

  flags = cif->flags;
  if (flags & AARCH64_RET_IN_MEM)
    rvalue = struct_rvalue;

  fun (cif, rvalue, avalue, user_data);

  return flags;
}

#endif /* (__aarch64__) || defined(__arm64__)|| defined (_M_ARM64)*/
>>>>>>> BRANCH (5dcb74 Move nested_struct3 test to closures directory)
