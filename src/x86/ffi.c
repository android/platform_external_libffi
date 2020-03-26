/* -----------------------------------------------------------------------
<<<<<<< HEAD   (1246a0 Merge "Remove redundant NOTICE copied from LICENSE.")
   ffi.c - Copyright (c) 1996, 1998, 1999, 2001, 2007, 2008  Red Hat, Inc.
           Copyright (c) 2002  Ranjit Mathew
           Copyright (c) 2002  Bo Thorsen
           Copyright (c) 2002  Roger Sayle
           Copyright (C) 2008, 2010  Free Software Foundation, Inc.

   x86 Foreign Function Interface

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#if !defined(__x86_64__) || defined(_WIN64) || defined(__CYGWIN__)

#ifdef _WIN64
#include <windows.h>
#endif

#include <ffi.h>
#include <ffi_common.h>

#include <stdlib.h>


/* ffi_prep_args is called by the assembly routine once stack space
   has been allocated for the function's arguments */

unsigned int ffi_prep_args(char *stack, extended_cif *ecif);
unsigned int ffi_prep_args(char *stack, extended_cif *ecif)
{
  register unsigned int i;
  register void **p_argv;
  register char *argp;
  register ffi_type **p_arg;
#ifndef X86_WIN64
  const int cabi = ecif->cif->abi;
  const int dir = (cabi == FFI_PASCAL || cabi == FFI_REGISTER) ? -1 : +1;
  unsigned int stack_args_count = 0;
  void *p_stack_data[3];
  char *argp2 = stack;
#else
  #define dir 1
#endif

  argp = stack;

  if ((ecif->cif->flags == FFI_TYPE_STRUCT
       || ecif->cif->flags == FFI_TYPE_MS_STRUCT)
#ifdef X86_WIN64
      && ((ecif->cif->rtype->size & (1 | 2 | 4 | 8)) == 0)
#endif
      )
    {
#ifndef X86_WIN64
      /* For fastcall/thiscall/register this is first register-passed
         argument.  */
      if (cabi == FFI_THISCALL || cabi == FFI_FASTCALL || cabi == FFI_REGISTER)
        {
          p_stack_data[stack_args_count] = argp;
          ++stack_args_count;
        }
#endif

      *(void **) argp = ecif->rvalue;
      argp += sizeof(void*);
    }

  p_arg  = ecif->cif->arg_types;
  p_argv = ecif->avalue;
  if (dir < 0)
    {
      const int nargs = ecif->cif->nargs - 1;
      if (nargs > 0)
      {
        p_arg  += nargs;
        p_argv += nargs;
      }
    }

  for (i = ecif->cif->nargs;
       i != 0;
       i--, p_arg += dir, p_argv += dir)
    {
      /* Align if necessary */
      if ((sizeof(void*) - 1) & (size_t) argp)
        argp = (char *) ALIGN(argp, sizeof(void*));

      size_t z = (*p_arg)->size;

#ifdef X86_WIN64
      if (z > FFI_SIZEOF_ARG
          || ((*p_arg)->type == FFI_TYPE_STRUCT
              && (z & (1 | 2 | 4 | 8)) == 0)
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
          || ((*p_arg)->type == FFI_TYPE_LONGDOUBLE)
#endif
          )
        {
          z = FFI_SIZEOF_ARG;
          *(void **)argp = *p_argv;
        }
      else if ((*p_arg)->type == FFI_TYPE_FLOAT)
        {
          memcpy(argp, *p_argv, z);
        }
      else
#endif
      if (z < FFI_SIZEOF_ARG)
        {
          z = FFI_SIZEOF_ARG;
          switch ((*p_arg)->type)
            {
            case FFI_TYPE_SINT8:
              *(ffi_sarg *) argp = (ffi_sarg)*(SINT8 *)(* p_argv);
              break;

            case FFI_TYPE_UINT8:
              *(ffi_arg *) argp = (ffi_arg)*(UINT8 *)(* p_argv);
              break;

            case FFI_TYPE_SINT16:
              *(ffi_sarg *) argp = (ffi_sarg)*(SINT16 *)(* p_argv);
              break;

            case FFI_TYPE_UINT16:
              *(ffi_arg *) argp = (ffi_arg)*(UINT16 *)(* p_argv);
              break;

            case FFI_TYPE_SINT32:
              *(ffi_sarg *) argp = (ffi_sarg)*(SINT32 *)(* p_argv);
              break;

            case FFI_TYPE_UINT32:
              *(ffi_arg *) argp = (ffi_arg)*(UINT32 *)(* p_argv);
              break;

            case FFI_TYPE_STRUCT:
              *(ffi_arg *) argp = *(ffi_arg *)(* p_argv);
              break;

            default:
              FFI_ASSERT(0);
            }
        }
      else
        {
          memcpy(argp, *p_argv, z);
        }

#ifndef X86_WIN64
    /* For thiscall/fastcall/register convention register-passed arguments
       are the first two none-floating-point arguments with a size
       smaller or equal to sizeof (void*).  */
    if ((z == FFI_SIZEOF_ARG)
        && ((cabi == FFI_REGISTER)
          || (cabi == FFI_THISCALL && stack_args_count < 1)
          || (cabi == FFI_FASTCALL && stack_args_count < 2))
        && ((*p_arg)->type != FFI_TYPE_FLOAT && (*p_arg)->type != FFI_TYPE_STRUCT)
       )
      {
        if (dir < 0 && stack_args_count > 2)
          {
            /* Iterating arguments backwards, so first register-passed argument
               will be passed last. Shift temporary values to make place. */
            p_stack_data[0] = p_stack_data[1];
            p_stack_data[1] = p_stack_data[2];
            stack_args_count = 2;
          }

        p_stack_data[stack_args_count] = argp;
        ++stack_args_count;
      }
#endif

#ifdef X86_WIN64
      argp += (z + sizeof(void*) - 1) & ~(sizeof(void*) - 1);
#else
      argp += z;
#endif
    }

#ifndef X86_WIN64
  /* We need to move the register-passed arguments for thiscall/fastcall/register
     on top of stack, so that those can be moved to registers by call-handler.  */
  if (stack_args_count > 0)
    {
      if (dir < 0 && stack_args_count > 1)
        {
          /* Reverse order if iterating arguments backwards */
          ffi_arg tmp = *(ffi_arg*) p_stack_data[0];
          *(ffi_arg*) p_stack_data[0] = *(ffi_arg*) p_stack_data[stack_args_count - 1];
          *(ffi_arg*) p_stack_data[stack_args_count - 1] = tmp;
        }
      
      int i;
      for (i = 0; i < stack_args_count; i++)
        {
          if (p_stack_data[i] != argp2)
            {
              ffi_arg tmp = *(ffi_arg*) p_stack_data[i];
              memmove (argp2 + FFI_SIZEOF_ARG, argp2, (size_t) ((char*) p_stack_data[i] - (char*)argp2));
              *(ffi_arg *) argp2 = tmp;
            }

          argp2 += FFI_SIZEOF_ARG;
        }
    }

    return stack_args_count;
#endif
    return 0;
}

/* Perform machine dependent cif processing */
ffi_status ffi_prep_cif_machdep(ffi_cif *cif)
{
  unsigned int i;
  ffi_type **ptr;

  /* Set the return type flag */
  switch (cif->rtype->type)
    {
    case FFI_TYPE_VOID:
    case FFI_TYPE_UINT8:
    case FFI_TYPE_UINT16:
    case FFI_TYPE_SINT8:
    case FFI_TYPE_SINT16:
#ifdef X86_WIN64
    case FFI_TYPE_UINT32:
    case FFI_TYPE_SINT32:
#endif
    case FFI_TYPE_SINT64:
    case FFI_TYPE_FLOAT:
    case FFI_TYPE_DOUBLE:
#ifndef X86_WIN64
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
    case FFI_TYPE_LONGDOUBLE:
#endif
#endif
      cif->flags = (unsigned) cif->rtype->type;
      break;

    case FFI_TYPE_UINT64:
#ifdef X86_WIN64
    case FFI_TYPE_POINTER:
#endif
      cif->flags = FFI_TYPE_SINT64;
      break;

    case FFI_TYPE_STRUCT:
#ifndef X86
      if (cif->rtype->size == 1)
        {
          cif->flags = FFI_TYPE_SMALL_STRUCT_1B; /* same as char size */
        }
      else if (cif->rtype->size == 2)
        {
          cif->flags = FFI_TYPE_SMALL_STRUCT_2B; /* same as short size */
        }
      else if (cif->rtype->size == 4)
        {
#ifdef X86_WIN64
          cif->flags = FFI_TYPE_SMALL_STRUCT_4B;
#else
          cif->flags = FFI_TYPE_INT; /* same as int type */
#endif
        }
      else if (cif->rtype->size == 8)
        {
          cif->flags = FFI_TYPE_SINT64; /* same as int64 type */
        }
      else
#endif
        {
#ifdef X86_WIN32
          if (cif->abi == FFI_MS_CDECL)
            cif->flags = FFI_TYPE_MS_STRUCT;
          else
#endif
            cif->flags = FFI_TYPE_STRUCT;
          /* allocate space for return value pointer */
          cif->bytes += ALIGN(sizeof(void*), FFI_SIZEOF_ARG);
        }
      break;

    default:
#ifdef X86_WIN64
      cif->flags = FFI_TYPE_SINT64;
      break;
    case FFI_TYPE_INT:
      cif->flags = FFI_TYPE_SINT32;
#else
      cif->flags = FFI_TYPE_INT;
#endif
      break;
    }

  for (ptr = cif->arg_types, i = cif->nargs; i > 0; i--, ptr++)
    {
      if (((*ptr)->alignment - 1) & cif->bytes)
        cif->bytes = ALIGN(cif->bytes, (*ptr)->alignment);
      cif->bytes += (unsigned)ALIGN((*ptr)->size, FFI_SIZEOF_ARG);
    }

#ifdef X86_WIN64
  /* ensure space for storing four registers */
  cif->bytes += 4 * FFI_SIZEOF_ARG;
#endif

#ifndef X86_WIN32
#ifndef X86_WIN64
  if (cif->abi == FFI_SYSV || cif->abi == FFI_UNIX64)
#endif
    cif->bytes = (cif->bytes + 15) & ~0xF;
#endif

  return FFI_OK;
}

#ifdef X86_WIN64
extern int
ffi_call_win64(unsigned int (*)(char *, extended_cif *), extended_cif *,
               unsigned, unsigned, unsigned *, void (*fn)(void));
#else
extern void
ffi_call_win32(unsigned int (*)(char *, extended_cif *), extended_cif *,
               unsigned, unsigned, unsigned, unsigned *, void (*fn)(void));
extern void ffi_call_SYSV(void (*)(char *, extended_cif *), extended_cif *,
                          unsigned, unsigned, unsigned *, void (*fn)(void));
#endif

void ffi_call(ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue)
{
  extended_cif ecif;

  ecif.cif = cif;
  ecif.avalue = avalue;
  
  /* If the return value is a struct and we don't have a return */
  /* value address then we need to make one                     */

#ifdef X86_WIN64
  if (rvalue == NULL
      && cif->flags == FFI_TYPE_STRUCT
      && ((cif->rtype->size & (1 | 2 | 4 | 8)) == 0))
    {
      ecif.rvalue = alloca((cif->rtype->size + 0xF) & ~0xF);
    }
#else
  if (rvalue == NULL
      && (cif->flags == FFI_TYPE_STRUCT
          || cif->flags == FFI_TYPE_MS_STRUCT))
    {
      ecif.rvalue = alloca(cif->rtype->size);
    }
#endif
  else
    ecif.rvalue = rvalue;
    
  
  switch (cif->abi) 
    {
#ifdef X86_WIN64
    case FFI_WIN64:
      ffi_call_win64(ffi_prep_args, &ecif, cif->bytes,
                     cif->flags, ecif.rvalue, fn);
      break;
#else
#ifndef X86_WIN32
    case FFI_SYSV:
      ffi_call_SYSV(ffi_prep_args, &ecif, cif->bytes, cif->flags, ecif.rvalue,
                    fn);
      break;
#else
    case FFI_SYSV:
    case FFI_MS_CDECL:
#endif
    case FFI_STDCALL:
    case FFI_THISCALL:
    case FFI_FASTCALL:
    case FFI_PASCAL:
    case FFI_REGISTER:
      ffi_call_win32(ffi_prep_args, &ecif, cif->abi, cif->bytes, cif->flags,
                     ecif.rvalue, fn);
      break;
#endif
    default:
      FFI_ASSERT(0);
      break;
    }
}


/** private members **/

/* The following __attribute__((regparm(1))) decorations will have no effect
   on MSVC or SUNPRO_C -- standard conventions apply. */
static unsigned int ffi_prep_incoming_args (char *stack, void **ret,
                                            void** args, ffi_cif* cif);
void FFI_HIDDEN ffi_closure_SYSV (ffi_closure *)
     __attribute__ ((regparm(1)));
unsigned int FFI_HIDDEN ffi_closure_SYSV_inner (ffi_closure *, void **, void *)
     __attribute__ ((regparm(1)));
unsigned int FFI_HIDDEN ffi_closure_WIN32_inner (ffi_closure *, void **, void *)
     __attribute__ ((regparm(1)));
void FFI_HIDDEN ffi_closure_raw_SYSV (ffi_raw_closure *)
     __attribute__ ((regparm(1)));
#ifdef X86_WIN32
void FFI_HIDDEN ffi_closure_raw_THISCALL (ffi_raw_closure *)
     __attribute__ ((regparm(1)));
#endif
#ifndef X86_WIN64
void FFI_HIDDEN ffi_closure_STDCALL (ffi_closure *);
void FFI_HIDDEN ffi_closure_THISCALL (ffi_closure *);
void FFI_HIDDEN ffi_closure_FASTCALL (ffi_closure *);
void FFI_HIDDEN ffi_closure_REGISTER (ffi_closure *);
#else
void FFI_HIDDEN ffi_closure_win64 (ffi_closure *);
#endif

/* This function is jumped to by the trampoline */

#ifdef X86_WIN64
void * FFI_HIDDEN
ffi_closure_win64_inner (ffi_closure *closure, void *args) {
  ffi_cif       *cif;
  void         **arg_area;
  void          *result;
  void          *resp = &result;

  cif         = closure->cif;
  arg_area    = (void**) alloca (cif->nargs * sizeof (void*));  

  /* this call will initialize ARG_AREA, such that each
   * element in that array points to the corresponding 
   * value on the stack; and if the function returns
   * a structure, it will change RESP to point to the
   * structure return address.  */

  ffi_prep_incoming_args(args, &resp, arg_area, cif);
  
  (closure->fun) (cif, resp, arg_area, closure->user_data);

  /* The result is returned in rax.  This does the right thing for
     result types except for floats; we have to 'mov xmm0, rax' in the
     caller to correct this.
     TODO: structure sizes of 3 5 6 7 are returned by reference, too!!!
  */
  return cif->rtype->size > sizeof(void *) ? resp : *(void **)resp;
}

#else
unsigned int FFI_HIDDEN __attribute__ ((regparm(1)))
ffi_closure_SYSV_inner (ffi_closure *closure, void **respp, void *args)
{
  /* our various things...  */
  ffi_cif       *cif;
  void         **arg_area;

  cif         = closure->cif;
  arg_area    = (void**) alloca (cif->nargs * sizeof (void*));  

  /* this call will initialize ARG_AREA, such that each
   * element in that array points to the corresponding 
   * value on the stack; and if the function returns
   * a structure, it will change RESP to point to the
   * structure return address.  */

  ffi_prep_incoming_args(args, respp, arg_area, cif);

  (closure->fun) (cif, *respp, arg_area, closure->user_data);

  return cif->flags;
}

unsigned int FFI_HIDDEN __attribute__ ((regparm(1)))
ffi_closure_WIN32_inner (ffi_closure *closure, void **respp, void *args)
{
  /* our various things...  */
  ffi_cif       *cif;
  void         **arg_area;
  unsigned int   ret;

  cif         = closure->cif;
  arg_area    = (void**) alloca (cif->nargs * sizeof (void*));  

  /* this call will initialize ARG_AREA, such that each
   * element in that array points to the corresponding 
   * value on the stack; and if the function returns
   * a structure, it will change RESP to point to the
   * structure return address.  */

  ret = ffi_prep_incoming_args(args, respp, arg_area, cif);

  (closure->fun) (cif, *respp, arg_area, closure->user_data);

  return ret;
}
#endif /* !X86_WIN64 */

static unsigned int
ffi_prep_incoming_args(char *stack, void **rvalue, void **avalue,
                       ffi_cif *cif)
{
  register unsigned int i;
  register void **p_argv;
  register char *argp;
  register ffi_type **p_arg;
#ifndef X86_WIN64
  const int cabi = cif->abi;
  const int dir = (cabi == FFI_PASCAL || cabi == FFI_REGISTER) ? -1 : +1;
  const unsigned int max_stack_count = (cabi == FFI_THISCALL) ? 1
                                     : (cabi == FFI_FASTCALL) ? 2
                                     : (cabi == FFI_REGISTER) ? 3
                                     : 0;
  unsigned int passed_regs = 0;
  void *p_stack_data[3] = { stack - 1 };
#else
  #define dir 1
#endif

  argp = stack;
#ifndef X86_WIN64
  argp += max_stack_count * FFI_SIZEOF_ARG;
#endif

  if ((cif->flags == FFI_TYPE_STRUCT
       || cif->flags == FFI_TYPE_MS_STRUCT)
#ifdef X86_WIN64
      && ((cif->rtype->size & (1 | 2 | 4 | 8)) == 0)
#endif
      )
    {
#ifndef X86_WIN64
      if (passed_regs < max_stack_count)
        {
          *rvalue = *(void**) (stack + (passed_regs*FFI_SIZEOF_ARG));
          ++passed_regs;
        }
      else
#endif
        {
          *rvalue = *(void **) argp;
          argp += sizeof(void *);
        }
    }

#ifndef X86_WIN64
  /* Do register arguments first  */
  for (i = 0, p_arg = cif->arg_types; 
       i < cif->nargs && passed_regs < max_stack_count;
       i++, p_arg++)
    {
      if ((*p_arg)->type == FFI_TYPE_FLOAT
         || (*p_arg)->type == FFI_TYPE_STRUCT)
        continue;

      size_t sz = (*p_arg)->size;
      if(sz == 0 || sz > FFI_SIZEOF_ARG)
        continue;

      p_stack_data[passed_regs] = avalue + i;
      avalue[i] = stack + (passed_regs*FFI_SIZEOF_ARG);
      ++passed_regs;
    }
#endif

  p_arg = cif->arg_types;
  p_argv = avalue;
  if (dir < 0)
    {
      const int nargs = cif->nargs - 1;
      if (nargs > 0)
      {
        p_arg  += nargs;
        p_argv += nargs;
      }
    }

  for (i = cif->nargs;
       i != 0;
       i--, p_arg += dir, p_argv += dir)
    {
      /* Align if necessary */
      if ((sizeof(void*) - 1) & (size_t) argp)
        argp = (char *) ALIGN(argp, sizeof(void*));

      size_t z = (*p_arg)->size;

#ifdef X86_WIN64
      if (z > FFI_SIZEOF_ARG
          || ((*p_arg)->type == FFI_TYPE_STRUCT
              && (z & (1 | 2 | 4 | 8)) == 0)
#if FFI_TYPE_DOUBLE != FFI_TYPE_LONGDOUBLE
          || ((*p_arg)->type == FFI_TYPE_LONGDOUBLE)
#endif
          )
        {
          z = FFI_SIZEOF_ARG;
          *p_argv = *(void **)argp;
        }
      else
#else
      if (passed_regs > 0
          && z <= FFI_SIZEOF_ARG
          && (p_argv == p_stack_data[0]
            || p_argv == p_stack_data[1]
            || p_argv == p_stack_data[2]))
        {
          /* Already assigned a register value */
          continue;
        }
      else
#endif
        {
          /* because we're little endian, this is what it turns into.   */
          *p_argv = (void*) argp;
        }

#ifdef X86_WIN64
      argp += (z + sizeof(void*) - 1) & ~(sizeof(void*) - 1);
#else
      argp += z;
#endif
    }

  return (size_t)argp - (size_t)stack;
}

#define FFI_INIT_TRAMPOLINE_WIN64(TRAMP,FUN,CTX,MASK) \
{ unsigned char *__tramp = (unsigned char*)(TRAMP); \
   void*  __fun = (void*)(FUN); \
   void*  __ctx = (void*)(CTX); \
   *(unsigned char*) &__tramp[0] = 0x41; \
   *(unsigned char*) &__tramp[1] = 0xbb; \
   *(unsigned int*) &__tramp[2] = MASK; /* mov $mask, %r11 */ \
   *(unsigned char*) &__tramp[6] = 0x48; \
   *(unsigned char*) &__tramp[7] = 0xb8; \
   *(void**) &__tramp[8] = __ctx; /* mov __ctx, %rax */ \
   *(unsigned char *)  &__tramp[16] = 0x49; \
   *(unsigned char *)  &__tramp[17] = 0xba; \
   *(void**) &__tramp[18] = __fun; /* mov __fun, %r10 */ \
   *(unsigned char *)  &__tramp[26] = 0x41; \
   *(unsigned char *)  &__tramp[27] = 0xff; \
   *(unsigned char *)  &__tramp[28] = 0xe2; /* jmp %r10 */ \
 }

/* How to make a trampoline.  Derived from gcc/config/i386/i386.c. */

#define FFI_INIT_TRAMPOLINE(TRAMP,FUN,CTX) \
{ unsigned char *__tramp = (unsigned char*)(TRAMP); \
   unsigned int  __fun = (unsigned int)(FUN); \
   unsigned int  __ctx = (unsigned int)(CTX); \
   unsigned int  __dis = __fun - (__ctx + 10);  \
   *(unsigned char*) &__tramp[0] = 0xb8; \
   *(unsigned int*)  &__tramp[1] = __ctx; /* movl __ctx, %eax */ \
   *(unsigned char*) &__tramp[5] = 0xe9; \
   *(unsigned int*)  &__tramp[6] = __dis; /* jmp __fun  */ \
 }

#define FFI_INIT_TRAMPOLINE_RAW_THISCALL(TRAMP,FUN,CTX,SIZE) \
{ unsigned char *__tramp = (unsigned char*)(TRAMP); \
   unsigned int  __fun = (unsigned int)(FUN); \
   unsigned int  __ctx = (unsigned int)(CTX); \
   unsigned int  __dis = __fun - (__ctx + 49);  \
   unsigned short __size = (unsigned short)(SIZE); \
   *(unsigned int *) &__tramp[0] = 0x8324048b;      /* mov (%esp), %eax */ \
   *(unsigned int *) &__tramp[4] = 0x4c890cec;      /* sub $12, %esp */ \
   *(unsigned int *) &__tramp[8] = 0x04890424;      /* mov %ecx, 4(%esp) */ \
   *(unsigned char*) &__tramp[12] = 0x24;           /* mov %eax, (%esp) */ \
   *(unsigned char*) &__tramp[13] = 0xb8; \
   *(unsigned int *) &__tramp[14] = __size;         /* mov __size, %eax */ \
   *(unsigned int *) &__tramp[18] = 0x08244c8d;     /* lea 8(%esp), %ecx */ \
   *(unsigned int *) &__tramp[22] = 0x4802e8c1;     /* shr $2, %eax ; dec %eax */ \
   *(unsigned short*) &__tramp[26] = 0x0b74;        /* jz 1f */ \
   *(unsigned int *) &__tramp[28] = 0x8908518b;     /* 2b: mov 8(%ecx), %edx */ \
   *(unsigned int *) &__tramp[32] = 0x04c18311;     /* mov %edx, (%ecx) ; add $4, %ecx */ \
   *(unsigned char*) &__tramp[36] = 0x48;           /* dec %eax */ \
   *(unsigned short*) &__tramp[37] = 0xf575;        /* jnz 2b ; 1f: */ \
   *(unsigned char*) &__tramp[39] = 0xb8; \
   *(unsigned int*)  &__tramp[40] = __ctx;          /* movl __ctx, %eax */ \
   *(unsigned char *)  &__tramp[44] = 0xe8; \
   *(unsigned int*)  &__tramp[45] = __dis;          /* call __fun  */ \
   *(unsigned char*)  &__tramp[49] = 0xc2;          /* ret  */ \
   *(unsigned short*)  &__tramp[50] = (__size + 8); /* ret (__size + 8)  */ \
 }

#define FFI_INIT_TRAMPOLINE_WIN32(TRAMP,FUN,CTX)  \
{ unsigned char *__tramp = (unsigned char*)(TRAMP); \
   unsigned int  __fun = (unsigned int)(FUN); \
   unsigned int  __ctx = (unsigned int)(CTX); \
   unsigned int  __dis = __fun - (__ctx + 10); \
   *(unsigned char*) &__tramp[0] = 0x68; \
   *(unsigned int*)  &__tramp[1] = __ctx; /* push __ctx */ \
   *(unsigned char*) &__tramp[5] = 0xe9; \
   *(unsigned int*)  &__tramp[6] = __dis; /* jmp __fun  */ \
 }

/* the cif must already be prep'ed */

ffi_status
ffi_prep_closure_loc (ffi_closure* closure,
                      ffi_cif* cif,
                      void (*fun)(ffi_cif*,void*,void**,void*),
                      void *user_data,
                      void *codeloc)
{
#ifdef X86_WIN64
#define ISFLOAT(IDX) (cif->arg_types[IDX]->type == FFI_TYPE_FLOAT || cif->arg_types[IDX]->type == FFI_TYPE_DOUBLE)
#define FLAG(IDX) (cif->nargs>(IDX)&&ISFLOAT(IDX)?(1<<(IDX)):0)
  if (cif->abi == FFI_WIN64) 
    {
      int mask = FLAG(0)|FLAG(1)|FLAG(2)|FLAG(3);
      FFI_INIT_TRAMPOLINE_WIN64 (&closure->tramp[0],
                                 &ffi_closure_win64,
                                 codeloc, mask);
      /* make sure we can execute here */
    }
#else
  if (cif->abi == FFI_SYSV)
    {
      FFI_INIT_TRAMPOLINE (&closure->tramp[0],
                           &ffi_closure_SYSV,
                           (void*)codeloc);
    }
  else if (cif->abi == FFI_REGISTER)
    {
      FFI_INIT_TRAMPOLINE_WIN32 (&closure->tramp[0],
                                   &ffi_closure_REGISTER,
                                   (void*)codeloc);
    }
  else if (cif->abi == FFI_FASTCALL)
    {
      FFI_INIT_TRAMPOLINE_WIN32 (&closure->tramp[0],
                                   &ffi_closure_FASTCALL,
                                   (void*)codeloc);
    }
  else if (cif->abi == FFI_THISCALL)
    {
      FFI_INIT_TRAMPOLINE_WIN32 (&closure->tramp[0],
                                   &ffi_closure_THISCALL,
                                   (void*)codeloc);
    }
  else if (cif->abi == FFI_STDCALL || cif->abi == FFI_PASCAL)
    {
      FFI_INIT_TRAMPOLINE_WIN32 (&closure->tramp[0],
                                   &ffi_closure_STDCALL,
                                   (void*)codeloc);
    }
#ifdef X86_WIN32
  else if (cif->abi == FFI_MS_CDECL)
    {
      FFI_INIT_TRAMPOLINE (&closure->tramp[0],
                           &ffi_closure_SYSV,
                           (void*)codeloc);
    }
#endif /* X86_WIN32 */
#endif /* !X86_WIN64 */
  else
    {
      return FFI_BAD_ABI;
    }
    
  closure->cif  = cif;
  closure->user_data = user_data;
  closure->fun  = fun;

  return FFI_OK;
}

/* ------- Native raw API support -------------------------------- */

#if !FFI_NO_RAW_API

ffi_status
ffi_prep_raw_closure_loc (ffi_raw_closure* closure,
                          ffi_cif* cif,
                          void (*fun)(ffi_cif*,void*,ffi_raw*,void*),
                          void *user_data,
                          void *codeloc)
{
  int i;

  if (cif->abi != FFI_SYSV
#ifdef X86_WIN32
      && cif->abi != FFI_THISCALL
#endif
     )
    return FFI_BAD_ABI;

  /* we currently don't support certain kinds of arguments for raw
     closures.  This should be implemented by a separate assembly
     language routine, since it would require argument processing,
     something we don't do now for performance.  */

  for (i = cif->nargs-1; i >= 0; i--)
    {
      FFI_ASSERT (cif->arg_types[i]->type != FFI_TYPE_STRUCT);
      FFI_ASSERT (cif->arg_types[i]->type != FFI_TYPE_LONGDOUBLE);
    }
  
#ifdef X86_WIN32
  if (cif->abi == FFI_SYSV)
    {
#endif
  FFI_INIT_TRAMPOLINE (&closure->tramp[0], &ffi_closure_raw_SYSV,
                       codeloc);
#ifdef X86_WIN32
    }
  else if (cif->abi == FFI_THISCALL)
    {
      FFI_INIT_TRAMPOLINE_RAW_THISCALL (&closure->tramp[0], &ffi_closure_raw_THISCALL, codeloc, cif->bytes);
    }
#endif
  closure->cif  = cif;
  closure->user_data = user_data;
  closure->fun  = fun;

  return FFI_OK;
}

static unsigned int 
ffi_prep_args_raw(char *stack, extended_cif *ecif)
{
  const ffi_cif *cif = ecif->cif;
  unsigned int i, passed_regs = 0;
  
#ifndef X86_WIN64
  const unsigned int abi = cif->abi;
  const unsigned int max_regs = (abi == FFI_THISCALL) ? 1
                              : (abi == FFI_FASTCALL) ? 2
                              : (abi == FFI_REGISTER) ? 3
                              : 0;

  if (cif->flags == FFI_TYPE_STRUCT)
    ++passed_regs;
  
  for (i = 0; i < cif->nargs && passed_regs <= max_regs; i++)
    {
      if (cif->arg_types[i]->type == FFI_TYPE_FLOAT
         || cif->arg_types[i]->type == FFI_TYPE_STRUCT)
        continue;

      size_t sz = cif->arg_types[i]->size;
      if (sz == 0 || sz > FFI_SIZEOF_ARG)
        continue;

      ++passed_regs;
    }
#endif

  memcpy (stack, ecif->avalue, cif->bytes);
  return passed_regs;
}

/* we borrow this routine from libffi (it must be changed, though, to
 * actually call the function passed in the first argument.  as of
 * libffi-1.20, this is not the case.)
 */

void
ffi_raw_call(ffi_cif *cif, void (*fn)(void), void *rvalue, ffi_raw *fake_avalue)
{
  extended_cif ecif;
  void **avalue = (void **)fake_avalue;

  ecif.cif = cif;
  ecif.avalue = avalue;
  
  /* If the return value is a struct and we don't have a return */
  /* value address then we need to make one                     */

  if (rvalue == NULL
      && (cif->flags == FFI_TYPE_STRUCT
          || cif->flags == FFI_TYPE_MS_STRUCT))
    {
      ecif.rvalue = alloca(cif->rtype->size);
    }
  else
    ecif.rvalue = rvalue;
    
  
  switch (cif->abi) 
    {
#ifndef X86_WIN32
    case FFI_SYSV:
      ffi_call_SYSV(ffi_prep_args_raw, &ecif, cif->bytes, cif->flags,
                    ecif.rvalue, fn);
      break;
#else
    case FFI_SYSV:
    case FFI_MS_CDECL:
#endif
#ifndef X86_WIN64
    case FFI_STDCALL:
    case FFI_THISCALL:
    case FFI_FASTCALL:
    case FFI_PASCAL:
    case FFI_REGISTER:
      ffi_call_win32(ffi_prep_args_raw, &ecif, cif->abi, cif->bytes, cif->flags,
                     ecif.rvalue, fn);
      break;
#endif
    default:
      FFI_ASSERT(0);
      break;
    }
}

#endif

#endif /* !__x86_64__  || X86_WIN64 */

=======
   ffi.c - Copyright (c) 2017  Anthony Green
           Copyright (c) 1996, 1998, 1999, 2001, 2007, 2008  Red Hat, Inc.
           Copyright (c) 2002  Ranjit Mathew
           Copyright (c) 2002  Bo Thorsen
           Copyright (c) 2002  Roger Sayle
           Copyright (C) 2008, 2010  Free Software Foundation, Inc.

   x86 Foreign Function Interface

   Permission is hereby granted, free of charge, to any person obtaining
   a copy of this software and associated documentation files (the
   ``Software''), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish,
   distribute, sublicense, and/or sell copies of the Software, and to
   permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included
   in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED ``AS IS'', WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
   HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
   DEALINGS IN THE SOFTWARE.
   ----------------------------------------------------------------------- */

#if defined(__i386__) || defined(_M_IX86)
#include <ffi.h>
#include <ffi_common.h>
#include <stdint.h>
#include <stdlib.h>
#include "internal.h"

/* Force FFI_TYPE_LONGDOUBLE to be different than FFI_TYPE_DOUBLE;
   all further uses in this file will refer to the 80-bit type.  */
#if FFI_TYPE_LONGDOUBLE != FFI_TYPE_DOUBLE
# if FFI_TYPE_LONGDOUBLE != 4
#  error FFI_TYPE_LONGDOUBLE out of date
# endif
#else
# undef FFI_TYPE_LONGDOUBLE
# define FFI_TYPE_LONGDOUBLE 4
#endif

#if defined(__GNUC__) && !defined(__declspec)
# define __declspec(x)  __attribute__((x))
#endif

#if defined(_MSC_VER) && defined(_M_IX86)
/* Stack is not 16-byte aligned on Windows.  */
#define STACK_ALIGN(bytes) (bytes)
#else
#define STACK_ALIGN(bytes) FFI_ALIGN (bytes, 16)
#endif

/* Perform machine dependent cif processing.  */
ffi_status FFI_HIDDEN
ffi_prep_cif_machdep(ffi_cif *cif)
{
  size_t bytes = 0;
  int i, n, flags, cabi = cif->abi;

  switch (cabi)
    {
    case FFI_SYSV:
    case FFI_STDCALL:
    case FFI_THISCALL:
    case FFI_FASTCALL:
    case FFI_MS_CDECL:
    case FFI_PASCAL:
    case FFI_REGISTER:
      break;
    default:
      return FFI_BAD_ABI;
    }

  switch (cif->rtype->type)
    {
    case FFI_TYPE_VOID:
      flags = X86_RET_VOID;
      break;
    case FFI_TYPE_FLOAT:
      flags = X86_RET_FLOAT;
      break;
    case FFI_TYPE_DOUBLE:
      flags = X86_RET_DOUBLE;
      break;
    case FFI_TYPE_LONGDOUBLE:
      flags = X86_RET_LDOUBLE;
      break;
    case FFI_TYPE_UINT8:
      flags = X86_RET_UINT8;
      break;
    case FFI_TYPE_UINT16:
      flags = X86_RET_UINT16;
      break;
    case FFI_TYPE_SINT8:
      flags = X86_RET_SINT8;
      break;
    case FFI_TYPE_SINT16:
      flags = X86_RET_SINT16;
      break;
    case FFI_TYPE_INT:
    case FFI_TYPE_SINT32:
    case FFI_TYPE_UINT32:
    case FFI_TYPE_POINTER:
      flags = X86_RET_INT32;
      break;
    case FFI_TYPE_SINT64:
    case FFI_TYPE_UINT64:
      flags = X86_RET_INT64;
      break;
    case FFI_TYPE_STRUCT:
#ifndef X86
      /* ??? This should be a different ABI rather than an ifdef.  */
      if (cif->rtype->size == 1)
	flags = X86_RET_STRUCT_1B;
      else if (cif->rtype->size == 2)
	flags = X86_RET_STRUCT_2B;
      else if (cif->rtype->size == 4)
	flags = X86_RET_INT32;
      else if (cif->rtype->size == 8)
	flags = X86_RET_INT64;
      else
#endif
	{
	do_struct:
	  switch (cabi)
	    {
	    case FFI_THISCALL:
	    case FFI_FASTCALL:
	    case FFI_STDCALL:
	    case FFI_MS_CDECL:
	      flags = X86_RET_STRUCTARG;
	      break;
	    default:
	      flags = X86_RET_STRUCTPOP;
	      break;
	    }
	  /* Allocate space for return value pointer.  */
	  bytes += FFI_ALIGN (sizeof(void*), FFI_SIZEOF_ARG);
	}
      break;
    case FFI_TYPE_COMPLEX:
      switch (cif->rtype->elements[0]->type)
	{
	case FFI_TYPE_DOUBLE:
	case FFI_TYPE_LONGDOUBLE:
	case FFI_TYPE_SINT64:
	case FFI_TYPE_UINT64:
	  goto do_struct;
	case FFI_TYPE_FLOAT:
	case FFI_TYPE_INT:
	case FFI_TYPE_SINT32:
	case FFI_TYPE_UINT32:
	  flags = X86_RET_INT64;
	  break;
	case FFI_TYPE_SINT16:
	case FFI_TYPE_UINT16:
	  flags = X86_RET_INT32;
	  break;
	case FFI_TYPE_SINT8:
	case FFI_TYPE_UINT8:
	  flags = X86_RET_STRUCT_2B;
	  break;
	default:
	  return FFI_BAD_TYPEDEF;
	}
      break;
    default:
      return FFI_BAD_TYPEDEF;
    }
  cif->flags = flags;

  for (i = 0, n = cif->nargs; i < n; i++)
    {
      ffi_type *t = cif->arg_types[i];

      bytes = FFI_ALIGN (bytes, t->alignment);
      bytes += FFI_ALIGN (t->size, FFI_SIZEOF_ARG);
    }
  cif->bytes = bytes;

  return FFI_OK;
}

static ffi_arg
extend_basic_type(void *arg, int type)
{
  switch (type)
    {
    case FFI_TYPE_SINT8:
      return *(SINT8 *)arg;
    case FFI_TYPE_UINT8:
      return *(UINT8 *)arg;
    case FFI_TYPE_SINT16:
      return *(SINT16 *)arg;
    case FFI_TYPE_UINT16:
      return *(UINT16 *)arg;

    case FFI_TYPE_SINT32:
    case FFI_TYPE_UINT32:
    case FFI_TYPE_POINTER:
    case FFI_TYPE_FLOAT:
      return *(UINT32 *)arg;

    default:
      abort();
    }
}

struct call_frame
{
  void *ebp;		/* 0 */
  void *retaddr;	/* 4 */
  void (*fn)(void);	/* 8 */
  int flags;		/* 12 */
  void *rvalue;		/* 16 */
  unsigned regs[3];	/* 20-28 */
};

struct abi_params
{
  int dir;		/* parameter growth direction */
  int static_chain;	/* the static chain register used by gcc */
  int nregs;		/* number of register parameters */
  int regs[3];
};

static const struct abi_params abi_params[FFI_LAST_ABI] = {
  [FFI_SYSV] = { 1, R_ECX, 0 },
  [FFI_THISCALL] = { 1, R_EAX, 1, { R_ECX } },
  [FFI_FASTCALL] = { 1, R_EAX, 2, { R_ECX, R_EDX } },
  [FFI_STDCALL] = { 1, R_ECX, 0 },
  [FFI_PASCAL] = { -1, R_ECX, 0 },
  /* ??? No defined static chain; gcc does not support REGISTER.  */
  [FFI_REGISTER] = { -1, R_ECX, 3, { R_EAX, R_EDX, R_ECX } },
  [FFI_MS_CDECL] = { 1, R_ECX, 0 }
};

#ifdef HAVE_FASTCALL
  #ifdef _MSC_VER
    #define FFI_DECLARE_FASTCALL __fastcall
  #else
    #define FFI_DECLARE_FASTCALL __declspec(fastcall)
  #endif
#else
  #define FFI_DECLARE_FASTCALL
#endif

extern void FFI_DECLARE_FASTCALL ffi_call_i386(struct call_frame *, char *) FFI_HIDDEN;

static void
ffi_call_int (ffi_cif *cif, void (*fn)(void), void *rvalue,
	      void **avalue, void *closure)
{
  size_t rsize, bytes;
  struct call_frame *frame;
  char *stack, *argp;
  ffi_type **arg_types;
  int flags, cabi, i, n, dir, narg_reg;
  const struct abi_params *pabi;

  flags = cif->flags;
  cabi = cif->abi;
  pabi = &abi_params[cabi];
  dir = pabi->dir;

  rsize = 0;
  if (rvalue == NULL)
    {
      switch (flags)
	{
	case X86_RET_FLOAT:
	case X86_RET_DOUBLE:
	case X86_RET_LDOUBLE:
	case X86_RET_STRUCTPOP:
	case X86_RET_STRUCTARG:
	  /* The float cases need to pop the 387 stack.
	     The struct cases need to pass a valid pointer to the callee.  */
	  rsize = cif->rtype->size;
	  break;
	default:
	  /* We can pretend that the callee returns nothing.  */
	  flags = X86_RET_VOID;
	  break;
	}
    }

  bytes = STACK_ALIGN (cif->bytes);
  stack = alloca(bytes + sizeof(*frame) + rsize);
  argp = (dir < 0 ? stack + bytes : stack);
  frame = (struct call_frame *)(stack + bytes);
  if (rsize)
    rvalue = frame + 1;

  frame->fn = fn;
  frame->flags = flags;
  frame->rvalue = rvalue;
  frame->regs[pabi->static_chain] = (unsigned)closure;

  narg_reg = 0;
  switch (flags)
    {
    case X86_RET_STRUCTARG:
      /* The pointer is passed as the first argument.  */
      if (pabi->nregs > 0)
	{
	  frame->regs[pabi->regs[0]] = (unsigned)rvalue;
	  narg_reg = 1;
	  break;
	}
      /* fallthru */
    case X86_RET_STRUCTPOP:
      *(void **)argp = rvalue;
      argp += sizeof(void *);
      break;
    }

  arg_types = cif->arg_types;
  for (i = 0, n = cif->nargs; i < n; i++)
    {
      ffi_type *ty = arg_types[i];
      void *valp = avalue[i];
      size_t z = ty->size;
      int t = ty->type;

      if (z <= FFI_SIZEOF_ARG && t != FFI_TYPE_STRUCT)
        {
	  ffi_arg val = extend_basic_type (valp, t);

	  if (t != FFI_TYPE_FLOAT && narg_reg < pabi->nregs)
	    frame->regs[pabi->regs[narg_reg++]] = val;
	  else if (dir < 0)
	    {
	      argp -= 4;
	      *(ffi_arg *)argp = val;
	    }
	  else
	    {
	      *(ffi_arg *)argp = val;
	      argp += 4;
	    }
	}
      else
	{
	  size_t za = FFI_ALIGN (z, FFI_SIZEOF_ARG);
	  size_t align = FFI_SIZEOF_ARG;

	  /* Issue 434: For thiscall and fastcall, if the paramter passed
	     as 64-bit integer or struct, all following integer paramters
	     will be passed on stack.  */
	  if ((cabi == FFI_THISCALL || cabi == FFI_FASTCALL)
	      && (t == FFI_TYPE_SINT64
		  || t == FFI_TYPE_UINT64
		  || t == FFI_TYPE_STRUCT))
	    narg_reg = 2;

	  /* Alignment rules for arguments are quite complex.  Vectors and
	     structures with 16 byte alignment get it.  Note that long double
	     on Darwin does have 16 byte alignment, and does not get this
	     alignment if passed directly; a structure with a long double
	     inside, however, would get 16 byte alignment.  Since libffi does
	     not support vectors, we need non concern ourselves with other
	     cases.  */
	  if (t == FFI_TYPE_STRUCT && ty->alignment >= 16)
	    align = 16;
	    
	  if (dir < 0)
	    {
	      /* ??? These reverse argument ABIs are probably too old
		 to have cared about alignment.  Someone should check.  */
	      argp -= za;
	      memcpy (argp, valp, z);
	    }
	  else
	    {
	      argp = (char *)FFI_ALIGN (argp, align);
	      memcpy (argp, valp, z);
	      argp += za;
	    }
	}
    }
  FFI_ASSERT (dir > 0 || argp == stack);

  ffi_call_i386 (frame, stack);
}

void
ffi_call (ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue)
{
  ffi_call_int (cif, fn, rvalue, avalue, NULL);
}

void
ffi_call_go (ffi_cif *cif, void (*fn)(void), void *rvalue,
	     void **avalue, void *closure)
{
  ffi_call_int (cif, fn, rvalue, avalue, closure);
}

/** private members **/

void FFI_HIDDEN ffi_closure_i386(void);
void FFI_HIDDEN ffi_closure_STDCALL(void);
void FFI_HIDDEN ffi_closure_REGISTER(void);

struct closure_frame
{
  unsigned rettemp[4];				/* 0 */
  unsigned regs[3];				/* 16-24 */
  ffi_cif *cif;					/* 28 */
  void (*fun)(ffi_cif*,void*,void**,void*);	/* 32 */
  void *user_data;				/* 36 */
};

int FFI_HIDDEN FFI_DECLARE_FASTCALL
ffi_closure_inner (struct closure_frame *frame, char *stack)
{
  ffi_cif *cif = frame->cif;
  int cabi, i, n, flags, dir, narg_reg;
  const struct abi_params *pabi;
  ffi_type **arg_types;
  char *argp;
  void *rvalue;
  void **avalue;

  cabi = cif->abi;
  flags = cif->flags;
  narg_reg = 0;
  rvalue = frame->rettemp;
  pabi = &abi_params[cabi];
  dir = pabi->dir;
  argp = (dir < 0 ? stack + STACK_ALIGN (cif->bytes) : stack);

  switch (flags)
    {
    case X86_RET_STRUCTARG:
      if (pabi->nregs > 0)
	{
	  rvalue = (void *)frame->regs[pabi->regs[0]];
	  narg_reg = 1;
	  frame->rettemp[0] = (unsigned)rvalue;
	  break;
	}
      /* fallthru */
    case X86_RET_STRUCTPOP:
      rvalue = *(void **)argp;
      argp += sizeof(void *);
      frame->rettemp[0] = (unsigned)rvalue;
      break;
    }

  n = cif->nargs;
  avalue = alloca(sizeof(void *) * n);

  arg_types = cif->arg_types;
  for (i = 0; i < n; ++i)
    {
      ffi_type *ty = arg_types[i];
      size_t z = ty->size;
      int t = ty->type;
      void *valp;

      if (z <= FFI_SIZEOF_ARG && t != FFI_TYPE_STRUCT)
	{
	  if (t != FFI_TYPE_FLOAT && narg_reg < pabi->nregs)
	    valp = &frame->regs[pabi->regs[narg_reg++]];
	  else if (dir < 0)
	    {
	      argp -= 4;
	      valp = argp;
	    }
	  else
	    {
	      valp = argp;
	      argp += 4;
	    }
	}
      else
	{
	  size_t za = FFI_ALIGN (z, FFI_SIZEOF_ARG);
	  size_t align = FFI_SIZEOF_ARG;

	  /* See the comment in ffi_call_int.  */
	  if (t == FFI_TYPE_STRUCT && ty->alignment >= 16)
	    align = 16;

	  /* Issue 434: For thiscall and fastcall, if the paramter passed
	     as 64-bit integer or struct, all following integer paramters
	     will be passed on stack.  */
	  if ((cabi == FFI_THISCALL || cabi == FFI_FASTCALL)
	      && (t == FFI_TYPE_SINT64
		  || t == FFI_TYPE_UINT64
		  || t == FFI_TYPE_STRUCT))
	    narg_reg = 2;

	  if (dir < 0)
	    {
	      /* ??? These reverse argument ABIs are probably too old
		 to have cared about alignment.  Someone should check.  */
	      argp -= za;
	      valp = argp;
	    }
	  else
	    {
	      argp = (char *)FFI_ALIGN (argp, align);
	      valp = argp;
	      argp += za;
	    }
	}

      avalue[i] = valp;
    }

  frame->fun (cif, rvalue, avalue, frame->user_data);

  if (cabi == FFI_STDCALL)
    return flags + (cif->bytes << X86_RET_POP_SHIFT);
  else
    return flags;
}

ffi_status
ffi_prep_closure_loc (ffi_closure* closure,
                      ffi_cif* cif,
                      void (*fun)(ffi_cif*,void*,void**,void*),
                      void *user_data,
                      void *codeloc)
{
  char *tramp = closure->tramp;
  void (*dest)(void);
  int op = 0xb8;  /* movl imm, %eax */

  switch (cif->abi)
    {
    case FFI_SYSV:
    case FFI_THISCALL:
    case FFI_FASTCALL:
    case FFI_MS_CDECL:
      dest = ffi_closure_i386;
      break;
    case FFI_STDCALL:
    case FFI_PASCAL:
      dest = ffi_closure_STDCALL;
      break;
    case FFI_REGISTER:
      dest = ffi_closure_REGISTER;
      op = 0x68;  /* pushl imm */
      break;
    default:
      return FFI_BAD_ABI;
    }

  /* movl or pushl immediate.  */
  tramp[0] = op;
  *(void **)(tramp + 1) = codeloc;

  /* jmp dest */
  tramp[5] = 0xe9;
  *(unsigned *)(tramp + 6) = (unsigned)dest - ((unsigned)codeloc + 10);

  closure->cif = cif;
  closure->fun = fun;
  closure->user_data = user_data;

  return FFI_OK;
}

void FFI_HIDDEN ffi_go_closure_EAX(void);
void FFI_HIDDEN ffi_go_closure_ECX(void);
void FFI_HIDDEN ffi_go_closure_STDCALL(void);

ffi_status
ffi_prep_go_closure (ffi_go_closure* closure, ffi_cif* cif,
		     void (*fun)(ffi_cif*,void*,void**,void*))
{
  void (*dest)(void);

  switch (cif->abi)
    {
    case FFI_SYSV:
    case FFI_MS_CDECL:
      dest = ffi_go_closure_ECX;
      break;
    case FFI_THISCALL:
    case FFI_FASTCALL:
      dest = ffi_go_closure_EAX;
      break;
    case FFI_STDCALL:
    case FFI_PASCAL:
      dest = ffi_go_closure_STDCALL;
      break;
    case FFI_REGISTER:
    default:
      return FFI_BAD_ABI;
    }

  closure->tramp = dest;
  closure->cif = cif;
  closure->fun = fun;

  return FFI_OK;
}

/* ------- Native raw API support -------------------------------- */

#if !FFI_NO_RAW_API

void FFI_HIDDEN ffi_closure_raw_SYSV(void);
void FFI_HIDDEN ffi_closure_raw_THISCALL(void);

ffi_status
ffi_prep_raw_closure_loc (ffi_raw_closure *closure,
                          ffi_cif *cif,
                          void (*fun)(ffi_cif*,void*,ffi_raw*,void*),
                          void *user_data,
                          void *codeloc)
{
  char *tramp = closure->tramp;
  void (*dest)(void);
  int i;

  /* We currently don't support certain kinds of arguments for raw
     closures.  This should be implemented by a separate assembly
     language routine, since it would require argument processing,
     something we don't do now for performance.  */
  for (i = cif->nargs-1; i >= 0; i--)
    switch (cif->arg_types[i]->type)
      {
      case FFI_TYPE_STRUCT:
      case FFI_TYPE_LONGDOUBLE:
	return FFI_BAD_TYPEDEF;
      }

  switch (cif->abi)
    {
    case FFI_THISCALL:
      dest = ffi_closure_raw_THISCALL;
      break;
    case FFI_SYSV:
      dest = ffi_closure_raw_SYSV;
      break;
    default:
      return FFI_BAD_ABI;
    }

  /* movl imm, %eax.  */
  tramp[0] = 0xb8;
  *(void **)(tramp + 1) = codeloc;

  /* jmp dest */
  tramp[5] = 0xe9;
  *(unsigned *)(tramp + 6) = (unsigned)dest - ((unsigned)codeloc + 10);

  closure->cif = cif;
  closure->fun = fun;
  closure->user_data = user_data;

  return FFI_OK;
}

void
ffi_raw_call(ffi_cif *cif, void (*fn)(void), void *rvalue, ffi_raw *avalue)
{
  size_t rsize, bytes;
  struct call_frame *frame;
  char *stack, *argp;
  ffi_type **arg_types;
  int flags, cabi, i, n, narg_reg;
  const struct abi_params *pabi;

  flags = cif->flags;
  cabi = cif->abi;
  pabi = &abi_params[cabi];

  rsize = 0;
  if (rvalue == NULL)
    {
      switch (flags)
	{
	case X86_RET_FLOAT:
	case X86_RET_DOUBLE:
	case X86_RET_LDOUBLE:
	case X86_RET_STRUCTPOP:
	case X86_RET_STRUCTARG:
	  /* The float cases need to pop the 387 stack.
	     The struct cases need to pass a valid pointer to the callee.  */
	  rsize = cif->rtype->size;
	  break;
	default:
	  /* We can pretend that the callee returns nothing.  */
	  flags = X86_RET_VOID;
	  break;
	}
    }

  bytes = STACK_ALIGN (cif->bytes);
  argp = stack =
      (void *)((uintptr_t)alloca(bytes + sizeof(*frame) + rsize + 15) & ~16);
  frame = (struct call_frame *)(stack + bytes);
  if (rsize)
    rvalue = frame + 1;

  frame->fn = fn;
  frame->flags = flags;
  frame->rvalue = rvalue;

  narg_reg = 0;
  switch (flags)
    {
    case X86_RET_STRUCTARG:
      /* The pointer is passed as the first argument.  */
      if (pabi->nregs > 0)
	{
	  frame->regs[pabi->regs[0]] = (unsigned)rvalue;
	  narg_reg = 1;
	  break;
	}
      /* fallthru */
    case X86_RET_STRUCTPOP:
      *(void **)argp = rvalue;
      argp += sizeof(void *);
      bytes -= sizeof(void *);
      break;
    }

  arg_types = cif->arg_types;
  for (i = 0, n = cif->nargs; narg_reg < pabi->nregs && i < n; i++)
    {
      ffi_type *ty = arg_types[i];
      size_t z = ty->size;
      int t = ty->type;

      if (z <= FFI_SIZEOF_ARG && t != FFI_TYPE_STRUCT && t != FFI_TYPE_FLOAT)
	{
	  ffi_arg val = extend_basic_type (avalue, t);
	  frame->regs[pabi->regs[narg_reg++]] = val;
	  z = FFI_SIZEOF_ARG;
	}
      else
	{
	  memcpy (argp, avalue, z);
	  z = FFI_ALIGN (z, FFI_SIZEOF_ARG);
	  argp += z;
	}
      avalue += z;
      bytes -= z;
    }
  if (i < n)
    memcpy (argp, avalue, bytes);

  ffi_call_i386 (frame, stack);
}
#endif /* !FFI_NO_RAW_API */
#endif /* __i386__ */
>>>>>>> BRANCH (5dcb74 Move nested_struct3 test to closures directory)
