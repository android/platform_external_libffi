/* -----------------------------------------------------------------------
<<<<<<< HEAD   (1246a0 Merge "Remove redundant NOTICE copied from LICENSE.")
   sysv.S - Copyright (c) 2013  The Written Word, Inc.
	  - Copyright (c) 1996,1998,2001-2003,2005,2008,2010  Red Hat, Inc.
   
   X86 Foreign Function Interface 

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

#ifndef __x86_64__

#define LIBFFI_ASM	
#include <fficonfig.h>
#include <ffi.h>

.text

.globl ffi_prep_args

	.align 4
.globl ffi_call_SYSV
        .type    ffi_call_SYSV,@function

ffi_call_SYSV:
.LFB1:
        pushl %ebp
.LCFI0:
        movl  %esp,%ebp
.LCFI1:
	/* Make room for all of the new args.  */
	movl  16(%ebp),%ecx
	subl  %ecx,%esp

        /* Align the stack pointer to 16-bytes */
        andl  $0xfffffff0, %esp

	movl  %esp,%eax

	/* Place all of the ffi_prep_args in position  */
	pushl 12(%ebp)
	pushl %eax
	call  *8(%ebp)

	/* Return stack to previous state and call the function  */
	addl  $8,%esp	

	call  *28(%ebp)

	/* Load %ecx with the return type code  */
	movl  20(%ebp),%ecx	

	/* Protect %esi.  We're going to pop it in the epilogue.  */
	pushl %esi

	/* If the return value pointer is NULL, assume no return value.  */
	cmpl  $0,24(%ebp)
	jne  0f

	/* Even if there is no space for the return value, we are 
	   obliged to handle floating-point values.  */
	cmpl  $FFI_TYPE_FLOAT,%ecx
	jne   noretval
	fstp  %st(0)

        jmp   epilogue

0:
	call  1f

.Lstore_table:
	.long	noretval-.Lstore_table	/* FFI_TYPE_VOID */
	.long	retint-.Lstore_table	/* FFI_TYPE_INT */
	.long	retfloat-.Lstore_table	/* FFI_TYPE_FLOAT */
	.long	retdouble-.Lstore_table	/* FFI_TYPE_DOUBLE */
	.long	retlongdouble-.Lstore_table	/* FFI_TYPE_LONGDOUBLE */
	.long	retuint8-.Lstore_table	/* FFI_TYPE_UINT8 */
	.long	retsint8-.Lstore_table	/* FFI_TYPE_SINT8 */
	.long	retuint16-.Lstore_table	/* FFI_TYPE_UINT16 */
	.long	retsint16-.Lstore_table	/* FFI_TYPE_SINT16 */
	.long	retint-.Lstore_table	/* FFI_TYPE_UINT32 */
	.long	retint-.Lstore_table	/* FFI_TYPE_SINT32 */
	.long	retint64-.Lstore_table	/* FFI_TYPE_UINT64 */
	.long	retint64-.Lstore_table	/* FFI_TYPE_SINT64 */
	.long	retstruct-.Lstore_table	/* FFI_TYPE_STRUCT */
	.long	retint-.Lstore_table	/* FFI_TYPE_POINTER */

1:
	pop  %esi
	add  (%esi, %ecx, 4), %esi
	jmp  *%esi

	/* Sign/zero extend as appropriate.  */
retsint8:
	movsbl  %al, %eax
	jmp  retint

retsint16:
	movswl  %ax, %eax
	jmp  retint

retuint8:
	movzbl  %al, %eax
	jmp  retint

retuint16:
	movzwl  %ax, %eax
	jmp  retint

retfloat:
	/* Load %ecx with the pointer to storage for the return value  */
	movl  24(%ebp),%ecx	
	fstps (%ecx)
	jmp   epilogue

retdouble:
	/* Load %ecx with the pointer to storage for the return value  */
	movl  24(%ebp),%ecx	
	fstpl (%ecx)
	jmp   epilogue

retlongdouble:
	/* Load %ecx with the pointer to storage for the return value  */
	movl  24(%ebp),%ecx	
	fstpt (%ecx)
	jmp   epilogue
	
retint64:	
	/* Load %ecx with the pointer to storage for the return value  */
	movl  24(%ebp),%ecx	
	movl  %eax,0(%ecx)
	movl  %edx,4(%ecx)
	jmp   epilogue
	
retint:
	/* Load %ecx with the pointer to storage for the return value  */
	movl  24(%ebp),%ecx	
	movl  %eax,0(%ecx)

retstruct:
	/* Nothing to do!  */

noretval:
epilogue:
        popl %esi
        movl %ebp,%esp
        popl %ebp
        ret
.LFE1:
.ffi_call_SYSV_end:
        .size    ffi_call_SYSV,.ffi_call_SYSV_end-ffi_call_SYSV

	.align	4
FFI_HIDDEN (ffi_closure_SYSV)
.globl ffi_closure_SYSV
	.type	ffi_closure_SYSV, @function

ffi_closure_SYSV:
.LFB2:
	pushl	%ebp
.LCFI2:
	movl	%esp, %ebp
.LCFI3:
	subl	$40, %esp
	leal	-24(%ebp), %edx
	movl	%edx, -12(%ebp)	/* resp */
	leal	8(%ebp), %edx
#ifdef __SUNPRO_C
	/* The SUNPRO compiler doesn't support GCC's regparm function
  	   attribute, so we have to pass all three arguments to
	   ffi_closure_SYSV_inner on the stack.  */
	movl	%edx, 8(%esp)	/* args = __builtin_dwarf_cfa () */
	leal	-12(%ebp), %edx
	movl	%edx, 4(%esp)	/* &resp */
	movl    %eax, (%esp)    /* closure */
#else
	movl	%edx, 4(%esp)	/* args = __builtin_dwarf_cfa () */
	leal	-12(%ebp), %edx
	movl	%edx, (%esp)	/* &resp */
#endif
#if defined HAVE_HIDDEN_VISIBILITY_ATTRIBUTE || !defined __PIC__
	call	ffi_closure_SYSV_inner
#else
	movl	%ebx, 8(%esp)
.LCFI7:
	call	1f
1:	popl	%ebx
	addl	$_GLOBAL_OFFSET_TABLE_+[.-1b], %ebx
	call	ffi_closure_SYSV_inner@PLT
	movl	8(%esp), %ebx
#endif
	movl	-12(%ebp), %ecx
	cmpl	$FFI_TYPE_INT, %eax
	je	.Lcls_retint

	/* Handle FFI_TYPE_UINT8, FFI_TYPE_SINT8, FFI_TYPE_UINT16,
	   FFI_TYPE_SINT16, FFI_TYPE_UINT32, FFI_TYPE_SINT32.  */
	cmpl	$FFI_TYPE_UINT64, %eax
	jge	0f
	cmpl	$FFI_TYPE_UINT8, %eax
	jge	.Lcls_retint
	
0:	cmpl	$FFI_TYPE_FLOAT, %eax
	je	.Lcls_retfloat
	cmpl	$FFI_TYPE_DOUBLE, %eax
	je	.Lcls_retdouble
	cmpl	$FFI_TYPE_LONGDOUBLE, %eax
	je	.Lcls_retldouble
	cmpl	$FFI_TYPE_SINT64, %eax
	je	.Lcls_retllong
	cmpl	$FFI_TYPE_STRUCT, %eax
	je	.Lcls_retstruct
.Lcls_epilogue:
	movl	%ebp, %esp
	popl	%ebp
	ret
.Lcls_retint:
	movl	(%ecx), %eax
	jmp	.Lcls_epilogue
.Lcls_retfloat:
	flds	(%ecx)
	jmp	.Lcls_epilogue
.Lcls_retdouble:
	fldl	(%ecx)
	jmp	.Lcls_epilogue
.Lcls_retldouble:
	fldt	(%ecx)
	jmp	.Lcls_epilogue
.Lcls_retllong:
	movl	(%ecx), %eax
	movl	4(%ecx), %edx
	jmp	.Lcls_epilogue
.Lcls_retstruct:
	movl	%ebp, %esp
	popl	%ebp
	ret	$4
.LFE2:
	.size	ffi_closure_SYSV, .-ffi_closure_SYSV

#if !FFI_NO_RAW_API

/* Precalculate for e.g. the Solaris 10/x86 assembler.  */
#if FFI_TRAMPOLINE_SIZE == 10
#define RAW_CLOSURE_CIF_OFFSET 12
#define RAW_CLOSURE_FUN_OFFSET 16
#define RAW_CLOSURE_USER_DATA_OFFSET 20
#elif FFI_TRAMPOLINE_SIZE == 24
#define RAW_CLOSURE_CIF_OFFSET 24
#define RAW_CLOSURE_FUN_OFFSET 28
#define RAW_CLOSURE_USER_DATA_OFFSET 32
#else
#define RAW_CLOSURE_CIF_OFFSET ((FFI_TRAMPOLINE_SIZE + 3) & ~3)
#define RAW_CLOSURE_FUN_OFFSET (RAW_CLOSURE_CIF_OFFSET + 4)
#define RAW_CLOSURE_USER_DATA_OFFSET (RAW_CLOSURE_FUN_OFFSET + 4)
#endif
#define CIF_FLAGS_OFFSET 20

	.align	4
FFI_HIDDEN (ffi_closure_raw_SYSV)
.globl ffi_closure_raw_SYSV
	.type	ffi_closure_raw_SYSV, @function

ffi_closure_raw_SYSV:
.LFB3:
	pushl	%ebp
.LCFI4:
	movl	%esp, %ebp
.LCFI5:
	pushl	%esi
.LCFI6:
	subl	$36, %esp
	movl	RAW_CLOSURE_CIF_OFFSET(%eax), %esi	 /* closure->cif */
	movl	RAW_CLOSURE_USER_DATA_OFFSET(%eax), %edx /* closure->user_data */
	movl	%edx, 12(%esp)	/* user_data */
	leal	8(%ebp), %edx	/* __builtin_dwarf_cfa () */
	movl	%edx, 8(%esp)	/* raw_args */
	leal	-24(%ebp), %edx
	movl	%edx, 4(%esp)	/* &res */
	movl	%esi, (%esp)	/* cif */
	call	*RAW_CLOSURE_FUN_OFFSET(%eax)		 /* closure->fun */
	movl	CIF_FLAGS_OFFSET(%esi), %eax		 /* rtype */
	cmpl	$FFI_TYPE_INT, %eax
	je	.Lrcls_retint

	/* Handle FFI_TYPE_UINT8, FFI_TYPE_SINT8, FFI_TYPE_UINT16,
	   FFI_TYPE_SINT16, FFI_TYPE_UINT32, FFI_TYPE_SINT32.  */
	cmpl	$FFI_TYPE_UINT64, %eax
	jge	0f
	cmpl	$FFI_TYPE_UINT8, %eax
	jge	.Lrcls_retint
0:
	cmpl	$FFI_TYPE_FLOAT, %eax
	je	.Lrcls_retfloat
	cmpl	$FFI_TYPE_DOUBLE, %eax
	je	.Lrcls_retdouble
	cmpl	$FFI_TYPE_LONGDOUBLE, %eax
	je	.Lrcls_retldouble
	cmpl	$FFI_TYPE_SINT64, %eax
	je	.Lrcls_retllong
.Lrcls_epilogue:
	addl	$36, %esp
	popl	%esi
	popl	%ebp
	ret
.Lrcls_retint:
	movl	-24(%ebp), %eax
	jmp	.Lrcls_epilogue
.Lrcls_retfloat:
	flds	-24(%ebp)
	jmp	.Lrcls_epilogue
.Lrcls_retdouble:
	fldl	-24(%ebp)
	jmp	.Lrcls_epilogue
.Lrcls_retldouble:
	fldt	-24(%ebp)
	jmp	.Lrcls_epilogue
.Lrcls_retllong:
	movl	-24(%ebp), %eax
	movl	-20(%ebp), %edx
	jmp	.Lrcls_epilogue
.LFE3:
	.size	ffi_closure_raw_SYSV, .-ffi_closure_raw_SYSV
#endif

#if defined __GNUC__
/* Only emit dwarf unwind info when building with GNU toolchain.  */

#if defined __PIC__
# if defined __sun__ && defined __svr4__
/* 32-bit Solaris 2/x86 uses datarel encoding for PIC.  GNU ld before 2.22
   doesn't correctly sort .eh_frame_hdr with mixed encodings, so match this.  */
#  define FDE_ENCODING		0x30	/* datarel */
#  define FDE_ENCODE(X)		X@GOTOFF
# else
#  define FDE_ENCODING		0x1b	/* pcrel sdata4 */
#  if defined HAVE_AS_X86_PCREL
#   define FDE_ENCODE(X)	X-.
#  else
#   define FDE_ENCODE(X)	X@rel
#  endif
# endif
#else
# define FDE_ENCODING		0	/* absolute */
# define FDE_ENCODE(X)		X
#endif

	.section	.eh_frame,EH_FRAME_FLAGS,@progbits
.Lframe1:
	.long	.LECIE1-.LSCIE1	/* Length of Common Information Entry */
.LSCIE1:
	.long	0x0	/* CIE Identifier Tag */
	.byte	0x1	/* CIE Version */
#ifdef HAVE_AS_ASCII_PSEUDO_OP
#ifdef __PIC__
	.ascii "zR\0"	/* CIE Augmentation */
#else
	.ascii "\0"	/* CIE Augmentation */
#endif
#elif defined HAVE_AS_STRING_PSEUDO_OP
#ifdef __PIC__
	.string "zR"	/* CIE Augmentation */
#else
	.string ""	/* CIE Augmentation */
#endif
#else
#error missing .ascii/.string
#endif
	.byte	0x1	/* .uleb128 0x1; CIE Code Alignment Factor */
	.byte	0x7c	/* .sleb128 -4; CIE Data Alignment Factor */
	.byte	0x8	/* CIE RA Column */
#ifdef __PIC__
	.byte	0x1	/* .uleb128 0x1; Augmentation size */
	.byte	FDE_ENCODING
#endif
	.byte	0xc	/* DW_CFA_def_cfa */
	.byte	0x4	/* .uleb128 0x4 */
	.byte	0x4	/* .uleb128 0x4 */
	.byte	0x88	/* DW_CFA_offset, column 0x8 */
	.byte	0x1	/* .uleb128 0x1 */
	.align 4
.LECIE1:
.LSFDE1:
	.long	.LEFDE1-.LASFDE1	/* FDE Length */
.LASFDE1:
	.long	.LASFDE1-.Lframe1	/* FDE CIE offset */
	.long	FDE_ENCODE(.LFB1)	/* FDE initial location */
	.long	.LFE1-.LFB1		/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI0-.LFB1
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 */
	.byte	0x2	/* .uleb128 0x2 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI1-.LCFI0
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.byte	0x5	/* .uleb128 0x5 */
	.align 4
.LEFDE1:
.LSFDE2:
	.long	.LEFDE2-.LASFDE2	/* FDE Length */
.LASFDE2:
	.long	.LASFDE2-.Lframe1	/* FDE CIE offset */
	.long	FDE_ENCODE(.LFB2)	/* FDE initial location */
	.long	.LFE2-.LFB2		/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI2-.LFB2
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 */
	.byte	0x2	/* .uleb128 0x2 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI3-.LCFI2
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.byte	0x5	/* .uleb128 0x5 */
#if !defined HAVE_HIDDEN_VISIBILITY_ATTRIBUTE && defined __PIC__
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI7-.LCFI3
	.byte	0x83	/* DW_CFA_offset, column 0x3 */
	.byte	0xa	/* .uleb128 0xa */
#endif
	.align 4
.LEFDE2:

#if !FFI_NO_RAW_API

.LSFDE3:
	.long	.LEFDE3-.LASFDE3	/* FDE Length */
.LASFDE3:
	.long	.LASFDE3-.Lframe1	/* FDE CIE offset */
	.long	FDE_ENCODE(.LFB3)	/* FDE initial location */
	.long	.LFE3-.LFB3		/* FDE address range */
#ifdef __PIC__
	.byte	0x0	/* .uleb128 0x0; Augmentation size */
#endif
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI4-.LFB3
	.byte	0xe	/* DW_CFA_def_cfa_offset */
	.byte	0x8	/* .uleb128 0x8 */
	.byte	0x85	/* DW_CFA_offset, column 0x5 */
	.byte	0x2	/* .uleb128 0x2 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI5-.LCFI4
	.byte	0xd	/* DW_CFA_def_cfa_register */
	.byte	0x5	/* .uleb128 0x5 */
	.byte	0x4	/* DW_CFA_advance_loc4 */
	.long	.LCFI6-.LCFI5
	.byte	0x86	/* DW_CFA_offset, column 0x6 */
	.byte	0x3	/* .uleb128 0x3 */
	.align 4
.LEFDE3:

#endif
#endif

#endif /* ifndef __x86_64__ */
=======
   sysv.S - Copyright (c) 2017  Anthony Green
          - Copyright (c) 2013  The Written Word, Inc.
          - Copyright (c) 1996,1998,2001-2003,2005,2008,2010  Red Hat, Inc.
   
   X86 Foreign Function Interface 

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

#ifdef __i386__
#ifndef _MSC_VER

#define LIBFFI_ASM	
#include <fficonfig.h>
#include <ffi.h>
#include "internal.h"

#define C2(X, Y)  X ## Y
#define C1(X, Y)  C2(X, Y)
#ifdef __USER_LABEL_PREFIX__
# define C(X)     C1(__USER_LABEL_PREFIX__, X)
#else
# define C(X)     X
#endif

#ifdef X86_DARWIN
# define L(X)     C1(L, X)
#else
# define L(X)     C1(.L, X)
#endif

#ifdef __ELF__
# define ENDF(X)  .type	X,@function; .size X, . - X
#else
# define ENDF(X)
#endif

/* Handle win32 fastcall name mangling.  */
#ifdef X86_WIN32
# define ffi_call_i386		@ffi_call_i386@8
# define ffi_closure_inner	@ffi_closure_inner@8
#else
# define ffi_call_i386		C(ffi_call_i386)
# define ffi_closure_inner	C(ffi_closure_inner)
#endif

/* This macro allows the safe creation of jump tables without an
   actual table.  The entry points into the table are all 8 bytes.
   The use of ORG asserts that we're at the correct location.  */
/* ??? The clang assembler doesn't handle .org with symbolic expressions.  */
#if defined(__clang__) || defined(__APPLE__) || (defined (__sun__) && defined(__svr4__))
# define E(BASE, X)	.balign 8
#else
# define E(BASE, X)	.balign 8; .org BASE + X * 8
#endif

	.text
	.balign	16
	.globl	ffi_call_i386
	FFI_HIDDEN(ffi_call_i386)

/* This is declared as

   void ffi_call_i386(struct call_frame *frame, char *argp)
        __attribute__((fastcall));

   Thus the arguments are present in

        ecx: frame
        edx: argp
*/

ffi_call_i386:
L(UW0):
	# cfi_startproc
#if !HAVE_FASTCALL
	movl	4(%esp), %ecx
	movl	8(%esp), %edx
#endif
	movl	(%esp), %eax		/* move the return address */
	movl	%ebp, (%ecx)		/* store %ebp into local frame */
	movl	%eax, 4(%ecx)		/* store retaddr into local frame */

	/* New stack frame based off ebp.  This is a itty bit of unwind
	   trickery in that the CFA *has* changed.  There is no easy way
	   to describe it correctly on entry to the function.  Fortunately,
	   it doesn't matter too much since at all points we can correctly
	   unwind back to ffi_call.  Note that the location to which we
	   moved the return address is (the new) CFA-4, so from the
	   perspective of the unwind info, it hasn't moved.  */
	movl	%ecx, %ebp
L(UW1):
	# cfi_def_cfa(%ebp, 8)
	# cfi_rel_offset(%ebp, 0)

	movl	%edx, %esp		/* set outgoing argument stack */
	movl	20+R_EAX*4(%ebp), %eax	/* set register arguments */
	movl	20+R_EDX*4(%ebp), %edx
	movl	20+R_ECX*4(%ebp), %ecx

	call	*8(%ebp)

	movl	12(%ebp), %ecx		/* load return type code */
	movl	%ebx, 8(%ebp)		/* preserve %ebx */
L(UW2):
	# cfi_rel_offset(%ebx, 8)

	andl	$X86_RET_TYPE_MASK, %ecx
#ifdef __PIC__
	call	C(__x86.get_pc_thunk.bx)
L(pc1):
	leal	L(store_table)-L(pc1)(%ebx, %ecx, 8), %ebx
#else
	leal	L(store_table)(,%ecx, 8), %ebx
#endif
	movl	16(%ebp), %ecx		/* load result address */
	jmp	*%ebx

	.balign	8
L(store_table):
E(L(store_table), X86_RET_FLOAT)
	fstps	(%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_DOUBLE)
	fstpl	(%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_LDOUBLE)
	fstpt	(%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_SINT8)
	movsbl	%al, %eax
	mov	%eax, (%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_SINT16)
	movswl	%ax, %eax
	mov	%eax, (%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_UINT8)
	movzbl	%al, %eax
	mov	%eax, (%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_UINT16)
	movzwl	%ax, %eax
	mov	%eax, (%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_INT64)
	movl	%edx, 4(%ecx)
	/* fallthru */
E(L(store_table), X86_RET_INT32)
	movl	%eax, (%ecx)
	/* fallthru */
E(L(store_table), X86_RET_VOID)
L(e1):
	movl	8(%ebp), %ebx
	movl	%ebp, %esp
	popl	%ebp
L(UW3):
	# cfi_remember_state
	# cfi_def_cfa(%esp, 4)
	# cfi_restore(%ebx)
	# cfi_restore(%ebp)
	ret
L(UW4):
	# cfi_restore_state

E(L(store_table), X86_RET_STRUCTPOP)
	jmp	L(e1)
E(L(store_table), X86_RET_STRUCTARG)
	jmp	L(e1)
E(L(store_table), X86_RET_STRUCT_1B)
	movb	%al, (%ecx)
	jmp	L(e1)
E(L(store_table), X86_RET_STRUCT_2B)
	movw	%ax, (%ecx)
	jmp	L(e1)

	/* Fill out the table so that bad values are predictable.  */
E(L(store_table), X86_RET_UNUSED14)
	ud2
E(L(store_table), X86_RET_UNUSED15)
	ud2

L(UW5):
	# cfi_endproc
ENDF(ffi_call_i386)

/* The inner helper is declared as

   void ffi_closure_inner(struct closure_frame *frame, char *argp)
	__attribute_((fastcall))

   Thus the arguments are placed in

	ecx:	frame
	edx:	argp
*/

/* Macros to help setting up the closure_data structure.  */

#if HAVE_FASTCALL
# define closure_FS	(40 + 4)
# define closure_CF	0
#else
# define closure_FS	(8 + 40 + 12)
# define closure_CF	8
#endif

#define FFI_CLOSURE_SAVE_REGS		\
	movl	%eax, closure_CF+16+R_EAX*4(%esp);	\
	movl	%edx, closure_CF+16+R_EDX*4(%esp);	\
	movl	%ecx, closure_CF+16+R_ECX*4(%esp)

#define FFI_CLOSURE_COPY_TRAMP_DATA					\
	movl	FFI_TRAMPOLINE_SIZE(%eax), %edx;	/* copy cif */	\
	movl	FFI_TRAMPOLINE_SIZE+4(%eax), %ecx;	/* copy fun */	\
	movl	FFI_TRAMPOLINE_SIZE+8(%eax), %eax;	/* copy user_data */ \
	movl	%edx, closure_CF+28(%esp);				\
	movl	%ecx, closure_CF+32(%esp);				\
	movl	%eax, closure_CF+36(%esp)

#if HAVE_FASTCALL
# define FFI_CLOSURE_PREP_CALL						\
	movl	%esp, %ecx;			/* load closure_data */	\
	leal	closure_FS+4(%esp), %edx;	/* load incoming stack */
#else
# define FFI_CLOSURE_PREP_CALL						\
	leal	closure_CF(%esp), %ecx;		/* load closure_data */	\
	leal	closure_FS+4(%esp), %edx;	/* load incoming stack */ \
	movl	%ecx, (%esp);						\
	movl	%edx, 4(%esp)
#endif

#define FFI_CLOSURE_CALL_INNER(UWN) \
	call	ffi_closure_inner

#define FFI_CLOSURE_MASK_AND_JUMP(N, UW)				\
	andl	$X86_RET_TYPE_MASK, %eax;				\
	leal	L(C1(load_table,N))(, %eax, 8), %edx;			\
	movl	closure_CF(%esp), %eax;		/* optimiztic load */	\
	jmp	*%edx

#ifdef __PIC__
# if defined X86_DARWIN || defined HAVE_HIDDEN_VISIBILITY_ATTRIBUTE
#  undef FFI_CLOSURE_MASK_AND_JUMP
#  define FFI_CLOSURE_MASK_AND_JUMP(N, UW)				\
	andl	$X86_RET_TYPE_MASK, %eax;				\
	call	C(__x86.get_pc_thunk.dx);				\
L(C1(pc,N)):								\
	leal	L(C1(load_table,N))-L(C1(pc,N))(%edx, %eax, 8), %edx;	\
	movl	closure_CF(%esp), %eax;		/* optimiztic load */	\
	jmp	*%edx
# else
#  define FFI_CLOSURE_CALL_INNER_SAVE_EBX
#  undef FFI_CLOSURE_CALL_INNER
#  define FFI_CLOSURE_CALL_INNER(UWN)					\
	movl	%ebx, 40(%esp);			/* save ebx */		\
L(C1(UW,UWN)):								\
	/* cfi_rel_offset(%ebx, 40); */					\
	call	C(__x86.get_pc_thunk.bx);	/* load got register */	\
	addl	$C(_GLOBAL_OFFSET_TABLE_), %ebx;			\
	call	ffi_closure_inner@PLT
#  undef FFI_CLOSURE_MASK_AND_JUMP
#  define FFI_CLOSURE_MASK_AND_JUMP(N, UWN)				\
	andl	$X86_RET_TYPE_MASK, %eax;				\
	leal	L(C1(load_table,N))@GOTOFF(%ebx, %eax, 8), %edx;	\
	movl	40(%esp), %ebx;			/* restore ebx */	\
L(C1(UW,UWN)):								\
	/* cfi_restore(%ebx); */					\
	movl	closure_CF(%esp), %eax;		/* optimiztic load */	\
	jmp	*%edx
# endif /* DARWIN || HIDDEN */
#endif /* __PIC__ */

	.balign	16
	.globl	C(ffi_go_closure_EAX)
	FFI_HIDDEN(C(ffi_go_closure_EAX))
C(ffi_go_closure_EAX):
L(UW6):
	# cfi_startproc
	subl	$closure_FS, %esp
L(UW7):
	# cfi_def_cfa_offset(closure_FS + 4)
	FFI_CLOSURE_SAVE_REGS
	movl	4(%eax), %edx			/* copy cif */
	movl	8(%eax), %ecx			/* copy fun */
	movl	%edx, closure_CF+28(%esp)
	movl	%ecx, closure_CF+32(%esp)
	movl	%eax, closure_CF+36(%esp)	/* closure is user_data */
	jmp	L(do_closure_i386)
L(UW8):
	# cfi_endproc
ENDF(C(ffi_go_closure_EAX))

	.balign	16
	.globl	C(ffi_go_closure_ECX)
	FFI_HIDDEN(C(ffi_go_closure_ECX))
C(ffi_go_closure_ECX):
L(UW9):
	# cfi_startproc
	subl	$closure_FS, %esp
L(UW10):
	# cfi_def_cfa_offset(closure_FS + 4)
	FFI_CLOSURE_SAVE_REGS
	movl	4(%ecx), %edx			/* copy cif */
	movl	8(%ecx), %eax			/* copy fun */
	movl	%edx, closure_CF+28(%esp)
	movl	%eax, closure_CF+32(%esp)
	movl	%ecx, closure_CF+36(%esp)	/* closure is user_data */
	jmp	L(do_closure_i386)
L(UW11):
	# cfi_endproc
ENDF(C(ffi_go_closure_ECX))

/* The closure entry points are reached from the ffi_closure trampoline.
   On entry, %eax contains the address of the ffi_closure.  */

	.balign	16
	.globl	C(ffi_closure_i386)
	FFI_HIDDEN(C(ffi_closure_i386))

C(ffi_closure_i386):
L(UW12):
	# cfi_startproc
	subl	$closure_FS, %esp
L(UW13):
	# cfi_def_cfa_offset(closure_FS + 4)

	FFI_CLOSURE_SAVE_REGS
	FFI_CLOSURE_COPY_TRAMP_DATA

	/* Entry point from preceeding Go closures.  */
L(do_closure_i386):

	FFI_CLOSURE_PREP_CALL
	FFI_CLOSURE_CALL_INNER(14)
	FFI_CLOSURE_MASK_AND_JUMP(2, 15)

	.balign	8
L(load_table2):
E(L(load_table2), X86_RET_FLOAT)
	flds	closure_CF(%esp)
	jmp	L(e2)
E(L(load_table2), X86_RET_DOUBLE)
	fldl	closure_CF(%esp)
	jmp	L(e2)
E(L(load_table2), X86_RET_LDOUBLE)
	fldt	closure_CF(%esp)
	jmp	L(e2)
E(L(load_table2), X86_RET_SINT8)
	movsbl	%al, %eax
	jmp	L(e2)
E(L(load_table2), X86_RET_SINT16)
	movswl	%ax, %eax
	jmp	L(e2)
E(L(load_table2), X86_RET_UINT8)
	movzbl	%al, %eax
	jmp	L(e2)
E(L(load_table2), X86_RET_UINT16)
	movzwl	%ax, %eax
	jmp	L(e2)
E(L(load_table2), X86_RET_INT64)
	movl	closure_CF+4(%esp), %edx
	jmp	L(e2)
E(L(load_table2), X86_RET_INT32)
	nop
	/* fallthru */
E(L(load_table2), X86_RET_VOID)
L(e2):
	addl	$closure_FS, %esp
L(UW16):
	# cfi_adjust_cfa_offset(-closure_FS)
	ret
L(UW17):
	# cfi_adjust_cfa_offset(closure_FS)
E(L(load_table2), X86_RET_STRUCTPOP)
	addl	$closure_FS, %esp
L(UW18):
	# cfi_adjust_cfa_offset(-closure_FS)
	ret	$4
L(UW19):
	# cfi_adjust_cfa_offset(closure_FS)
E(L(load_table2), X86_RET_STRUCTARG)
	jmp	L(e2)
E(L(load_table2), X86_RET_STRUCT_1B)
	movzbl	%al, %eax
	jmp	L(e2)
E(L(load_table2), X86_RET_STRUCT_2B)
	movzwl	%ax, %eax
	jmp	L(e2)

	/* Fill out the table so that bad values are predictable.  */
E(L(load_table2), X86_RET_UNUSED14)
	ud2
E(L(load_table2), X86_RET_UNUSED15)
	ud2

L(UW20):
	# cfi_endproc
ENDF(C(ffi_closure_i386))

	.balign	16
	.globl	C(ffi_go_closure_STDCALL)
	FFI_HIDDEN(C(ffi_go_closure_STDCALL))
C(ffi_go_closure_STDCALL):
L(UW21):
	# cfi_startproc
	subl	$closure_FS, %esp
L(UW22):
	# cfi_def_cfa_offset(closure_FS + 4)
	FFI_CLOSURE_SAVE_REGS
	movl	4(%ecx), %edx			/* copy cif */
	movl	8(%ecx), %eax			/* copy fun */
	movl	%edx, closure_CF+28(%esp)
	movl	%eax, closure_CF+32(%esp)
	movl	%ecx, closure_CF+36(%esp)	/* closure is user_data */
	jmp	L(do_closure_STDCALL)
L(UW23):
	# cfi_endproc
ENDF(C(ffi_go_closure_STDCALL))

/* For REGISTER, we have no available parameter registers, and so we
   enter here having pushed the closure onto the stack.  */

	.balign	16
	.globl	C(ffi_closure_REGISTER)
	FFI_HIDDEN(C(ffi_closure_REGISTER))
C(ffi_closure_REGISTER):
L(UW24):
	# cfi_startproc
	# cfi_def_cfa(%esp, 8)
	# cfi_offset(%eip, -8)
	subl	$closure_FS-4, %esp
L(UW25):
	# cfi_def_cfa_offset(closure_FS + 4)
	FFI_CLOSURE_SAVE_REGS
	movl	closure_FS-4(%esp), %ecx	/* load retaddr */
	movl	closure_FS(%esp), %eax		/* load closure */
	movl	%ecx, closure_FS(%esp)		/* move retaddr */
	jmp	L(do_closure_REGISTER)
L(UW26):
	# cfi_endproc
ENDF(C(ffi_closure_REGISTER))

/* For STDCALL (and others), we need to pop N bytes of arguments off
   the stack following the closure.  The amount needing to be popped
   is returned to us from ffi_closure_inner.  */

	.balign	16
	.globl	C(ffi_closure_STDCALL)
	FFI_HIDDEN(C(ffi_closure_STDCALL))
C(ffi_closure_STDCALL):
L(UW27):
	# cfi_startproc
	subl	$closure_FS, %esp
L(UW28):
	# cfi_def_cfa_offset(closure_FS + 4)

	FFI_CLOSURE_SAVE_REGS

	/* Entry point from ffi_closure_REGISTER.  */
L(do_closure_REGISTER):

	FFI_CLOSURE_COPY_TRAMP_DATA

	/* Entry point from preceeding Go closure.  */
L(do_closure_STDCALL):

	FFI_CLOSURE_PREP_CALL
	FFI_CLOSURE_CALL_INNER(29)

	movl	%eax, %ecx
	shrl	$X86_RET_POP_SHIFT, %ecx	/* isolate pop count */
	leal	closure_FS(%esp, %ecx), %ecx	/* compute popped esp */
	movl	closure_FS(%esp), %edx		/* move return address */
	movl	%edx, (%ecx)

	/* From this point on, the value of %esp upon return is %ecx+4,
	   and we've copied the return address to %ecx to make return easy.
	   There's no point in representing this in the unwind info, as
	   there is always a window between the mov and the ret which
	   will be wrong from one point of view or another.  */

	FFI_CLOSURE_MASK_AND_JUMP(3, 30)

	.balign	8
L(load_table3):
E(L(load_table3), X86_RET_FLOAT)
	flds    closure_CF(%esp)
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_DOUBLE)
	fldl    closure_CF(%esp)
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_LDOUBLE)
	fldt    closure_CF(%esp)
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_SINT8)
	movsbl  %al, %eax
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_SINT16)
	movswl  %ax, %eax
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_UINT8)
	movzbl  %al, %eax
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_UINT16)
	movzwl  %ax, %eax
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_INT64)
	movl	closure_CF+4(%esp), %edx
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_INT32)
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_VOID)
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_STRUCTPOP)
	movl    %ecx, %esp
	ret
E(L(load_table3), X86_RET_STRUCTARG)
	movl	%ecx, %esp
	ret
E(L(load_table3), X86_RET_STRUCT_1B)
	movzbl	%al, %eax
	movl	%ecx, %esp
	ret
E(L(load_table3), X86_RET_STRUCT_2B)
	movzwl	%ax, %eax
	movl	%ecx, %esp
	ret

	/* Fill out the table so that bad values are predictable.  */
E(L(load_table3), X86_RET_UNUSED14)
	ud2
E(L(load_table3), X86_RET_UNUSED15)
	ud2

L(UW31):
	# cfi_endproc
ENDF(C(ffi_closure_STDCALL))

#if !FFI_NO_RAW_API

#define raw_closure_S_FS	(16+16+12)

	.balign	16
	.globl	C(ffi_closure_raw_SYSV)
	FFI_HIDDEN(C(ffi_closure_raw_SYSV))
C(ffi_closure_raw_SYSV):
L(UW32):
	# cfi_startproc
	subl	$raw_closure_S_FS, %esp
L(UW33):
	# cfi_def_cfa_offset(raw_closure_S_FS + 4)
	movl	%ebx, raw_closure_S_FS-4(%esp)
L(UW34):
	# cfi_rel_offset(%ebx, raw_closure_S_FS-4)

	movl	FFI_TRAMPOLINE_SIZE+8(%eax), %edx	/* load cl->user_data */
	movl	%edx, 12(%esp)
	leal	raw_closure_S_FS+4(%esp), %edx		/* load raw_args */
	movl	%edx, 8(%esp)
	leal	16(%esp), %edx				/* load &res */
	movl	%edx, 4(%esp)
	movl	FFI_TRAMPOLINE_SIZE(%eax), %ebx		/* load cl->cif */
	movl	%ebx, (%esp)
	call	*FFI_TRAMPOLINE_SIZE+4(%eax)		/* call cl->fun */

	movl	20(%ebx), %eax				/* load cif->flags */
	andl	$X86_RET_TYPE_MASK, %eax
#ifdef __PIC__
	call	C(__x86.get_pc_thunk.bx)
L(pc4):
	leal	L(load_table4)-L(pc4)(%ebx, %eax, 8), %ecx
#else
	leal	L(load_table4)(,%eax, 8), %ecx
#endif
	movl	raw_closure_S_FS-4(%esp), %ebx
L(UW35):
	# cfi_restore(%ebx)
	movl	16(%esp), %eax				/* Optimistic load */
	jmp	*%ecx

	.balign	8
L(load_table4):
E(L(load_table4), X86_RET_FLOAT)
	flds	16(%esp)
	jmp	L(e4)
E(L(load_table4), X86_RET_DOUBLE)
	fldl	16(%esp)
	jmp	L(e4)
E(L(load_table4), X86_RET_LDOUBLE)
	fldt	16(%esp)
	jmp	L(e4)
E(L(load_table4), X86_RET_SINT8)
	movsbl	%al, %eax
	jmp	L(e4)
E(L(load_table4), X86_RET_SINT16)
	movswl	%ax, %eax
	jmp	L(e4)
E(L(load_table4), X86_RET_UINT8)
	movzbl	%al, %eax
	jmp	L(e4)
E(L(load_table4), X86_RET_UINT16)
	movzwl	%ax, %eax
	jmp	L(e4)
E(L(load_table4), X86_RET_INT64)
	movl	16+4(%esp), %edx
	jmp	L(e4)
E(L(load_table4), X86_RET_INT32)
	nop
	/* fallthru */
E(L(load_table4), X86_RET_VOID)
L(e4):
	addl	$raw_closure_S_FS, %esp
L(UW36):
	# cfi_adjust_cfa_offset(-raw_closure_S_FS)
	ret
L(UW37):
	# cfi_adjust_cfa_offset(raw_closure_S_FS)
E(L(load_table4), X86_RET_STRUCTPOP)
	addl	$raw_closure_S_FS, %esp
L(UW38):
	# cfi_adjust_cfa_offset(-raw_closure_S_FS)
	ret	$4
L(UW39):
	# cfi_adjust_cfa_offset(raw_closure_S_FS)
E(L(load_table4), X86_RET_STRUCTARG)
	jmp	L(e4)
E(L(load_table4), X86_RET_STRUCT_1B)
	movzbl	%al, %eax
	jmp	L(e4)
E(L(load_table4), X86_RET_STRUCT_2B)
	movzwl	%ax, %eax
	jmp	L(e4)

	/* Fill out the table so that bad values are predictable.  */
E(L(load_table4), X86_RET_UNUSED14)
	ud2
E(L(load_table4), X86_RET_UNUSED15)
	ud2

L(UW40):
	# cfi_endproc
ENDF(C(ffi_closure_raw_SYSV))

#define raw_closure_T_FS	(16+16+8)

	.balign	16
	.globl	C(ffi_closure_raw_THISCALL)
	FFI_HIDDEN(C(ffi_closure_raw_THISCALL))
C(ffi_closure_raw_THISCALL):
L(UW41):
	# cfi_startproc
	/* Rearrange the stack such that %ecx is the first argument.
	   This means moving the return address.  */
	popl	%edx
L(UW42):
	# cfi_def_cfa_offset(0)
	# cfi_register(%eip, %edx)
	pushl	%ecx
L(UW43):
	# cfi_adjust_cfa_offset(4)
	pushl	%edx
L(UW44):
	# cfi_adjust_cfa_offset(4)
	# cfi_rel_offset(%eip, 0)
	subl	$raw_closure_T_FS, %esp
L(UW45):
	# cfi_adjust_cfa_offset(raw_closure_T_FS)
	movl	%ebx, raw_closure_T_FS-4(%esp)
L(UW46):
	# cfi_rel_offset(%ebx, raw_closure_T_FS-4)

	movl	FFI_TRAMPOLINE_SIZE+8(%eax), %edx	/* load cl->user_data */
	movl	%edx, 12(%esp)
	leal	raw_closure_T_FS+4(%esp), %edx		/* load raw_args */
	movl	%edx, 8(%esp)
	leal	16(%esp), %edx				/* load &res */
	movl	%edx, 4(%esp)
	movl	FFI_TRAMPOLINE_SIZE(%eax), %ebx		/* load cl->cif */
	movl	%ebx, (%esp)
	call	*FFI_TRAMPOLINE_SIZE+4(%eax)		/* call cl->fun */

	movl	20(%ebx), %eax				/* load cif->flags */
	andl	$X86_RET_TYPE_MASK, %eax
#ifdef __PIC__
	call	C(__x86.get_pc_thunk.bx)
L(pc5):
	leal	L(load_table5)-L(pc5)(%ebx, %eax, 8), %ecx
#else
	leal	L(load_table5)(,%eax, 8), %ecx
#endif
	movl	raw_closure_T_FS-4(%esp), %ebx
L(UW47):
	# cfi_restore(%ebx)
	movl	16(%esp), %eax				/* Optimistic load */
	jmp	*%ecx

	.balign	8
L(load_table5):
E(L(load_table5), X86_RET_FLOAT)
	flds	16(%esp)
	jmp	L(e5)
E(L(load_table5), X86_RET_DOUBLE)
	fldl	16(%esp)
	jmp	L(e5)
E(L(load_table5), X86_RET_LDOUBLE)
	fldt	16(%esp)
	jmp	L(e5)
E(L(load_table5), X86_RET_SINT8)
	movsbl	%al, %eax
	jmp	L(e5)
E(L(load_table5), X86_RET_SINT16)
	movswl	%ax, %eax
	jmp	L(e5)
E(L(load_table5), X86_RET_UINT8)
	movzbl	%al, %eax
	jmp	L(e5)
E(L(load_table5), X86_RET_UINT16)
	movzwl	%ax, %eax
	jmp	L(e5)
E(L(load_table5), X86_RET_INT64)
	movl	16+4(%esp), %edx
	jmp	L(e5)
E(L(load_table5), X86_RET_INT32)
	nop
	/* fallthru */
E(L(load_table5), X86_RET_VOID)
L(e5):
	addl	$raw_closure_T_FS, %esp
L(UW48):
	# cfi_adjust_cfa_offset(-raw_closure_T_FS)
	/* Remove the extra %ecx argument we pushed.  */
	ret	$4
L(UW49):
	# cfi_adjust_cfa_offset(raw_closure_T_FS)
E(L(load_table5), X86_RET_STRUCTPOP)
	addl	$raw_closure_T_FS, %esp
L(UW50):
	# cfi_adjust_cfa_offset(-raw_closure_T_FS)
	ret	$8
L(UW51):
	# cfi_adjust_cfa_offset(raw_closure_T_FS)
E(L(load_table5), X86_RET_STRUCTARG)
	jmp	L(e5)
E(L(load_table5), X86_RET_STRUCT_1B)
	movzbl	%al, %eax
	jmp	L(e5)
E(L(load_table5), X86_RET_STRUCT_2B)
	movzwl	%ax, %eax
	jmp	L(e5)

	/* Fill out the table so that bad values are predictable.  */
E(L(load_table5), X86_RET_UNUSED14)
	ud2
E(L(load_table5), X86_RET_UNUSED15)
	ud2

L(UW52):
	# cfi_endproc
ENDF(C(ffi_closure_raw_THISCALL))

#endif /* !FFI_NO_RAW_API */

#ifdef X86_DARWIN
# define COMDAT(X)							\
        .section __TEXT,__text,coalesced,pure_instructions;		\
        .weak_definition X;						\
        FFI_HIDDEN(X)
#elif defined __ELF__ && !(defined(__sun__) && defined(__svr4__))
# define COMDAT(X)							\
	.section .text.X,"axG",@progbits,X,comdat;			\
	.globl	X;							\
	FFI_HIDDEN(X)
#else
# define COMDAT(X)
#endif

#if defined(__PIC__)
	COMDAT(C(__x86.get_pc_thunk.bx))
C(__x86.get_pc_thunk.bx):
	movl	(%esp), %ebx
	ret
ENDF(C(__x86.get_pc_thunk.bx))
# if defined X86_DARWIN || defined HAVE_HIDDEN_VISIBILITY_ATTRIBUTE
	COMDAT(C(__x86.get_pc_thunk.dx))
C(__x86.get_pc_thunk.dx):
	movl	(%esp), %edx
	ret
ENDF(C(__x86.get_pc_thunk.dx))
#endif /* DARWIN || HIDDEN */
#endif /* __PIC__ */

/* Sadly, OSX cctools-as doesn't understand .cfi directives at all.  */

#ifdef __APPLE__
.section __TEXT,__eh_frame,coalesced,no_toc+strip_static_syms+live_support
EHFrame0:
#elif defined(X86_WIN32)
.section .eh_frame,"r"
#elif defined(HAVE_AS_X86_64_UNWIND_SECTION_TYPE)
.section .eh_frame,EH_FRAME_FLAGS,@unwind
#else
.section .eh_frame,EH_FRAME_FLAGS,@progbits
#endif

#ifdef HAVE_AS_X86_PCREL
# define PCREL(X)	X - .
#else
# define PCREL(X)	X@rel
#endif

/* Simplify advancing between labels.  Assume DW_CFA_advance_loc1 fits.  */
#define ADV(N, P)	.byte 2, L(N)-L(P)

	.balign 4
L(CIE):
	.set	L(set0),L(ECIE)-L(SCIE)
	.long	L(set0)			/* CIE Length */
L(SCIE):
	.long	0			/* CIE Identifier Tag */
	.byte	1			/* CIE Version */
	.ascii	"zR\0"			/* CIE Augmentation */
	.byte	1			/* CIE Code Alignment Factor */
	.byte	0x7c			/* CIE Data Alignment Factor */
	.byte	0x8			/* CIE RA Column */
	.byte	1			/* Augmentation size */
	.byte	0x1b			/* FDE Encoding (pcrel sdata4) */
	.byte	0xc, 4, 4		/* DW_CFA_def_cfa, %esp offset 4 */
	.byte	0x80+8, 1		/* DW_CFA_offset, %eip offset 1*-4 */
	.balign 4
L(ECIE):

	.set	L(set1),L(EFDE1)-L(SFDE1)
	.long	L(set1)			/* FDE Length */
L(SFDE1):
	.long	L(SFDE1)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW0))		/* Initial location */
	.long	L(UW5)-L(UW0)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW1, UW0)
	.byte	0xc, 5, 8		/* DW_CFA_def_cfa, %ebp 8 */
	.byte	0x80+5, 2		/* DW_CFA_offset, %ebp 2*-4 */
	ADV(UW2, UW1)
	.byte	0x80+3, 0		/* DW_CFA_offset, %ebx 0*-4 */
	ADV(UW3, UW2)
	.byte	0xa			/* DW_CFA_remember_state */
	.byte	0xc, 4, 4		/* DW_CFA_def_cfa, %esp 4 */
	.byte	0xc0+3			/* DW_CFA_restore, %ebx */
	.byte	0xc0+5			/* DW_CFA_restore, %ebp */
	ADV(UW4, UW3)
	.byte	0xb			/* DW_CFA_restore_state */
	.balign	4
L(EFDE1):

	.set	L(set2),L(EFDE2)-L(SFDE2)
	.long	L(set2)			/* FDE Length */
L(SFDE2):
	.long	L(SFDE2)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW6))		/* Initial location */
	.long	L(UW8)-L(UW6)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW7, UW6)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE2):

	.set	L(set3),L(EFDE3)-L(SFDE3)
	.long	L(set3)			/* FDE Length */
L(SFDE3):
	.long	L(SFDE3)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW9))		/* Initial location */
	.long	L(UW11)-L(UW9)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW10, UW9)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE3):

	.set	L(set4),L(EFDE4)-L(SFDE4)
	.long	L(set4)			/* FDE Length */
L(SFDE4):
	.long	L(SFDE4)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW12))		/* Initial location */
	.long	L(UW20)-L(UW12)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW13, UW12)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
#ifdef FFI_CLOSURE_CALL_INNER_SAVE_EBX
	ADV(UW14, UW13)
	.byte	0x80+3, (40-(closure_FS+4))/-4  /* DW_CFA_offset %ebx */
	ADV(UW15, UW14)
	.byte	0xc0+3			/* DW_CFA_restore %ebx */
	ADV(UW16, UW15)
#else
	ADV(UW16, UW13)
#endif
	.byte	0xe, 4			/* DW_CFA_def_cfa_offset */
	ADV(UW17, UW16)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
	ADV(UW18, UW17)
	.byte	0xe, 4			/* DW_CFA_def_cfa_offset */
	ADV(UW19, UW18)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE4):

	.set	L(set5),L(EFDE5)-L(SFDE5)
	.long	L(set5)			/* FDE Length */
L(SFDE5):
	.long	L(SFDE5)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW21))		/* Initial location */
	.long	L(UW23)-L(UW21)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW22, UW21)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE5):

	.set	L(set6),L(EFDE6)-L(SFDE6)
	.long	L(set6)			/* FDE Length */
L(SFDE6):
	.long	L(SFDE6)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW24))		/* Initial location */
	.long	L(UW26)-L(UW24)		/* Address range */
	.byte	0			/* Augmentation size */
	.byte	0xe, 8			/* DW_CFA_def_cfa_offset */
	.byte	0x80+8, 2		/* DW_CFA_offset %eip, 2*-4 */
	ADV(UW25, UW24)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE6):

	.set	L(set7),L(EFDE7)-L(SFDE7)
	.long	L(set7)			/* FDE Length */
L(SFDE7):
	.long	L(SFDE7)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW27))		/* Initial location */
	.long	L(UW31)-L(UW27)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW28, UW27)
	.byte	0xe, closure_FS+4	/* DW_CFA_def_cfa_offset */
#ifdef FFI_CLOSURE_CALL_INNER_SAVE_EBX
	ADV(UW29, UW28)
	.byte	0x80+3, (40-(closure_FS+4))/-4  /* DW_CFA_offset %ebx */
	ADV(UW30, UW29)
	.byte	0xc0+3			/* DW_CFA_restore %ebx */
#endif
	.balign	4
L(EFDE7):

#if !FFI_NO_RAW_API
	.set	L(set8),L(EFDE8)-L(SFDE8)
	.long	L(set8)			/* FDE Length */
L(SFDE8):
	.long	L(SFDE8)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW32))		/* Initial location */
	.long	L(UW40)-L(UW32)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW33, UW32)
	.byte	0xe, raw_closure_S_FS+4	/* DW_CFA_def_cfa_offset */
	ADV(UW34, UW33)
	.byte	0x80+3, 2		/* DW_CFA_offset %ebx 2*-4 */
	ADV(UW35, UW34)
	.byte	0xc0+3			/* DW_CFA_restore %ebx */
	ADV(UW36, UW35)
	.byte	0xe, 4			/* DW_CFA_def_cfa_offset */
	ADV(UW37, UW36)
	.byte	0xe, raw_closure_S_FS+4	/* DW_CFA_def_cfa_offset */
	ADV(UW38, UW37)
	.byte	0xe, 4			/* DW_CFA_def_cfa_offset */
	ADV(UW39, UW38)
	.byte	0xe, raw_closure_S_FS+4	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE8):

	.set	L(set9),L(EFDE9)-L(SFDE9)
	.long	L(set9)			/* FDE Length */
L(SFDE9):
	.long	L(SFDE9)-L(CIE)		/* FDE CIE offset */
	.long	PCREL(L(UW41))		/* Initial location */
	.long	L(UW52)-L(UW41)		/* Address range */
	.byte	0			/* Augmentation size */
	ADV(UW42, UW41)
	.byte	0xe, 0			/* DW_CFA_def_cfa_offset */
	.byte	0x9, 8, 2		/* DW_CFA_register %eip, %edx */
	ADV(UW43, UW42)
	.byte	0xe, 4			/* DW_CFA_def_cfa_offset */
	ADV(UW44, UW43)
	.byte	0xe, 8			/* DW_CFA_def_cfa_offset */
	.byte	0x80+8, 2		/* DW_CFA_offset %eip 2*-4 */
	ADV(UW45, UW44)
	.byte	0xe, raw_closure_T_FS+8	/* DW_CFA_def_cfa_offset */
	ADV(UW46, UW45)
	.byte	0x80+3, 3		/* DW_CFA_offset %ebx 3*-4 */
	ADV(UW47, UW46)
	.byte	0xc0+3			/* DW_CFA_restore %ebx */
	ADV(UW48, UW47)
	.byte	0xe, 8			/* DW_CFA_def_cfa_offset */
	ADV(UW49, UW48)
	.byte	0xe, raw_closure_T_FS+8	/* DW_CFA_def_cfa_offset */
	ADV(UW50, UW49)
	.byte	0xe, 8			/* DW_CFA_def_cfa_offset */
	ADV(UW51, UW50)
	.byte	0xe, raw_closure_T_FS+8	/* DW_CFA_def_cfa_offset */
	.balign	4
L(EFDE9):
#endif /* !FFI_NO_RAW_API */

#ifdef _WIN32
	.def	 @feat.00;
	.scl	3;
	.type	0;
	.endef
	.globl	@feat.00
@feat.00 = 1
#endif

#ifdef __APPLE__
    .subsections_via_symbols
    .section __LD,__compact_unwind,regular,debug

    /* compact unwind for ffi_call_i386 */
    .long    C(ffi_call_i386)
    .set     L1,L(UW5)-L(UW0)
    .long    L1
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_go_closure_EAX */
    .long    C(ffi_go_closure_EAX)
    .set     L2,L(UW8)-L(UW6)
    .long    L2
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_go_closure_ECX */
    .long    C(ffi_go_closure_ECX)
    .set     L3,L(UW11)-L(UW9)
    .long    L3
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_closure_i386 */
    .long    C(ffi_closure_i386)
    .set     L4,L(UW20)-L(UW12)
    .long    L4
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_go_closure_STDCALL */
    .long    C(ffi_go_closure_STDCALL)
    .set     L5,L(UW23)-L(UW21)
    .long    L5
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_closure_REGISTER */
    .long    C(ffi_closure_REGISTER)
    .set     L6,L(UW26)-L(UW24)
    .long    L6
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_closure_STDCALL */
    .long    C(ffi_closure_STDCALL)
    .set     L7,L(UW31)-L(UW27)
    .long    L7
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_closure_raw_SYSV */
    .long    C(ffi_closure_raw_SYSV)
    .set     L8,L(UW40)-L(UW32)
    .long    L8
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0

    /* compact unwind for ffi_closure_raw_THISCALL */
    .long    C(ffi_closure_raw_THISCALL)
    .set     L9,L(UW52)-L(UW41)
    .long    L9
    .long    0x04000000 /* use dwarf unwind info */
    .long    0
    .long    0
#endif /* __APPLE__ */

#endif /* ifndef _MSC_VER */
#endif /* ifdef __i386__ */
>>>>>>> BRANCH (5dcb74 Move nested_struct3 test to closures directory)

#if defined __ELF__ && defined __linux__
	.section	.note.GNU-stack,"",@progbits
#endif
