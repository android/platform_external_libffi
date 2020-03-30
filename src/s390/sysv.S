/* -----------------------------------------------------------------------
   sysv.S - Copyright (c) 2000 Software AG
            Copyright (c) 2008 Red Hat, Inc.
<<<<<<< HEAD   (1246a0 Merge "Remove redundant NOTICE copied from LICENSE.")
 
   S390 Foreign Function Interface
 
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

#define LIBFFI_ASM
#include <fficonfig.h>
#include <ffi.h>

#ifndef __s390x__
 
.text

	# r2:	cif->bytes
	# r3:	&ecif
	# r4:	ffi_prep_args
	# r5:	ret_type
	# r6:	ecif.rvalue
	# ov:	fn 
 
	# This assumes we are using gas.
	.globl	ffi_call_SYSV
	.type	ffi_call_SYSV,%function
ffi_call_SYSV:
.LFB1:
	stm	%r6,%r15,24(%r15)		# Save registers
.LCFI0:
	basr	%r13,0				# Set up base register
.Lbase:
	lr	%r11,%r15			# Set up frame pointer
.LCFI1:
	sr	%r15,%r2
	ahi	%r15,-96-48			# Allocate stack
	lr	%r8,%r6				# Save ecif.rvalue
	sr	%r9,%r9
	ic	%r9,.Ltable-.Lbase(%r13,%r5)	# Load epilog address
	l	%r7,96(%r11)			# Load function address
	st	%r11,0(%r15)			# Set up back chain
	ahi	%r11,-48			# Register save area
.LCFI2:

	la	%r2,96(%r15)			# Save area
						# r3 already holds &ecif
	basr	%r14,%r4			# Call ffi_prep_args

	lm	%r2,%r6,0(%r11)			# Load arguments
	ld	%f0,32(%r11)
	ld	%f2,40(%r11)
	la	%r14,0(%r13,%r9)		# Set return address
	br	%r7				# ... and call function

.LretNone:					# Return void
	l	%r4,48+56(%r11)
	lm	%r6,%r15,48+24(%r11)
	br	%r4

.LretFloat:
	l	%r4,48+56(%r11)
	ste	%f0,0(%r8)			# Return float
	lm	%r6,%r15,48+24(%r11)
	br	%r4
 
.LretDouble:
	l	%r4,48+56(%r11)
	std	%f0,0(%r8)			# Return double
	lm	%r6,%r15,48+24(%r11)
	br	%r4

.LretInt32:
	l	%r4,48+56(%r11)
	st	%r2,0(%r8)			# Return int
	lm	%r6,%r15,48+24(%r11)
	br	%r4
 
.LretInt64:
	l	%r4,48+56(%r11)
	stm	%r2,%r3,0(%r8)			# Return long long
	lm	%r6,%r15,48+24(%r11)
	br	%r4
 
.Ltable:
	.byte	.LretNone-.Lbase		# FFI390_RET_VOID
	.byte	.LretNone-.Lbase		# FFI390_RET_STRUCT
	.byte	.LretFloat-.Lbase		# FFI390_RET_FLOAT
	.byte	.LretDouble-.Lbase		# FFI390_RET_DOUBLE
	.byte	.LretInt32-.Lbase		# FFI390_RET_INT32
	.byte	.LretInt64-.Lbase		# FFI390_RET_INT64

.LFE1: 
.ffi_call_SYSV_end:
	.size	 ffi_call_SYSV,.ffi_call_SYSV_end-ffi_call_SYSV


	.globl	ffi_closure_SYSV
	.type	ffi_closure_SYSV,%function
ffi_closure_SYSV:
.LFB2:
	stm	%r12,%r15,48(%r15)		# Save registers
.LCFI10:
	basr	%r13,0				# Set up base register
.Lcbase:
	stm	%r2,%r6,8(%r15)			# Save arguments
	std	%f0,64(%r15)
	std	%f2,72(%r15)
	lr	%r1,%r15			# Set up stack frame
	ahi	%r15,-96
.LCFI11:
	l	%r12,.Lchelper-.Lcbase(%r13)	# Get helper function
	lr	%r2,%r0				# Closure
	la	%r3,8(%r1)			# GPRs
	la	%r4,64(%r1)			# FPRs
	la	%r5,96(%r1)			# Overflow
	st	%r1,0(%r15)			# Set up back chain

	bas	%r14,0(%r12,%r13)		# Call helper

	l	%r4,96+56(%r15)
	ld	%f0,96+64(%r15)			# Load return registers
	lm	%r2,%r3,96+8(%r15)
	lm	%r12,%r15,96+48(%r15)
	br	%r4

	.align 4
.Lchelper:
	.long	ffi_closure_helper_SYSV-.Lcbase

.LFE2: 

.ffi_closure_SYSV_end:
	.size	 ffi_closure_SYSV,.ffi_closure_SYSV_end-ffi_closure_SYSV


	.section	.eh_frame,EH_FRAME_FLAGS,@progbits
.Lframe1:
	.4byte	.LECIE1-.LSCIE1	# Length of Common Information Entry
.LSCIE1:
	.4byte	0x0	# CIE Identifier Tag
	.byte	0x1	# CIE Version
	.ascii "zR\0"	# CIE Augmentation
	.uleb128 0x1	# CIE Code Alignment Factor
	.sleb128 -4	# CIE Data Alignment Factor
	.byte	0xe	# CIE RA Column
	.uleb128 0x1	# Augmentation size
	.byte	0x1b	# FDE Encoding (pcrel sdata4)
	.byte	0xc	# DW_CFA_def_cfa
	.uleb128 0xf
	.uleb128 0x60
	.align	4
.LECIE1:
.LSFDE1:
	.4byte	.LEFDE1-.LASFDE1	# FDE Length
.LASFDE1:
	.4byte	.LASFDE1-.Lframe1	# FDE CIE offset
	.4byte	.LFB1-.	# FDE initial location
	.4byte	.LFE1-.LFB1	# FDE address range
	.uleb128 0x0	# Augmentation size
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI0-.LFB1
	.byte	0x8f	# DW_CFA_offset, column 0xf
	.uleb128 0x9
	.byte	0x8e	# DW_CFA_offset, column 0xe
	.uleb128 0xa
	.byte	0x8d	# DW_CFA_offset, column 0xd
	.uleb128 0xb
	.byte	0x8c	# DW_CFA_offset, column 0xc
	.uleb128 0xc
	.byte	0x8b	# DW_CFA_offset, column 0xb
	.uleb128 0xd
	.byte	0x8a	# DW_CFA_offset, column 0xa
	.uleb128 0xe
	.byte	0x89	# DW_CFA_offset, column 0x9
	.uleb128 0xf
	.byte	0x88	# DW_CFA_offset, column 0x8
	.uleb128 0x10
	.byte	0x87	# DW_CFA_offset, column 0x7
	.uleb128 0x11
	.byte	0x86	# DW_CFA_offset, column 0x6
	.uleb128 0x12
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI1-.LCFI0
	.byte	0xd	# DW_CFA_def_cfa_register
	.uleb128 0xb
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI2-.LCFI1
	.byte	0xe	# DW_CFA_def_cfa_offset
	.uleb128 0x90
	.align	4
.LEFDE1:
.LSFDE2:
	.4byte	.LEFDE2-.LASFDE2	# FDE Length
.LASFDE2:
	.4byte	.LASFDE2-.Lframe1	# FDE CIE offset
	.4byte	.LFB2-.	# FDE initial location
	.4byte	.LFE2-.LFB2	# FDE address range
	.uleb128 0x0	# Augmentation size
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI10-.LFB2
	.byte	0x8f	# DW_CFA_offset, column 0xf
	.uleb128 0x9
	.byte	0x8e	# DW_CFA_offset, column 0xe
	.uleb128 0xa
	.byte	0x8d	# DW_CFA_offset, column 0xd
	.uleb128 0xb
	.byte	0x8c	# DW_CFA_offset, column 0xc
	.uleb128 0xc
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI11-.LCFI10
	.byte	0xe	# DW_CFA_def_cfa_offset
	.uleb128 0xc0
	.align	4
.LEFDE2:

#else
 
.text
 
	# r2:	cif->bytes
	# r3:	&ecif
	# r4:	ffi_prep_args
	# r5:	ret_type
	# r6:	ecif.rvalue
	# ov:	fn 
 
	# This assumes we are using gas.
	.globl	ffi_call_SYSV
	.type	ffi_call_SYSV,%function
ffi_call_SYSV:
.LFB1:
	stmg	%r6,%r15,48(%r15)		# Save registers
.LCFI0:
	larl	%r13,.Lbase			# Set up base register
	lgr	%r11,%r15			# Set up frame pointer
.LCFI1:
	sgr	%r15,%r2
	aghi	%r15,-160-80			# Allocate stack
	lgr	%r8,%r6				# Save ecif.rvalue
	llgc	%r9,.Ltable-.Lbase(%r13,%r5)	# Load epilog address
	lg	%r7,160(%r11)			# Load function address
	stg	%r11,0(%r15)			# Set up back chain
	aghi	%r11,-80			# Register save area
.LCFI2:

	la	%r2,160(%r15)			# Save area
						# r3 already holds &ecif
	basr	%r14,%r4			# Call ffi_prep_args

	lmg	%r2,%r6,0(%r11)			# Load arguments
	ld	%f0,48(%r11)
	ld	%f2,56(%r11)
	ld	%f4,64(%r11)
	ld	%f6,72(%r11)
	la	%r14,0(%r13,%r9)		# Set return address
	br	%r7				# ... and call function

.Lbase:
.LretNone:					# Return void
	lg	%r4,80+112(%r11)
	lmg	%r6,%r15,80+48(%r11)
	br	%r4

.LretFloat:
	lg	%r4,80+112(%r11)
	ste	%f0,0(%r8)			# Return float
	lmg	%r6,%r15,80+48(%r11)
	br	%r4
 
.LretDouble:
	lg	%r4,80+112(%r11)
	std	%f0,0(%r8)			# Return double
	lmg	%r6,%r15,80+48(%r11)
	br	%r4

.LretInt32:
	lg	%r4,80+112(%r11)
	st	%r2,0(%r8)			# Return int
	lmg	%r6,%r15,80+48(%r11)
	br	%r4
 
.LretInt64:
	lg	%r4,80+112(%r11)
	stg	%r2,0(%r8)			# Return long
	lmg	%r6,%r15,80+48(%r11)
	br	%r4
 
.Ltable:
	.byte	.LretNone-.Lbase		# FFI390_RET_VOID
	.byte	.LretNone-.Lbase		# FFI390_RET_STRUCT
	.byte	.LretFloat-.Lbase		# FFI390_RET_FLOAT
	.byte	.LretDouble-.Lbase		# FFI390_RET_DOUBLE
	.byte	.LretInt32-.Lbase		# FFI390_RET_INT32
	.byte	.LretInt64-.Lbase		# FFI390_RET_INT64

.LFE1: 
.ffi_call_SYSV_end:
	.size	 ffi_call_SYSV,.ffi_call_SYSV_end-ffi_call_SYSV


	.globl	ffi_closure_SYSV
	.type	ffi_closure_SYSV,%function
ffi_closure_SYSV:
.LFB2:
	stmg	%r14,%r15,112(%r15)		# Save registers
.LCFI10:
	stmg	%r2,%r6,16(%r15)		# Save arguments
	std	%f0,128(%r15)
	std	%f2,136(%r15)
	std	%f4,144(%r15)
	std	%f6,152(%r15)
	lgr	%r1,%r15			# Set up stack frame
	aghi	%r15,-160
.LCFI11:
	lgr	%r2,%r0				# Closure
	la	%r3,16(%r1)			# GPRs
	la	%r4,128(%r1)			# FPRs
	la	%r5,160(%r1)			# Overflow
	stg	%r1,0(%r15)			# Set up back chain

	brasl	%r14,ffi_closure_helper_SYSV	# Call helper

	lg	%r14,160+112(%r15)
	ld	%f0,160+128(%r15)		# Load return registers
	lg	%r2,160+16(%r15)
	la	%r15,160(%r15)
	br	%r14
.LFE2: 

.ffi_closure_SYSV_end:
	.size	 ffi_closure_SYSV,.ffi_closure_SYSV_end-ffi_closure_SYSV



	.section	.eh_frame,EH_FRAME_FLAGS,@progbits
.Lframe1:
	.4byte	.LECIE1-.LSCIE1	# Length of Common Information Entry
.LSCIE1:
	.4byte	0x0	# CIE Identifier Tag
	.byte	0x1	# CIE Version
	.ascii "zR\0"	# CIE Augmentation
	.uleb128 0x1	# CIE Code Alignment Factor
	.sleb128 -8	# CIE Data Alignment Factor
	.byte	0xe	# CIE RA Column
	.uleb128 0x1	# Augmentation size
	.byte	0x1b	# FDE Encoding (pcrel sdata4)
	.byte	0xc	# DW_CFA_def_cfa
	.uleb128 0xf
	.uleb128 0xa0
	.align	8
.LECIE1:
.LSFDE1:
	.4byte	.LEFDE1-.LASFDE1	# FDE Length
.LASFDE1:
	.4byte	.LASFDE1-.Lframe1	# FDE CIE offset
	.4byte	.LFB1-.	# FDE initial location
	.4byte	.LFE1-.LFB1	# FDE address range
	.uleb128 0x0	# Augmentation size
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI0-.LFB1
	.byte	0x8f	# DW_CFA_offset, column 0xf
	.uleb128 0x5
	.byte	0x8e	# DW_CFA_offset, column 0xe
	.uleb128 0x6
	.byte	0x8d	# DW_CFA_offset, column 0xd
	.uleb128 0x7
	.byte	0x8c	# DW_CFA_offset, column 0xc
	.uleb128 0x8
	.byte	0x8b	# DW_CFA_offset, column 0xb
	.uleb128 0x9
	.byte	0x8a	# DW_CFA_offset, column 0xa
	.uleb128 0xa
	.byte	0x89	# DW_CFA_offset, column 0x9
	.uleb128 0xb
	.byte	0x88	# DW_CFA_offset, column 0x8
	.uleb128 0xc
	.byte	0x87	# DW_CFA_offset, column 0x7
	.uleb128 0xd
	.byte	0x86	# DW_CFA_offset, column 0x6
	.uleb128 0xe
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI1-.LCFI0
	.byte	0xd	# DW_CFA_def_cfa_register
	.uleb128 0xb
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI2-.LCFI1
	.byte	0xe	# DW_CFA_def_cfa_offset
	.uleb128 0xf0
	.align	8
.LEFDE1:
.LSFDE2:
	.4byte	.LEFDE2-.LASFDE2	# FDE Length
.LASFDE2:
	.4byte	.LASFDE2-.Lframe1	# FDE CIE offset
	.4byte	.LFB2-.	# FDE initial location
	.4byte	.LFE2-.LFB2	# FDE address range
	.uleb128 0x0	# Augmentation size
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI10-.LFB2
	.byte	0x8f	# DW_CFA_offset, column 0xf
	.uleb128 0x5
	.byte	0x8e	# DW_CFA_offset, column 0xe
	.uleb128 0x6
	.byte	0x4	# DW_CFA_advance_loc4
	.4byte	.LCFI11-.LCFI10
	.byte	0xe	# DW_CFA_def_cfa_offset
	.uleb128 0x140
	.align	8
.LEFDE2:

#endif
=======

   S390 Foreign Function Interface

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

#define LIBFFI_ASM
#include <fficonfig.h>
#include <ffi.h>

	.text

#ifndef __s390x__

	# r2:	frame
	# r3:	ret_type
	# r4:	ret_addr
	# r5:	fun
	# r6:	closure

	# This assumes we are using gas.
	.balign	8
	.globl	ffi_call_SYSV
	FFI_HIDDEN(ffi_call_SYSV)
	.type	ffi_call_SYSV,%function
ffi_call_SYSV:
	.cfi_startproc
	st	%r6,44(%r2)			# Save registers
	stm	%r12,%r14,48(%r2)
	lr	%r13,%r2			# Install frame pointer
	.cfi_rel_offset r6, 44
	.cfi_rel_offset r12, 48
	.cfi_rel_offset r13, 52
	.cfi_rel_offset r14, 56
	.cfi_def_cfa_register r13
	st	%r2,0(%r15)			# Set up back chain
	sla	%r3,3				# ret_type *= 8
	lr	%r12,%r4			# Save ret_addr
	lr	%r1,%r5				# Save fun
	lr	%r0,%r6				# Install static chain

	# Set return address, so that there is only one indirect jump.
#ifdef HAVE_AS_S390_ZARCH
	larl	%r14,.Ltable
	ar	%r14,%r3
#else
	basr	%r14,0
0:	la	%r14,.Ltable-0b(%r14,%r3)
#endif

	lm	%r2,%r6,8(%r13)			# Load arguments
	ld	%f0,64(%r13)
	ld	%f2,72(%r13)
	br	%r1				# ... and call function

	.balign	8
.Ltable:
# FFI390_RET_DOUBLE
	std	%f0,0(%r12)
	j	.Ldone

	.balign	8
# FFI390_RET_FLOAT
	ste	%f0,0(%r12)
	j	.Ldone

	.balign	8
# FFI390_RET_INT64
	st	%r3,4(%r12)
	nop
	# fallthru

	.balign	8
# FFI390_RET_INT32
	st	%r2,0(%r12)
	nop
	# fallthru

	.balign	8
# FFI390_RET_VOID
.Ldone:
	l	%r14,56(%r13)
	l	%r12,48(%r13)
	l	%r6,44(%r13)
	l	%r13,52(%r13)
	.cfi_restore 14
	.cfi_restore 13
	.cfi_restore 12
	.cfi_restore 6
	.cfi_def_cfa r15, 96
	br	%r14
	.cfi_endproc
	.size	 ffi_call_SYSV,.-ffi_call_SYSV


	.balign	8
	.globl	ffi_go_closure_SYSV
	FFI_HIDDEN(ffi_go_closure_SYSV)
	.type	ffi_go_closure_SYSV,%function
ffi_go_closure_SYSV:
	.cfi_startproc
	stm	%r2,%r6,8(%r15)			# Save arguments
	lr	%r4,%r0				# Load closure -> user_data
	l	%r2,4(%r4)			#   ->cif
	l	%r3,8(%r4)			#   ->fun
	j	.Ldoclosure
	.cfi_endproc

	.balign	8
	.globl	ffi_closure_SYSV
	FFI_HIDDEN(ffi_closure_SYSV)
	.type	ffi_closure_SYSV,%function
ffi_closure_SYSV:
	.cfi_startproc
	stm	%r2,%r6,8(%r15)			# Save arguments
	lr	%r4,%r0				# Closure
	l	%r2,16(%r4)			#   ->cif
	l	%r3,20(%r4)			#   ->fun
	l	%r4,24(%r4)			#   ->user_data
.Ldoclosure:
	stm	%r12,%r15,48(%r15)		# Save registers
	lr	%r12,%r15
	.cfi_def_cfa_register r12
	.cfi_rel_offset r6, 24
	.cfi_rel_offset r12, 48
	.cfi_rel_offset r13, 52
	.cfi_rel_offset r14, 56
	.cfi_rel_offset r15, 60
#ifndef HAVE_AS_S390_ZARCH
	basr	%r13,0				# Set up base register
.Lcbase:
	l	%r1,.Lchelper-.Lcbase(%r13)	# Get helper function
#endif
	ahi	%r15,-96-8			# Set up stack frame
	st	%r12,0(%r15)			# Set up back chain

	std	%f0,64(%r12)			# Save fp arguments
	std	%f2,72(%r12)

	la	%r5,96(%r12)			# Overflow
	st	%r5,96(%r15)
	la	%r6,64(%r12)			# FPRs
	la	%r5,8(%r12)			# GPRs
#ifdef HAVE_AS_S390_ZARCH
	brasl	%r14,ffi_closure_helper_SYSV
#else
	bas	%r14,0(%r1,%r13)		# Call helper
#endif

	lr	%r15,%r12
	.cfi_def_cfa_register r15
	lm	%r12,%r14,48(%r12)		# Restore saved registers
	l	%r6,24(%r15)
	ld	%f0,64(%r15)			# Load return registers
	lm	%r2,%r3,8(%r15)
	br	%r14
	.cfi_endproc

#ifndef HAVE_AS_S390_ZARCH
	.align 4
.Lchelper:
	.long	ffi_closure_helper_SYSV-.Lcbase
#endif

	.size	 ffi_closure_SYSV,.-ffi_closure_SYSV

#else

	# r2:	frame
	# r3:	ret_type
	# r4:	ret_addr
	# r5:	fun
	# r6:	closure

	# This assumes we are using gas.
	.balign	8
	.globl	ffi_call_SYSV
	FFI_HIDDEN(ffi_call_SYSV)
	.type	ffi_call_SYSV,%function
ffi_call_SYSV:
	.cfi_startproc
	stg	%r6,88(%r2)			# Save registers
	stmg	%r12,%r14,96(%r2)
	lgr	%r13,%r2			# Install frame pointer
	.cfi_rel_offset r6, 88
	.cfi_rel_offset r12, 96
	.cfi_rel_offset r13, 104
	.cfi_rel_offset r14, 112
	.cfi_def_cfa_register r13
	stg	%r2,0(%r15)			# Set up back chain
	larl	%r14,.Ltable			# Set up return address
	slag	%r3,%r3,3			# ret_type *= 8
	lgr	%r12,%r4			# Save ret_addr
	lgr	%r1,%r5				# Save fun
	lgr	%r0,%r6				# Install static chain
	agr	%r14,%r3
	lmg	%r2,%r6,16(%r13)		# Load arguments
	ld	%f0,128(%r13)
	ld	%f2,136(%r13)
	ld	%f4,144(%r13)
	ld	%f6,152(%r13)
	br	%r1				# ... and call function

	.balign	8
.Ltable:
# FFI390_RET_DOUBLE
	std	%f0,0(%r12)
	j	.Ldone

	.balign	8
# FFI390_RET_DOUBLE
	ste	%f0,0(%r12)
	j	.Ldone

	.balign	8
# FFI390_RET_INT64
	stg	%r2,0(%r12)

	.balign	8
# FFI390_RET_INT32
	# Never used, as we always store type ffi_arg.
	# But the stg above is 6 bytes and we cannot
	# jump around this case, so fall through.
	nop
	nop

	.balign	8
# FFI390_RET_VOID
.Ldone:
	lg	%r14,112(%r13)
	lg	%r12,96(%r13)
	lg	%r6,88(%r13)
	lg	%r13,104(%r13)
	.cfi_restore r14
	.cfi_restore r13
	.cfi_restore r12
	.cfi_restore r6
	.cfi_def_cfa r15, 160
	br	%r14
	.cfi_endproc
	.size	 ffi_call_SYSV,.-ffi_call_SYSV


	.balign	8
	.globl	ffi_go_closure_SYSV
	FFI_HIDDEN(ffi_go_closure_SYSV)
	.type	ffi_go_closure_SYSV,%function
ffi_go_closure_SYSV:
	.cfi_startproc
	stmg	%r2,%r6,16(%r15)		# Save arguments
	lgr	%r4,%r0				# Load closure -> user_data
	lg	%r2,8(%r4)			#   ->cif
	lg	%r3,16(%r4)			#   ->fun
	j	.Ldoclosure
	.cfi_endproc
	.size	 ffi_go_closure_SYSV,.-ffi_go_closure_SYSV


	.balign	8
	.globl	ffi_closure_SYSV
	FFI_HIDDEN(ffi_closure_SYSV)
	.type	ffi_closure_SYSV,%function
ffi_closure_SYSV:
	.cfi_startproc
	stmg	%r2,%r6,16(%r15)		# Save arguments
	lgr	%r4,%r0				# Load closure
	lg	%r2,32(%r4)			#   ->cif
	lg	%r3,40(%r4)			#   ->fun
	lg	%r4,48(%r4)			#   ->user_data
.Ldoclosure:
	stmg	%r13,%r15,104(%r15)		# Save registers
	lgr	%r13,%r15
	.cfi_def_cfa_register r13
	.cfi_rel_offset r6, 48
	.cfi_rel_offset r13, 104
	.cfi_rel_offset r14, 112
	.cfi_rel_offset r15, 120
	aghi	%r15,-160-16			# Set up stack frame
	stg	%r13,0(%r15)			# Set up back chain

	std	%f0,128(%r13)			# Save fp arguments
	std	%f2,136(%r13)
	std	%f4,144(%r13)
	std	%f6,152(%r13)
	la	%r5,160(%r13)			# Overflow
	stg	%r5,160(%r15)
	la	%r6,128(%r13)			# FPRs
	la	%r5,16(%r13)			# GPRs
	brasl	%r14,ffi_closure_helper_SYSV	# Call helper

	lgr	%r15,%r13
	.cfi_def_cfa_register r15
	lmg	%r13,%r14,104(%r13)		# Restore saved registers
	lg	%r6,48(%r15)
	ld	%f0,128(%r15)			# Load return registers
	lg	%r2,16(%r15)
	br	%r14
	.cfi_endproc
	.size	 ffi_closure_SYSV,.-ffi_closure_SYSV
#endif /* !s390x */
>>>>>>> BRANCH (5dcb74 Move nested_struct3 test to closures directory)

#if defined __ELF__ && defined __linux__
	.section	.note.GNU-stack,"",@progbits
#endif
