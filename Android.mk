# Copyright 2007 The Android Open Source Project
#
# The libffi code is organized primarily by architecture, but at some
# point OS-specific issues started to creep in.  In some cases there are
# OS-specific source files, in others there are just #ifdefs in the code.
# We need to generate the appropriate defines and select the right set of
# source files for the OS and architecture.

ifneq ($(TARGET_ARCH),arm)
ifneq ($(TARGET_ARCH),sh)

LOCAL_PATH:= $(call my-dir)
include $(CLEAR_VARS)

LOCAL_C_INCLUDES := \
	external/libffi/include \
	external/libffi/$(TARGET_OS)-$(TARGET_ARCH)

LOCAL_SRC_FILES := src/debug.c src/prep_cif.c src/types.c \
        src/raw_api.c src/java_raw_api.c

ifeq ($(TARGET_OS)-$(TARGET_ARCH),linux-arm)
  LOCAL_SRC_FILES += src/arm/sysv.S src/arm/ffi.c
endif
ifeq ($(TARGET_OS)-$(TARGET_ARCH),linux-x86)
  LOCAL_SRC_FILES += src/x86/ffi.c src/x86/sysv.S
endif

ifeq ($(LOCAL_SRC_FILES),)
  LOCAL_SRC_FILES := your-architecture-not-supported-by-ffi-makefile.c
endif

LOCAL_MODULE := libffi


include $(BUILD_SHARED_LIBRARY)


include external/libffi/testsuite/Android.mk

endif
endif
