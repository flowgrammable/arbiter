# Copyright (C) 2012-2013, Texas A&M University
# All rights reserved.
# Written by Gabriel Dos Reis
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     - Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     - Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#
#     - Neither the name of Liz, nor the names of its contributors may
#       be used to endorse or promote products derived from this software
#       without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

dnl ----------------------------
dnl -- LIZ_CANONICAL_TRIPLETS --
dnl ----------------------------
dnl Set up the canonical triplets for the build, 
dnl host, and target systems.
AC_DEFUN([LIZ_CANONICAL_TRIPLETS], [
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET
])

dnl ----------------------
dnl -- LIZ_SYSTEM_PATHS --
dnl ----------------------
AC_DEFUN([LIZ_SYSTEM_PATHS], [
AC_SUBST([LIZ_SYSDIR],[\${pkglibdir}-${VERSION}])
])

dnl ----------------------------
dnl -- LIZ_REQUIRE_MODERN_CXX --
dnl ----------------------------
dnl Require a modern C++ compiler, e.g. at least C++14.
AC_DEFUN([LIZ_REQUIRE_MODERN_CXX],[
# Assume supported compilers understand -std=c++14
liz_saved_cxx_flags="$CXXFLAGS"
CXXFLAGS="$CXXFLAGS -std=c++14"
AC_MSG_CHECKING([whether C++ compiler supports C++14])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
  [AC_MSG_RESULT([yes])],
  [AC_MSG_RESULT([no])]
  [AC_MSG_ERROR([Liz requires a C++ that supports at least C++14])])
])

dnl ---------------------
dnl -- LIZ_BUILD_TOOLS --
dnl ---------------------
dnl Check for various build tools.
AC_DEFUN([LIZ_BUILD_TOOLS], [
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_PROG_LN_S
AC_PROG_RANLIB
AC_PROG_SED

AC_PROG_CXX
AC_PROG_CPP
AC_PROG_CXXCPP
AC_SUBST([CXX])
AC_SUBST([LD])

AC_CHECK_PROGS(LIZ_LLVM_CONFIG,[llvm-config])
])

dnl ------------------
dnl -- LIZ_CHECK_MM --
dnl ------------------
dnl Check for host capability of memory mapping.
AC_DEFUN([LIZ_CHECK_MM],[
AC_CHECK_HEADERS([sys/mman.h fcntl.h])
## We want annonymous mapping for memory allocation.  Unfortunately,
## the flag for anonymous mapping is not standardized.  Popular names
##  are MAP_ANONYMOUS and MAP_ANON.
if test x"$ac_cv_header_sys_mman_h" = xyes; then
   AC_MSG_CHECKING([for flag to request anonymous memory mapping])
   AC_EGREP_CPP([MAP_ANONYMOUS],
                [#include <sys/mman.h>
#ifdef MAP_ANONYMOUS
   "MAP_ANONYMOUS"
#endif],
                [liz_mm_anonymous_flag=MAP_ANONYMOUS])
   if test -z "$liz_mm_anonymous_flag"; then
      AC_EGREP_CPP([MAP_ANON],
                   [#include <sys/mman.h>
#ifdef MAP_ANON
   "MAP_ANON"
#endif],
                   [liz_mm_anonymous_flag=MAP_ANON])
   fi
   ## It would be curious that we don't have an anonymous mapping
   ## capability.  Let that be known loudly.
   if test -n "$liz_mm_anonymous_flag"; then
      AC_MSG_RESULT([$liz_mm_anonymous_flag])
   else
      AC_MSG_ERROR([Could not find flag for anonymous map])
   fi
   AC_DEFINE_UNQUOTED([LIZ_MM_ANONYMOUS_MAP_FLAG],
                      [$liz_mm_anonymous_flag],
                      [mmap anonymous flag])
fi
])

dnl -----------------------
dnl -- LIZ_HOST_CPPFLAGS --
dnl -----------------------
AC_DEFUN([LIZ_HOST_CPPFLAGS],[
case $host in
  *linux*)
     CPPFLAGS="$CPPFLAGS -D_GNU_SOURCE"
     ;;
  *bsd*|*dragonfly*)
     CPPFLAGS="$CPPFLAGS -D_BSD_SOURCE"
     ;;
  *mingw*)
     CPPFLAGS="$CPPFLAGS -DLIZ_WINDOWS_HOST"
     ;;
esac
])

dnl ----------------------
dnl -- LIZ_TARGET_TOOLS --
dnl ----------------------
dnl Check for the availability of tools needed for
dnl the target.
AC_DEFUN([LIZ_TARGET_TOOLS], [
AC_CHECK_TARGET_TOOL([LIZ_TARGET_AS],[as])
AC_CHECK_TARGET_TOOL([LIZ_TARGET_LD],[ld])
AC_SUBST(LIZ_TARGET_AS)
AC_SUBST(LIZ_TARGET_LD)
])

dnl -------------------
dnl -- LIZ_CHECK_GMP --
dnl -------------------
AC_DEFUN([LIZ_CHECK_GMP], [
## Default to macports locations on macs
case "${target}" in
   *-*-darwin*)
     if test x"${with_gmp}" = x && test -f /opt/local/include/gmp.h; then
        with_gmp=/opt/local
     fi
   ;;
esac
# Take whatever user wants us to use.
AC_ARG_WITH(gmp,
  [AS_HELP_STRING([--with-gmp=PATH],
     [specify prefix path for the installed GMP packag])])
if test x"${with_gmp}" != x; then
  CPPFLAGS="-I${with_gmp}/include ${CPPFLAGS}"
  LIBS="-L${with_gmp}/lib ${LIBS}"
fi
# Check if the whole thing works out.
AC_CHECK_HEADERS([gmp.h], [], 
                [AC_MSG_ERROR([Could not find <gmp.h> header file])])
AC_CHECK_LIB([gmp], [__gmpz_init], [],
             [AC_MSG_ERROR([GMP runtime library not found])])
])

dnl ---------------------------
dnl -- LIZ_SUPPORT_LIBRARIES --
dnl ---------------------------
dnl Check for host and target libraries support libraries.
AC_DEFUN([LIZ_SUPPORT_LIBRARIES], [
LIZ_CHECK_GMP
]) 
