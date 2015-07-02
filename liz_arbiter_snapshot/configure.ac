dnl Copyright (C) 2012-2013, Texas A&M University
dnl Copyright (C) 2014, Gabriel Dos Reis.
dnl All rights reserved.
dnl Written by Gabriel Dos Reis.
dnl
dnl Redistribution and use in source and binary forms, with or without
dnl modification, are permitted provided that the following conditions are
dnl met:
dnl
dnl     - Redistributions of source code must retain the above copyright
dnl       notice, this list of conditions and the following disclaimer.
dnl
dnl     - Redistributions in binary form must reproduce the above copyright
dnl       notice, this list of conditions and the following disclaimer in
dnl       the documentation and/or other materials provided with the
dnl       distribution.
dnl
dnl     - Neither the name of Liz, nor the names of its contributors may
dnl       be used to endorse or promote products derived from this software
dnl       without specific prior written permission.
dnl
dnl THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
dnl IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
dnl TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
dnl PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
dnl OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
dnl EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
dnl PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
dnl PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
dnl LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
dnl NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
dnl SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

dnl Most of the macros used in this configure.ac are defined in files
dnl located in the subdirectory misc/
sinclude(misc/liz.m4)

AC_INIT([liz], [1.0], [liz@axiomatics.org])

AC_CONFIG_AUX_DIR([misc])
AC_CONFIG_MACRO_DIR([misc])

LIZ_CANONICAL_TRIPLETS

AM_INIT_AUTOMAKE([foreign subdir-objects])
AC_PREREQ([2.62])

AC_REQUIRE_AUX_FILE(liz.m4)
AC_CONFIG_FILES([Makefile include/Makefile src/Makefile library/Makefile
   library/Basic/Makefile library/Builtin/Makefile library/Flow/Makefile])
AC_LANG([C++])

dnl Put all configuration macros here
AC_CONFIG_HEADERS([include/liz/config:misc/config.h.in])
AH_TOP([#ifndef LIZ_CONFIG_included
#define LIZ_CONFIG_included
])
AH_BOTTOM([#endif   // LIZ_CONFIG_included])

LIZ_SYSTEM_PATHS
LIZ_BUILD_TOOLS
LIZ_REQUIRE_MODERN_CXX

LIZ_HOST_CPPFLAGS
LIZ_CHECK_MM
LIZ_TARGET_TOOLS
LIZ_SUPPORT_LIBRARIES
AM_CONDITIONAL([LIZ_BUILD_IDE], [test x$liz_has_qt = xyes])

AC_CONFIG_LINKS([
  include/liz/utility:src/support/utility.H
  include/liz/storage:src/support/storage.H
  include/liz/Character:src/io/Character.H
  include/liz/Input:src/io/Input.H
  include/liz/Token:src/syntax/Token.H
  include/liz/Ast:src/syntax/Ast.H
])

## Default to macports locations on macs
case "${target}" in
   *-*-darwin*)
     if test x"${with_lpsolve}" = x && test -f /opt/local/include/lp_lib.h; then
        with_lpsolve=/opt/local
     fi
   ;;
esac
# Check whether --with-gmp was given.
if test "${with_lpsolve+set}" = set; then :
  withval=$with_lpsolve;
fi

if test x"${with_lpsolve}" != x; then
  CPPFLAGS="-I${with_lpsolve}/include/lpsolve ${CPPFLAGS}"
  LIBS="-L${with_lpsolve}/lib ${LIBS}"
fi
# Check if the whole thing works out.
for ac_header in lp_lib.h
do :
  ac_fn_cxx_check_header_mongrel "$LINENO" "lp_lib.h" "ac_cv_header_lp_lib_h" "$ac_includes_default"
if test "x$ac_cv_header_lp_lib_h" = xyes; then :
  cat >>confdefs.h <<_ACEOF
#define HAVE_LPSOLVE_H 1
_ACEOF

else
  as_fn_error $? "Could not find <lp_lib.h> header file" "$LINENO" 5
fi

done

{ $as_echo "$as_me:${as_lineno-$LINENO}: checking for make_lp in -llpsolve" >&5
$as_echo_n "checking for make_lp in liblpsolve55.so... " >&6; }
if ${ac_cv_lib_lpsolve_make_lp+:} false; then :
  $as_echo_n "(cached) " >&6
else
  ac_check_lib_save_LIBS=$LIBS
LIBS="-llpsolve55  $LIBS"
cat confdefs.h - <<_ACEOF >conftest.$ac_ext
#include "lp_lib.h"
/* end confdefs.h.  */

/* Override any GCC internal prototype to avoid an error.
   Use char because int might match the return type of a GCC
   builtin and then its argument prototype would still apply.  */
#ifdef __cplusplus
extern "C"
#endif
int
main ()
{
  lprec *lp;
  lp = make_lp(0,0);
  delete_lp(lp);
  return 0;
}
_ACEOF
if ac_fn_cxx_try_link "$LINENO"; then :
  ac_cv_lib_lpsolve_make_lp=yes
else
  ac_cv_lib_lpsolve_make_lp=no
fi
rm -f core conftest.err conftest.$ac_objext \
    conftest$ac_exeext conftest.$ac_ext
LIBS=$ac_check_lib_save_LIBS
fi
{ $as_echo "$as_me:${as_lineno-$LINENO}: result: $ac_cv_lib_lpsolve_make_lp" >&5
$as_echo "$ac_cv_lib_lpsolve_make_lp" >&6; }
if test "x$ac_cv_lib_lpsolve_make_lp" = xyes; then :
  cat >>confdefs.h <<_ACEOF
#define HAVE_LIBLPSOLVE 1
_ACEOF

  LIBS="-llpsolve55 $LIBS"

else
  as_fn_error $? "lpsolve runtime library not found" "$LINENO" 5
fi

AC_CONFIG_FILES([src/support/tools.C src/ide/qt-mk.pro])

AC_OUTPUT

## Set up Makefile for QT-based gui
if test x$liz_has_qt = xyes; then
  liz_qmake_spec=
  case $host,$GXX in
     *darwin*,yes)
         liz_qmake_spec="-spec macx-g++"
         ;;
  esac
  AC_SUBST([liz_qmake_spec])
  (cd src/ide && $LIZ_QMAKE -makefile $liz_qmake_spec qt-mk.pro)
fi

