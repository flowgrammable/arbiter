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

AUTOMAKE_OPTIONS = nostdinc

LIZ_SYSDIR = $(pkglibdir)-$(VERSION)


bin_PROGRAMS = liz
noinst_LIBRARIES = libliz.a

liz_cxxwarn_flags = -Wall -pedantic -Wextra
liz_cpp_defines = -DLIZ_SYSTEM_DIRECTORY="\"$(LIZ_SYSDIR)\"" \
  -DLIZ_BUILD_DIRECTORY="\"$(abs_top_builddir)\""
liz_incdir = \
	-I$(srcdir) \
	-I$(top_builddir)/include \
	-I$(srcdir)/io \
	-I$(srcdir)/overseer \
	-I$(srcdir)/syntax \
	-I$(srcdir)/semantics \
	-I$(srcdir)/meta
AM_CXXFLAGS = $(liz_incdir) $(liz_cxxwarn_flags) $(liz_cpp_defines)

libliz_a_SOURCES = \
	support/utility.C \
	support/tools.C \
	support/storage.C \
	io/Character.C \
	io/Input.C \
	io/Output.C \
	io/pretty-print.C \
	overseer/BasicDriver.C \
	syntax/Token.C \
	syntax/Ast.C \
	syntax/ast-io.C \
	syntax/Parser.C \
	semantics/Data.C \
	semantics/Type.C \
	semantics/Expression.C \
	semantics/expression-io.C \
	semantics/pattern.C \
	semantics/logic.C \
	semantics/arithmetic.C \
	semantics/elaboration.C \
	backend/cxx.C \
	backend/cxx-format.C \
	meta/Translator.C \
	meta/Elaborator.C \
	meta/Evaluator.C \
	meta/Compiler.C \
	meta/intrinsics.C

liz_SOURCES = \
	cli/driver.C

liz_LDADD = libliz.a
