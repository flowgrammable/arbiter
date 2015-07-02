// Copyright (C) 2013, Texas A&M University.
// All rights reserved.
// Written by Gabriel Dos Reis.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are
// met:
//
//     - Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//
//     - Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//
//     - Neither the name of Liz, nor the names of its contributors may
//       be used to endorse or promote products derived from this software
//       without specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
// IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
// TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
// PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
// OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

#ifndef LIZ_OUTLET_included
#define LIZ_OUTLET_included

#include <QPlainTextEdit>
#include <liz/Input>
#include <liz/Ast>
#include "Compiler.H"

namespace liz {
   namespace ide {
      class Reader : public QWidget, public liz::Reader {
         Q_OBJECT;
      public:
         Reader();
         ~Reader();

         void error(const Diagnostic&);
      };

      class Outlet : public QPlainTextEdit {
         Q_OBJECT;
      public:
         Outlet();

         void process_input();
         TokenStream lex(const QString&);

         void process(const QTextDocument*);

      private slots:
         // Act on a change of text at position `pos' where
         // `rem' characters were removed and `add' characters
         // were added.
         void act_on_edits(int pos, int rem, int add);

      private:
         Fragment frag;
         Reader rdr;
         Compiler comp;
      };
   }
}

#endif  // LIZ_OUTLET_included

