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

#include <QMessageBox>
#include <QTextBlock>
#include <stdio.h>
#include "Outlet.H"

namespace liz {
   // -- ide::Reader --
   
   ide::Reader::Reader()
   { }

   ide::Reader::~Reader()
   { }

   void
   ide::Reader::error(const Diagnostic& s) {
      QMessageBox::critical(this, tr("Reader"), tr(s.c_str()),
                            QMessageBox::Yes);
   }

   // -- idea::Outlet --

   ide::Outlet::Outlet() : comp(rdr) { 
      setLineWrapMode(NoWrap);  // please, no line wrap.
      setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
   }

   static void
   display_tokens(ide::Outlet* out, const TokenStream& toks) {
      input::Line::Number n = { };
      bool needs_space = false;
      for (auto& t : toks) {
         if (n != line_number(t)) {
            out->appendPlainText("");
            n = line_number(t);
         }
         else if (needs_space)
            out->insertPlainText(" ");
         out->insertPlainText(liz::show(t).c_str());
         needs_space = true;
      }
   }

   void
   ide::Outlet::act_on_edits(int pos, int rem, int add) {
      auto current_block = document()->findBlock(pos);
      if (not current_block.isValid())
         return;
      auto end_block = document()->findBlock(pos + add);
   }

   void
   ide::Outlet::process(const QTextDocument* doc) {
      frag.source.clear();
      frag.tokens.clear();
      for (auto block = doc->begin(); block.isValid(); block = block.next()) {
         auto text = block.text().toStdString();
         if (text.empty() or text == "\n")
            continue;
         frag.source.append(text);
      }
      frag.tokens << frag.source;
      setPlainText("");
      display_tokens(this, frag.tokens);
   }
}
