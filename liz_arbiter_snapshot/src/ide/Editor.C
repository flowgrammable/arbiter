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

#include <QFont>
#include "Editor.H"

namespace liz {
   // Return a monospace font
   static QFont monospace_font() {
      QFont f("Monaco", 11);
      f.setStyleHint(QFont::TypeWriter);
      return f;
   }

   // Default number of characters per question line.
   const int columns = 80;
   const int lines = 40;

   ide::Editor::Editor(QWidget* w) : QPlainTextEdit(w) {
      setFont(monospace_font());
      setBackgroundRole(QPalette::Base);
      setLineWrapMode(NoWrap);  // please, no line wrap.
      setSizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
      setDocumentTitle(path.isEmpty() ? "*unsaved*" : path);
   }

   ide::Editor::~Editor() { }

   QSize
   ide::Editor::sizeHint() const {
      if (document()->isEmpty())
         return {
            columns * fontMetrics().width('m'),
            lines * fontMetrics().lineSpacing()
         };
      return size();
   }
}
