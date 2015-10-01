// Copyright (C) 2012, Texas A&M University
// All rights reserved.
// written by Gabriel Dos Reis.
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

#include <iostream>
#include "Output.H"

namespace liz {
   namespace io {
      // -- Output --
      Output::Output(std::ostream& s) : os(s), nblanks(), sz()
      { }

      Output& Output::nibble(int n) {
         nblanks += n;
         return *this;
      }

      Output& Output::operator<<(char c) {
         stream() << c;
         ++sz;
         return *this;
      }
      
      Output& Output::operator<<(const std::string& s) {
         stream() << s;
         sz += s.size();
         return *this;
      }

      Output& Output::operator<<(newline) {
         stream() << '\n';
         sz = 0;
         return *this;
      }

      Output& Output::operator<<(indent) {
         const int n = indentation();
         for (int i = 0; i < n; ++i)
            stream() << ' ';
         sz = n;
         return *this;
      }

      Output& operator<<(Output& out, Symbol s) {
         return out << s.string();
      }
   }
}
