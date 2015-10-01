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

#ifndef LIZ_OUTPUT_INCLUDED
#define LIZ_OUTPUT_INCLUDED

#include <iosfwd>
#include <string>
#include <liz/utility>
#include <liz/Character>

namespace liz {
   namespace io {
      // IO new line action.
      struct newline { };

      // IO indent action.
      struct indent { };

      // Amount of blank characters by indentation unit.
      const int tab_unit = 3;

      // ------------
      // -- Output --
      // ------------
      struct Output {
         explicit Output(std::ostream&);

         // Augment current indentation count by a given amount.
         // Note: no actual indentation is performed.
         Output& nibble(int);

         // Return the underlying stream object.
         std::ostream& stream() { return os; }

         // Return the current indentation count.
         int indentation() const { return nblanks; }

         // Return the current line length since the last newline action.
         int line_lenght() const { return sz; }

         // Act upon a newline request.
         Output& operator<<(newline);

         // Act upon an indentatin request.
         Output& operator<<(indent);

         // Format a character; update line length.
         Output& operator<<(char);

         // Format a string; update line length.
         Output& operator<<(const std::string&);

      private:
         std::ostream& os;
         int nblanks;
         int sz;
      };

      Output& operator<<(Output&, Symbol);

      inline Output& operator<<(Output& out, Character c) {
         out.stream() << c;
         return out;
      }

      inline Output& operator<<(Output& out, int i) {
         out.stream() << i;
         return out;
      }

      inline Output& operator<<(Output& out, size_t i) {
         out.stream() << i;
         return out;
      }

      inline Output& operator<<(Output& out, intmax_t i) {
         out.stream() << i;
         return out;
      }

      inline Output& operator<<(Output& out, double x) {
         out.stream() << x;
         return out;
      }

      template <typename Iter>
      inline Output&
      pretty_range(Output& out, Iter f, Iter l) {
         out.stream() << '{';
         if (f != l) {
            out.stream() << ' ' << *f++;
            while (f != l)
               out.stream() << ", " << *f++;
         }
         out.stream() << " }";
         return out;
      }
   }
}

#endif  // LIZ_OUTPUT_INCLUDED
