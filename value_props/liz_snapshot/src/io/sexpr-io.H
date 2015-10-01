// Copyright (C) 2012, Texas A&M University.
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
//

#ifndef LIZ_SEXPR_IO_INCLUDED
#define LIZ_SEXPR_IO_INCLUDED

#include "Output.H"

namespace liz {
   namespace io {
      // Format an offset value as s-expression.
      inline Output&
      sexpr(Output& out, offset_type i) {
         return out << i;
      }

      // Format a literal as s-expression
      inline Output&
      sexpr(Output& out, const std::string& s) {
         return out << s;
      }

      // Format the operand of a unary structure as s-expression.
      template<typename T>
      inline Output&
      sexpr(Output& out, const structure::unary<T>& x) {
         return sexpr(out, x.operand());
      }

      // -- length of s-expression format of an entity.
      inline int
      sexpr_length(const std::string& s) {
         return s.size();
      }

      inline int
      sexpr_length(offset_type) {
         return -1;                // FIXME
      }

      template<typename T>
      inline int
      sexpr_length(const T&) {
         return -1;                // we don't know.
      }

      // -- is an s-expression representation atomic?
      inline bool
      is_atomic(const std::string&) {
         return true;
      }

      inline bool
      is_atomic(offset_type) {
         return false;             // FIXME
      }

      // -- Can operands of a binary structure be formatted as one-liner?
      const int max_line_length = 80;
      template<typename T, typename U>
      bool
      can_go_one_liner(io::Output& out, const structure::binary<T, U>& x) {
         return is_atomic(x.first())
            and is_atomic(x.second())
            and (sexpr_length(x.first()) + sexpr_length(x.second()) + 2
                 < max_line_length - out.line_lenght());
      }

      // Format operands of a binary structure as s-expressions.
      template<typename T, typename U>
      Output&
      sexpr(Output& out, const structure::binary<T, U>& x) {
         if (can_go_one_liner(out, x)) {
            sexpr(out, x.first()) << ' ';
            return sexpr(out, x.second());
         }
         out.nibble(tab_unit) << newline() << indent();
         sexpr(out, x.first()) << newline() << indent();
         sexpr(out, x.second());
         out.nibble(-tab_unit);
         return out;
      }

      // Format operands of a ternary structure.
      template<typename T, typename U, typename V>
      Output&
      sexpr(Output& out, const structure::ternary<T, U, V>& x) {
         out.nibble(tab_unit) << newline() << indent();
         sexpr(out, x.first()) << newline() << indent();
         sexpr(out, x.second()) << newline() << indent();
         sexpr(out, x.third());
         out.nibble(-tab_unit);
         return out;
      }
   

      

   }
}

#endif  // LIZ_SEXPR_IO_INCLUDED
