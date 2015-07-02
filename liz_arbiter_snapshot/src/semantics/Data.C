// Copyright (C) 2012, Texas A&M University
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


#include <limits.h>
#include <iostream>
#include "Data.H"

namespace liz {
   namespace Data {
      // -- void
      static void
      format_void(Location, std::ostream&, Value) {
      }

      // -- bool
      static void
      format_bool(Location, std::ostream& os, Value v) {
         os << std::boolalpha << bool(v);
      }
      
      // -- byte
      static void
      format_byte(Location, std::ostream& os, Value v) {
         os << int(byte(v));
      }
      
      template<typename S>
      static void
      fmt_scalar(Location, std::ostream& os, Value v) {
         os << S(intmax_t(v));
      }

      // -- unsigned int max
      static void
      fmt_uintmax_t(Location, std::ostream& os, Value v) {
         os << uintmax_t(v);
      }
      
      // -- double
      static void
      format_double(Location, std::ostream& os, Value v) {
         os << double(v);
      }
      
      // -- string
      static void
      format_string(Location, std::ostream& os, Value v) {
         Symbol s = v;
         os << '"' << s.string() << '"';
      }
      
      // ---------------------------------------
      // -- basic machine datatype properties --
      // ---------------------------------------
      static const Property basic_modes[] = {
         { Mode::Void, 0, 0, { format_void, 0} },
         { Mode::Bool, CHAR_BIT, alignof (bool), { format_bool, 0 } },
         { Mode::Int8, 8, alignof(int8_t), { fmt_scalar<int8_t>, 0 } },
         { Mode::Uint8, 8, alignof(int8_t), { fmt_scalar<uint8_t>, 0 } },
         { Mode::Int16, 16, alignof(int16_t), { fmt_scalar<int16_t>, 0 } },
         { Mode::Uint16, 16, alignof(uint16_t), { fmt_scalar<uint16_t>, 0 } },
         { Mode::Int32, 32, alignof(int32_t), { fmt_scalar<int32_t>, 0 } },
         { Mode::Uint32, 32, alignof(uint32_t), { fmt_scalar<uint32_t>, 0 } },
         { Mode::Int64, 64, alignof(int64_t), { fmt_scalar<int64_t>, 0 } },
         { Mode::Uint64, 64, alignof(uint64_t), { fmt_scalar<uint64_t>, 0 } },
         { Mode::Uint, sizeof(uintmax_t) * CHAR_BIT,
           alignof(int), { fmt_uintmax_t, 0 } },
         { Mode::Byte, 8, alignof(int8_t), { format_byte, 0 } },
         { Mode::Char, CHAR_BIT, alignof(char), { fmt_scalar<char>, 0 } },
         { Mode::Int, sizeof(int) * CHAR_BIT,
           alignof(int), { fmt_scalar<int>, 0 } },
         { Mode::Long, sizeof(long) * CHAR_BIT,
           alignof(long), { fmt_scalar<long>, 0 } },
         { Mode::Dfloat,  sizeof(double) * CHAR_BIT,
           alignof(double), { format_double, 0 } },
         
         { Mode::String, sizeof(Location) * CHAR_BIT,
           alignof(Location), { format_string, 0 } },
         { Mode::Pointer, sizeof(Location) * CHAR_BIT,
           alignof(Location), { fmt_scalar<Location>, 0 } },
         { Mode::Code,    sizeof(CodePointer) * CHAR_BIT,
           alignof(CodePointer), { fmt_scalar<CodePointer>, 0 } },
      };

      const Property*
      property(Data::Mode m) {
         for (int i = 0; i < length(basic_modes); ++i)
            if (m == basic_modes[i].mode)
               return &basic_modes[i];
         return 0;
      }
   }
}
