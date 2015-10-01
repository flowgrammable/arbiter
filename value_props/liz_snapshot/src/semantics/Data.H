// Copyright (C) 2009-2013, Texas A&M University.
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


#ifndef LIZ_DATA_INCLUDED
#define LIZ_DATA_INCLUDED

#include <liz/config>
#include <vector>

#ifdef HAVE_STDINT_H
#  include <stdint.h>
#endif
#ifdef HAVE_INTTYPES_H
#  include <inttypes.h>
#endif

#include <stddef.h>
#include <string>
#include <liz/utility>
#include <liz/Character>

namespace liz {
   // ----------
   // -- Data --
   // ----------
   // Objects of this type hold Liz values
   // Note:  the type `void' has no value, so internally
   // we use a null pointer of type 'Data*' to signal
   // 'no value' (especially for void-returning functions).
   namespace Data {
      enum class Mode {
         Void,                  // no data to store at all
         Bool,                  // boolean data
         Int8,                  // signed 8-bit integer data
         Uint8,                 // unsigned 8-bit integer data
         Int16,                 // signed 16-bit integer data
         Uint16,                // unsigned 16-bit integer data
         Int32,                 // signed 32-bit integer data
         Uint32,                // unsigned 32-bit integer data
         Int64,                 // signed 64-bit integer data
         Uint64,                // unsigned 64-bit integer data
         Uint,                  // Arbitrary width uint.
         Byte,                  // dedicated 8 byte data.
         Char,                  // character data
         Int,                   // natural width machine integer data
         Long,                  // long machine integer data
         Dfloat,                // double precision floating point data
         String,                // string literal data
         Symbol,                // symbol data
         Pointer,               // generic pointer-to-data data
         Code,                  // generic pointer-to-function data
         Record,                // heterogeneous aggregate data
         Union,                 // union data
         Array,                 // heterogeneous aggregate data
         max                    // number of modes.
      };

      using Abstract = void*;

      union Value {
         Value(bool b) : ival(b) { }
         Value(byte b) : ival(b) { }
         Value(Character c) : cval(c) { }
         Value(intmax_t i = 0) : ival(i) { }
         Value(uintmax_t i) : uival(i) { }
         Value(double f) : fval(f) { }
         Value(Symbol s) : sval(s) { }
         template<typename T>
         Value(T* p) : pval(Abstract(p)) { }
         Value(std::vector<Data::Value>* a) : aval(a) { }

         operator bool() const { return bool(ival); }
         operator byte() const { return byte(ival); }
         operator Character() const { return cval; }
         operator intmax_t() const { return ival; }
         operator uintmax_t() const { return uival; }
         operator double() const { return fval; }
         operator Symbol() const { return sval; }
         template<typename T>
         operator T*() const { return static_cast<T*>(pval); }
         operator std::vector<Data::Value>*() const { return aval; }

      private:
         Character cval;
         intmax_t ival;
         uintmax_t uival;
         double fval;
         Symbol sval;
         Abstract pval;
         std::vector<Data::Value>* aval;
      };

      using Location = void*;
      using ValueLocation = Value*;
      using CodePointer = void (*)();
      using ArrayHandle = std::vector<Data::Value>*;
      using Array = std::vector<Data::Value>;

      inline ValueLocation
      to_value_location(Value v) {
         return static_cast<Value*>(Location(v));
      }

      // ---------------
      // -- Formatter --
      // ---------------
      // Type of functions that print values of builtin type on
      // the standard output.
      using FormatFunction = void (*)(Location, std::ostream&, Value);

      template<typename F>
      struct Closure {
         F fun;
         Location env;
      };

      template<typename F>
      inline Closure<F>
      closure(F f) {
         const Closure<F> c = { f, 0 };
         return c;
      }

      template<typename F, typename T>
      inline Closure<F>
      closure(F f, T* e) {
         const Closure<F> c = { f, Location(e) };
         return c;
      }
      
   // -- Formatter --
      using Formatter = Closure<FormatFunction>;

      // --------------
      // -- Property --
      // --------------
      struct Property {
         Mode mode;                         // access mode
         size_t precision;                  // sizeof in bits
         size_t alignment;                  // alignment in bytes.
         Formatter formatter;               // function to format data
      };
      
      // Return a pointer to builtin data mode info, if present; null otherwise.
      const Property* property(Mode);
      
   }
}

#endif  // LIZ_DATA_INCLUDED
