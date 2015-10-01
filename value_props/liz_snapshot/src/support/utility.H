// Copyright (C) 2012-2013, Texas A&M University
// Copyright (C) 2014, Gabriel Dos Reis.
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

#ifndef LIZ_UTILITY_INCLUDED
#define LIZ_UTILITY_INCLUDED

#include <stddef.h>
#include <stdint.h>
#include <string>
#include <sstream>
#include <functional>
#include <iterator>

namespace liz {
   // -- Data types for labeling positions and counting items.
   using Ordinal = std::size_t;
   using Cardinal = std::size_t;

   namespace defaults {
      // Generate definition for != assuming existence of ==.
      template<typename T>
      struct neq {
         friend bool operator!=(T x, T y) { return not(x == y); }
      };

      // Generate definitions for ordering functions assuming <.
      template<typename T>
      struct ordering {
         friend bool operator<=(T x, T y) { return not(y < x); }
         friend bool operator>(T x, T y) { return y < x; }
         friend bool operator>=(T x, T y) { return not(x < y); }
      };
   }

   namespace iterator {
      // -- Read-only iterator over a sequence with random access capabilities.
      template<typename Seq>
      struct basic
         : std::iterator<std::random_access_iterator_tag,
                         const typename Seq::value_type>,
         defaults::neq<basic<Seq>>, defaults::ordering<basic<Seq>> {
         using reference = typename Seq::const_reference;
         using pointer = typename Seq::const_pointer;
         using difference_type = typename Seq::difference_type;

         reference operator*() const { return seq->at(idx); }
         pointer operator->() const { return &seq->at(idx); }
         reference operator[](Ordinal i) const { return seq->at(idx + i); }

         basic& operator++() { ++idx; return *this; }
         basic operator++(int) { auto t = *this; ++idx; return t; }
         basic& operator--() { --idx; return *this; }
         basic operator--(int) { auto t = *this; --idx; return t; }

         basic& operator+=(Cardinal n) { idx += n; return *this; }
         basic& operator-=(Cardinal n) { idx -= n; return *this; }
         basic operator+(Cardinal n) const { return { seq, idx + n }; }
         basic operator-(Cardinal n) const { return { seq, idx - n }; }
         difference_type operator-(basic that) const { return idx - that.idx; }
         

         bool operator==(basic that) const { return idx == that.idx; }
         bool operator<(basic that) const { return idx < that.idx; }
         
      private:
         friend Seq;
         const Seq* seq;
         Ordinal idx;
         constexpr basic(const Seq* s, Ordinal i) : seq(s), idx(i) { }
      };
   }

   namespace range {
      template<typename T>
      struct semi_open_interval {
         struct iterator : defaults::neq<iterator> {
            constexpr iterator(T p) : val(p) { }
            iterator& operator++() { ++val; return *this; }
            iterator operator++(int) { auto t = *this; ++val; return t; }
            T operator*() const { return val; }
            bool operator==(iterator p) const { return val == p.val; }
         private:
            T val;
         };

         constexpr semi_open_interval(T f, T l) : first(f), last(l) { }
         constexpr iterator begin() const { return { first }; }
         constexpr iterator end() const { return { last }; }
      private:
         const T first;
         const T last;
      };

      template<typename T>
      constexpr semi_open_interval<T>
      semi_open(T f, T l) {
         return { f, l };
      }

      template<typename T>
      constexpr semi_open_interval<T>
      naturals_less_than(T n) {
         return { { }, n };
      }
   }

   // -- Path --
   // Type of a file or drectory pathname in a file system
   // FIXME: This definition is only temporary; it is not accurate.
   using Path = std::string;

   // Enclose a text in a quote.
   std::string quote(const std::string&);

   // -- Symbol --
   struct Symbol : defaults::neq<Symbol>, defaults::ordering<Symbol> {
      constexpr Symbol() : rep() { }
      constexpr explicit Symbol(const std::string* s) : rep(s) { }
      const std::string& string() const { return *rep; }
      // Convenient conversion function for use in contextual conditionals
      explicit operator bool() const { return rep != nullptr; }

      friend bool operator==(Symbol, Symbol);
      friend bool operator<(Symbol, Symbol);
   private:
      const std::string* rep;
   };

   std::ostream& operator<<(std::ostream&, Symbol);

   inline bool
   operator==(Symbol x, Symbol y) {
      return x.rep == y.rep;
   }

   inline bool
   operator<(Symbol x, Symbol y) {
      using Rep = const std::string*;
      return std::less<Rep>()(x.rep, y.rep);
   }

   // quote the string representation of a symbol.
   inline std::string
   quote(Symbol s) {
      return quote(s.string());
   }

   namespace debug {
      enum Verbosity {
         nothing        = 0,
         lexing         = 1 << 0,
         parsing        = 1 << 1,
         scope          = 1 << 2,
         overload       = 1 << 3,
         eq             = 1 << 4, // equational reasoning
         logic          = 1 << 5,
         codegen        = 1 << 6,
         matching       = 1 << 7, // pattern matching
         instantiation  = 1 << 8,
         substitution   = 1 << 9,
         everything     = ~0
      };

      constexpr bool
      has(Verbosity flags, Verbosity bit) {
         return (flags & bit) != 0;
      }

      inline Verbosity&
      operator|=(Verbosity& lhs, Verbosity rhs) {
         return lhs = Verbosity(int(lhs) | int(rhs));
      }
   }

   namespace translation {
      // Sort of action that a Liz translator instance should perform
      enum Action {
         unknown,               // don't guess what to do yet
         eval,                  // run the interpreter
         compile                // run the compiler
      };
   }

   // -----------
   // -- Flags --
   // -----------
   // Translator parameters
   struct Flags {
      debug::Verbosity verbosity;
      translation::Action action;
      Path output_file;
      Path stdlib;
   };

   // -- helper class for structural abstractions --
   namespace structure {
      // unary type structure
      template<typename T>
      struct unary {
         explicit constexpr unary(const T& t) : arg(t) { };
         constexpr const T& operand() const { return arg; }
      private:
         const T arg;
      };

      // binary type structure
      template<typename T, typename U = T>
      struct binary {
         constexpr binary(const T& t, const U& u) : arg0(t), arg1(u) { };
         constexpr const T& first() const { return arg0; }
         constexpr const U& second() const { return arg1; }
      private:
         const T arg0;
         const U arg1;
      };

      // ternary type structure
      template<typename T, typename U = T, typename V = U>
      struct ternary {
         constexpr ternary(const T& t, const U& u, const V& v)
               : arg0(t), arg1(u), arg2(v) { }
         constexpr const T& first() const { return arg0; }
         constexpr const U& second() const { return arg1; }
         constexpr const V& third() const { return arg2; }
      private:
         const T arg0;
         const U arg1;
         const V arg2;
      };
   }

   // Datatype for the fundamental unit of storage.
   using byte = unsigned char;
   
   // Type of pointers to data.
   using Pointer = void*;
   
   // -- useful functions that are part of C++0x, but not of C++03.
   // return the compile-time length of a static C-array
   template<typename T, int N>
   constexpr inline int
   length(const T (&)[N]) {
      return N;
   }

   // Return a string representation of the object `t', assumed to
   // be streamable.
   template<typename T>
   std::string show(const T& t) {
      std::ostringstream os;
      os << t;
      return os.str();
   }

   // -- comparator of objects sorted by their `name' members
   struct named_entity_lt {
      template<typename T>
      bool operator()(const T& lhs, const T& rhs) const {
         return std::less<const std::string*>()(lhs.name(), rhs.name());
      }
   };

   // ----------------
   // -- BasicError --
   // ----------------
   // Base class for error classes that appear during the processing
   // of an input to the Liz interpreter.
   struct BasicError {
      explicit BasicError(const std::string&);

      // issue a diagnostic on the output stream.
      void issue_on(std::ostream&) const;
   protected:
      virtual void disclose_location_on(std::ostream&) const;
      virtual void format_message(std::ostream&) const;
      const std::string msg;
   };

   // -------------------
   // -- InternalError --
   // -------------------
   // Exceptions of this class  report Liz internal inconsistency errors
   struct InternalError : BasicError {
      explicit InternalError(const std::string&);
   };

   // ---------------
   // -- FileError --
   // ---------------
   struct FileError : BasicError {
      explicit FileError(const std::string&);
   };

   // -----------------
   // -- SystemError --
   // -----------------
   struct SystemError : BasicError {
      explicit SystemError(const std::string&);
   };

   // ---------------------
   // -- EvaluationError --
   // ---------------------
   struct EvaluationError : BasicError {
      explicit EvaluationError(const std::string&);
   };

   // -----------------------
   // -- RedefinitionError --
   // -----------------------
   struct RedefinitionError : EvaluationError {
      explicit RedefinitionError(const std::string&);
   };


   // -- Utility functions to raise expressions.
   void internal_error(const std::string&);
   void system_error(const std::string&);
   void cannot_open(const std::string&);
   void filesystem_error(const std::string&);
   void evaluation_error(const std::string&);
   void redefinition_error(const std::string&);

   // Tools used to compile and link intermediate files
   namespace tools {
      // ----------------
      // -- SystemName --
      // ----------------
      struct SystemName {
         const char* cpu;
         const char* vendor;
         const char* os;
      };

      namespace build {
         // canonical name of build system
         extern const SystemName system_name;
      }

      namespace host {
         // canonical name of host system
         extern const SystemName system_name;

         // name of the C++ compiler
         extern const char* const cxx_compiler;

         // flags used to invoke the C++ compiler
         extern const char* const cxx_flags;
         
         // flags used to invoke the C++ linker
         extern const char* const cxx_ldflags;

         // Libraries used to invoke the C++ linker
         extern const char* const cxx_libs;
      }

      namespace target {
         // canonical name of target system
         extern const SystemName system_name;

         extern const char* const assembler;
         extern const char* const linker;
      }
   }   

   template<typename S, typename T>
   inline const S& as(const T& t) {
      return t;
   }
}


#endif  // LIZ_UTILITY_INCLUDED
