// Copyright (C) 2009-2012, Texas A&M University.
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

#ifndef LIZ_ENVIRONMENT_INCLUDED
#define LIZ_ENVIRONMENT_INCLUDED

// This module defines data structures for scope management, both at
// compile time and run time.  Liz (like C++) follows a lexical scope
// discipline; therefore the mechanisms of compile-time and runtime scopes
// are quite similar.  The two kind of scopes differ essentially only in
// the nature of the values they hold.  Consequently, we use a parameterized
// data structure.
// 
// At an abstract level, a scope is a map from symbols to values.  The
// C++ data structure std::map appears a natural candidate.  In fact, that
// is what has been used since the very early (baby) steps of Liz.
// Liz, like C++, has the notion of `overload set', i.e. a collection
// of functions of differing types in a given scope.  The possibility of
// defining the same symbol with different types (and values) in the same
// scope alters the simple view of a scope as a `map from symbols
// to values.'
// 
// Although the notion of `overloading' (therefore `overload set') is
// central to C++, the C++ standard never considers it as a first class
// abstraction -- e.g. one cannot pass an overload set as an argument
// in a function call.  For a while, Liz directly represented overload
// sets as values, e.g. first class entities.  That representation nicely
// co-existed with the simple view of scope (at elaboration phase).
// However, the code for runtime binding of symbols appeared slightly more
// complicated.  So, it was decided to use multi-maps to represent
// scopes, and abandon `overload set as first class values.'
// Rather instructions will be generated to construct and deconstruct
// overload sets where needed.  Hopefully, we'd still maintain the observable
// behaviour that `overload sets' are first class objects in Liz.

#include <new>
#include <list>
#include <map>
#include <set>
#include <stdlib.h>

#include <liz/utility>
#include "Data.H"

namespace liz {
   // ----------------
   // -- Contour --
   // ----------------
   struct Contour {
      enum Kind {
         lexical,           // the envinronment is lexical, e.g. block
         dynamic,           // the environment is dynamic, e.g. frame
         freestanding
      };

      Kind kind() const { return knd; }

   protected:
      explicit Contour(Kind k) : knd(k) { }
      
   private:
      const Kind knd;      // what kind of contour is this environment
   };
   
   // -------------
   // -- Binding --
   // -------------
   template<typename S, typename V>
   struct Binding : std::pair<S, V> {
      using name_type = S;
      using value_type = V;

      name_type name() const { return this->first; }
      const value_type& value() const { return this->second; }
      value_type& value() { return this->second; }
      Binding(name_type n, const value_type& v)
            : std::pair<name_type, value_type>(n, v)
      { }
   };

   // Helper function.  Constructs a binding given a name and its value.
   template<typename S, typename V>
   inline Binding<S, V>
   make_binding(S n, const V& v) {
      return Binding<S, V>(n, v);
   }

   // Function object class for an ordering over bindings.  Bindings
   // are ordered by their names.
   struct BindingOrdering {
      template<typename X>
      bool
      operator()(const X& lhs, const X& rhs) const {
         using N = typename X::name_type;
         return std::less<N>()(lhs.name(), rhs.name());
      }
   };
   
   // -----------------
   // -- Environment --
   // -----------------
   // Objects of this data type binds symbols to values
   template<typename X>
   struct Environment : Contour, std::multiset<X, BindingOrdering> {
      struct BindingSet;
      using Base = std::multiset<X, BindingOrdering>;
      using iterator = typename Base::iterator;
      using name_type = typename X::name_type;
      using value_type = typename X::value_type;
      using binding_type = typename Base::value_type;

      explicit Environment(Kind k) : Contour(k) { }

      // Bing a symbol to a value in this environment and return
      // a pointer to the storage holding that value.  Raise an
      // exception in case of redefinition.
      binding_type* bind(name_type, const value_type&);

      // Return the overload set of values bound to a given symbol
      BindingSet lookup(name_type) const;
      template<typename T>  binding_type* select(name_type, T) const;
   };

   // -----------------------------
   // -- Environment::BindingSet --
   // -----------------------------
   template<typename X>
   struct Environment<X>::BindingSet :
      std::pair<typename Environment<X>::iterator,
                typename Environment<X>::iterator> {
      using Base = std::pair<typename Environment<X>::iterator,
                             typename Environment<X>::iterator>;
      using iterator = typename Environment<X>::iterator;
      using binding_type = typename Environment<X>::binding_type;
      
      BindingSet() : Base() { }
      explicit BindingSet(const Base& b) : Base(b) { }
      
      iterator begin() const {
         return this->first;
      }

      iterator end() const {
         return this->second;
      }
      
      bool empty() const {
         return begin() == end();
      }

      // Note: convenient function for use in conditionals
      explicit operator bool() const {
         return not empty();
      }

      std::size_t size() const {
         return std::distance(begin(), end());
      }

      template<typename T>
      binding_type* select(T t) {
         for (iterator p = begin(); p != end(); ++p)
            if (p->value().type() == t)
               return const_cast<binding_type*>(&*p);
         return nullptr;
      }
   };

   template<typename X>
   typename Environment<X>::BindingSet
   Environment<X>::lookup(name_type name) const {
      return BindingSet(this->equal_range(make_binding(name, value_type())));
   }

   template<typename X>
   template<typename T>
   typename Environment<X>::binding_type*
   Environment<X>::select(name_type name, T t) const {
      return lookup(name).select(t);
   }


   template<typename X>
   typename Environment<X>::binding_type*
   Environment<X>::bind(name_type s, const value_type& v) {
      // If `s' is already bound with the type of `t', then
      // this must be a mistake.
      if (lookup(s).select(v.type()))
         redefinition_error("redefinition of symbol " + quote(s));
      
      return const_cast<binding_type*>(&*this->insert(make_binding(s, v)));
   }

   // ------------
   // -- EnvRef --
   // ------------
   template<typename E>
   struct EnvRef {
      using BindingSet = typename E::BindingSet;
      using name_type = typename E::name_type;
      using value_type = typename E::value_type;
      using binding_type = typename E::binding_type;
      using const_iterator = typename E::const_iterator;

      explicit EnvRef(E* e) : env(e) { }

      Contour::Kind kind() const { return env->kind(); }
      E* operator->() const { return env; }
      BindingSet lookup(name_type s) { return env->lookup(s); }
      E* base() const { return env; }
      const_iterator begin() const { return base()->begin(); }
      const_iterator end() const { return base()->end(); }
      
   private:
      E* const env;
   };

   // -----------
   // -- Stack --
   // -----------
   // Representation of the environment stack during
   template<typename X, typename C = Environment<X>>
   struct Stack : protected std::list<C> {
      using iterator = std::reverse_iterator<typename std::list<C>::iterator>;
      using BindingSet = typename C::BindingSet;
      using name_type = typename C::name_type;
      using value_type = typename C::value_type;
      using binding_type = typename C::binding_type;
      using std::list<C>::size;
      using std::list<C>::empty;

      iterator begin() { return this->rbegin(); }
      iterator end() { return this->rend(); }

      C& top() { return this->back(); }
      C& bottom() { return this->front(); }

      C& current_env() { return top(); }
      C& global_env() { return bottom(); }

      BindingSet lexical_lookup(name_type);

      // Like `lexical_lookup' except that the symbol is assumed bound in
      // this environment.  Raise an exception if not.
      BindingSet get_binding(name_type name) {
         BindingSet set = lexical_lookup(name);
         if (set.empty())
            evaluation_error("symbol " + quote(name) + " is not defined");
         return set;
      }

      template<typename T>
      binding_type*
      select_if_can(name_type name, T t) {
         return lexical_lookup(name).select(t);
      }

      template<typename T>
      binding_type*
      select(name_type name, T t) {
         auto d = select_if_can(name, t);
         if (d == nullptr)
            evaluation_error("there is no " + quote(show(*t))
                             + " binding of symbol " + quote(name));
         return d;
      }

      // Return an ambiguous binding for a symbol, if any. Or else.
      value_type* get_unambiguous_binding(name_type);
   };

   template<typename X, typename C>
   typename Stack<X, C>::BindingSet
   Stack<X, C>::lexical_lookup(name_type sym) {
      iterator last = this->end();
      for (auto& c : *this) {
         BindingSet set = c.lookup(sym);
         if (not set.empty())
            return set;
         if (c.kind() != Contour::lexical)
            break;
      }
      return BindingSet();
   }

   template<typename X, typename C>
   typename Stack<X, C>::value_type*
   Stack<X, C>::get_unambiguous_binding(name_type name) {
      BindingSet set = lexical_lookup(name);
      const std::size_t n = set.size();
      if (n == 0)
         system_error("there is no definition of "
                      + quote(name) + " in scope");
      else if (n > 1)
         system_error("ambiguous reference to " + quote(name));
      return const_cast<value_type*>(&set.begin()->value());
   }
}

#endif  // LIZ_ENVIRONMENT_INCLUDED
