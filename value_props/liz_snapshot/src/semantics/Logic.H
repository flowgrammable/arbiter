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

#ifndef LIZ_LOGIC_INCLUDED
#define LIZ_LOGIC_INCLUDED

#include <iterator>
#include <list>
#include "Expression.H"

namespace liz {
   // -----------------
   // -- EqClassNode --
   // -----------------
   template<typename T>
   struct EqClassNode {
      EqClassNode(const T& t) : data(t), next() { }
      T data;
      EqClassNode* next;
   };

   template<typename T> struct EqClass;

   // ---------------------
   // -- EqClassIterator --
   // ---------------------
   template<typename T>
   struct EqClassIterator : std::iterator<std::forward_iterator_tag, const T> {

      const T& operator*() const { return node->data; }
      const T* operator->() const { return &node->data; }
      EqClassIterator& operator++() {
         node = node->next;
         return *this;
      }

      EqClassIterator operator++(int) {
         EqClassNode<T>* tmp = node;
         node = node->next;
         return tmp;
      }

      friend bool operator==(EqClassIterator lhs, EqClassIterator rhs) {
         return lhs.node == rhs.node;
      }

      friend bool operator!=(EqClassIterator lhs, EqClassIterator rhs) {
         return lhs.node != rhs.node;
      }

   private:
      EqClassNode<T>* node;
      EqClassIterator(EqClassNode<T>* n) : node(n) { }

      friend class EqClass<T>;
   };

   struct EquivalenceQuotient;

   // -------------
   // -- EqClass --
   // -------------
   template<typename T>
   struct EqClass : private std::allocator<EqClassNode<T>> {
      using Base = std::allocator<EqClassNode<T>>;
      using iterator = EqClassIterator<T>;

      EqClass(const T& t) : rep() { insert(t); }

      EqClass(const EqClass& c) : Base(), rep() {
         for (iterator p = c.begin(); p != c.end(); ++p)
            insert(*p);
      }

      ~EqClass() {
         while (rep != nullptr) {
            EqClassNode<T>* p = rep;
            rep = rep->next;
            p->~EqClassNode<T>();
            this->deallocate(p, 1);
         }
      }

      EqClassIterator<T> begin() const { return rep; }
      EqClassIterator<T> end() const { return nullptr; }

      const T& leader() const { return *begin(); }
      std::list<T>& users() { return dep; }

      EqClassIterator<T> insert(const T& t) {
         EqClassNode<T>* n = new(this->allocate(1)) EqClassNode<T>(t);
         // Put the representative first.
         if (is_closed(t) or rep == nullptr) {
            n->next = rep;
            rep = n;
         }
         else  {
            n->next = rep->next;
            rep->next = n;
         }
         return n;
      }

      EqClassNode<T>* rep;
      std::list<T> dep;
      friend EquivalenceQuotient;
   };

   // -------------------------
   // -- EquivalenceQuotient --
   // -------------------------
   struct EquivalenceQuotient : std::list<EqClass<Elaboration>> {
      void assume(ExpressionFactory&, Elaboration, Elaboration);
   };

   // -- Debug support for equivalences.
   void print_equivalences(const EquivalenceQuotient&, std::ostream&);

   // -- Close a term given an equivalence closure.
   Elaboration
   modulo_equiv(ExpressionFactory&, const EquivalenceQuotient&, Elaboration);
   TypeElaboration
   modulo_equiv(ExpressionFactory&, const EquivalenceQuotient&,
                TypeElaboration);
   FunctionElaboration
   modulo_equiv(ExpressionFactory&, const EquivalenceQuotient&,
                FunctionElaboration);

   // -- True iff the equivalence closure is consistent.
   //  Consistency is defined by the following axioms.
   //    A1) No structural violations.
   //          Even if equivalent terms are not closed, their constructors must
   //          match.
   //    A2) No ground type violations.
   //          Names of basic types must be equal.
   //    A3) Kinds must match.
   //          Terms to terms and types to types.
   //  These are satisfied iff there exists a substitution
   bool consistent(const EquivalenceQuotient&);

   // --------------------
   // -- VarIndexedList --
   // --------------------
   struct VarIndexedList {
      using ElabRef = std::list<Elaboration>::iterator;
      using Link = std::pair<const Expression*, std::vector<ElabRef>>;

      void push_back(Elaboration);
      void erase_if_has(Elaboration);

      std::list<Elaboration>::const_iterator begin()
        { return elabs.begin(); }
      std::list<Elaboration>::const_iterator end()
        { return elabs.end(); }
      std::list<Elaboration>::const_iterator begin() const
        { return elabs.cbegin(); }
      std::list<Elaboration>::const_iterator end() const
        { return elabs.cend(); }
      std::list<Elaboration>::const_iterator cbegin() const
        { return elabs.cbegin(); }
      std::list<Elaboration>::const_iterator cend() const
        { return elabs.cend(); }

      std::size_t size() const { return elabs.size(); }

      std::list<Elaboration> elabs;
      std::vector<Link> links;
   private:
      void push_link(Elaboration, ElabRef);
      const std::vector<ElabRef>* get_refs(Elaboration);
   };

   // -------------------
   // -- AssumptionSet --
   // -------------------
   // -- Node in a given equivalence class of expression.  We keep these
   // -- nodes separate from expression node themselses, because nodes
   // -- may be involved simultaneously in several equivalence classes, but
   // -- simulatenously in the same scope.
   struct AssumptionSet : VarIndexedList {
      EquivalenceQuotient equivalences;
      std::vector<Elaboration> local_exprs;
      bool is_local(Elaboration) const;
      void assume_local(Elaboration);
      bool contains(Elaboration) const;
      void discharge(Elaboration e);
      void assume(Elaboration e);
   };

   // -- Debug support for assumptions.
   void print_assumption_set(const AssumptionSet&, std::ostream&);
}

#endif  // LIZ_LOGIC_INCLUDED
