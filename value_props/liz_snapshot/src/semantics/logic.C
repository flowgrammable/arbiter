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

#include <iostream>
#include <algorithm>
#include "Elaborator.H"

namespace liz {
   void
   print_assumption_set(const AssumptionSet& s, std::ostream& os) {
      os << "\n=================== Assumption Set ===================\n";
      for (auto p = s.begin(); p != s.end(); ++p)
         os << '\t' << show(p->code()) << std::endl;
      os << "\n======================================================\n\n";
   }

   // -- EquivalenceQuotient --
   static Elaboration
   get_rep(const EquivalenceQuotient& eqs, Elaboration e) {
      for (auto set: eqs)
         for (auto f: set)
            if (structural_equivalence(e, f.code()))
               return set.leader();
      return { };
   }

   static std::list<EqClass<Elaboration>>::iterator
   get_eq_class(EquivalenceQuotient& eqs, Elaboration e) {
      auto eq = eqs.begin();
      for (; eq != eqs.end(); ++eq)
         for (auto elt: *eq)
            if (structural_equivalence(e, elt.code()))
               return eq;
      eqs.push_back(e);
      return --eqs.end();
   }

   // True iff  `e0' is a subexpression of `e1'.
   // FIXME: Implement.
   bool is_subexpr(Elaboration, Elaboration) { return false; }

   // Displace the elements of the class pointed to by `src' to
   // the class pointed to by `dst'.
   static void
   displace(EquivalenceQuotient& eqs, EquivalenceQuotient::iterator src,
            EquivalenceQuotient::iterator dst)
   {
      auto* n = dst->rep;
      while (n->next != nullptr)
         n = n->next;
      n->next = src->rep;
      src->rep = nullptr;
      eqs.erase(src);
   }

   static void
   merge_classes(EquivalenceQuotient& eqs, EquivalenceQuotient::iterator c1,
                 EquivalenceQuotient::iterator c2)
   {
      // Leader election.
      if (c1 != c2) {
         // Prefer reductions first.
         if (is_subexpr(c1->leader(), c2->leader()))
            displace(eqs, c2, c1);
         // Prefer closed expressions second.
         else if (is_closed(c2->leader()))
            displace(eqs, c1, c2);
         // Prefer symbolic values to aliases.
         else if (is<SymbolicValue>(c2->leader().code()))
            displace(eqs, c1, c2);
         else
            displace(eqs, c1, c2);
      }
   }

   TypeElaboration
   modulo_equiv(ExpressionFactory& fact, const EquivalenceQuotient& eqs,
         TypeElaboration te)
   {
      auto e = modulo_equiv(fact, eqs, (Elaboration)te);
      if (auto t = is<Type>(e.code()))
         return { te.type(), t };
      internal_error("equivalence normalized a type to a non-type");
      return  { };
   }

   FunctionElaboration
   modulo_equiv(ExpressionFactory& fact, const EquivalenceQuotient& eqs,
         FunctionElaboration fe)
   {
      auto e = modulo_equiv(fact, eqs, (Elaboration)fe);
      fe.code(e.code());
      return fe;
   }

   Elaboration
   modulo_equiv(ExpressionFactory& factory, const EquivalenceQuotient& eqs,
         Elaboration e)
   {
      if (auto rep = get_rep(eqs, e))
         return rep;

      struct V : Expression::Visitor {
         ExpressionFactory& factory;
         const EquivalenceQuotient& eqs;
         Elaboration result;
         V(ExpressionFactory& fact, const EquivalenceQuotient& eqs,
           Elaboration e)
            : factory(fact), eqs(eqs), result(e) { }
         void visit(const Expression&) { }
         void visit(const TypeExpression& x) {
            auto expr = modulo_equiv(factory, eqs, x.expr());
            result.code(factory.make_type_expression(expr));
         }
         void visit(const Plus& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_plus(function, lhs, rhs));
         }
         void visit(const Dash& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_dash(function, lhs, rhs));
         }
         void visit(const Star& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_star(function, lhs, rhs));
         }
         void visit(const Slash& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_slash(function, lhs, rhs));
         }
         void visit(const Div& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_div(function, lhs, rhs));
         }
         void visit(const Quo& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_quo(function, lhs, rhs));
         }
         void visit(const Rem& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_rem(function, lhs, rhs));
         }
         void visit(const Mod& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_mod(function, lhs, rhs));
         }
         void visit(const Langle& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_langle(function, lhs, rhs));
         }
         void visit(const Rangle& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_rangle(function, lhs, rhs));
         }
         void visit(const Langleq& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_langleq(function, lhs, rhs));
         }
         void visit(const Rangleq& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_rangleq(function, lhs, rhs));
         }
         void visit(const Eqeq& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_eqeq(function, lhs, rhs));
         }
         void visit(const Excleq& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_excleq(function, lhs, rhs));
         }
         void visit(const And& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_and(function, lhs, rhs));
         }
         void visit(const Or& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_or(function, lhs, rhs));
         }
         void visit(const BinaryLogical& x) {
            auto op = x.operation();
            auto lhs = modulo_equiv(factory, eqs, x.lhs());
            auto rhs = modulo_equiv(factory, eqs, x.rhs());
            result.code(factory.build_logical(op, lhs, rhs));
         }
         void visit(const CallExpression& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            const std::size_t n = x.arguments().size();
            Arguments args(n);
            for (std::size_t i = 0; i != n; ++i)
               args[i] = modulo_equiv(factory, eqs, x.arguments()[i]);
            result.code(factory.build_call(function, args));
         }
         void visit(const Read& x) {
            auto address = modulo_equiv(factory, eqs, x.address());
            result.code(factory.build_read(address));
         }
         void visit(const Component& x) {
            auto whole = modulo_equiv(factory, eqs, x.whole());
            result.code(factory.build_component(whole, x.link_name()));
         }
         void visit(const Negate& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto argument = modulo_equiv(factory, eqs, x.argument());
            result.code(factory.build_negate(function, argument));
         }
         void visit(const Not& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto argument = modulo_equiv(factory, eqs, x.argument());
            result.code(factory.build_not(function, argument));
         }
         void visit(const Complement& x) {
            auto function = modulo_equiv(factory, eqs, x.function());
            auto argument = modulo_equiv(factory, eqs, x.argument());
            result.code(factory.build_complement(function, argument));
         }
      };
      V v { factory, eqs, e };
      e.code()->accept(v);
      if (auto rep = get_rep(eqs, v.result))
         return rep;
      return v.result;
   }

   // Normalize `e', equate the result with `e', and return the closed form.
   // FIXME: make this a `using'.
   static EquivalenceQuotient::iterator
   register_closed(ExpressionFactory& fact, EquivalenceQuotient& eqs,
                   Elaboration e)
   {
      auto eq_class = get_eq_class(eqs, e);
      auto ce = modulo_equiv(fact, eqs, e);
      auto ceq_class = get_eq_class(eqs, ce);
      merge_classes(eqs, ceq_class, eq_class);
      return ceq_class;
   }

   void EquivalenceQuotient::assume(ExpressionFactory& fact, Elaboration e0,
                                   Elaboration e1)
   {
      // Set equal to their closures.
      auto c0 = register_closed(fact, *this, e0);
      auto c1 = register_closed(fact, *this, e1);
      merge_classes(*this, c0, c1);
   }

   void print_equivalences(const EquivalenceQuotient& eqs, std::ostream& os) {
      os << "=================== Equivalences ===================\n";
      for (auto eq: eqs) {
         os << "    " << pretty(eq.leader().code()) << " <- {";
         bool first = true;
         for (auto e: eq) {
            if (first) os << ' ' << pretty(e.code());
            else       os << ", " << pretty(e.code());
         }
         os << " }\n";
      }
   }

   namespace {
      using Elabs = std::vector<Elaboration>;

      void get_reads(Elabs& elabs, Elaboration e) {
         if (e.code() == nullptr)
            return;
         struct V : Expression::Visitor {
            Elabs& elabs;
            const Type* t;
            V(Elabs& es, const Type* type) : elabs(es), t(type) { }
            void visit(const Expression&) {
               internal_error("`get_reads` got an unknown expression");
            }
            void visit(const Value&) { }
            void visit(const Type&) { }
            void visit(const Quote& x) { get_reads(elabs, x.body()); }
            void visit(const LinkName&) { }
            void visit(const Macro& x) { get_reads(elabs, x.replacement()); }
            void visit(const Function&) { }
            void visit(const Constructor&) { }
            void visit(const Instance&) { }
            void visit(const Assumption&) { }
            void visit(const BasicView&) { }
            void visit(const Postulate&) { }
            void visit(const Formal&) { }
            void visit(const SymbolicValue&) { }
            void visit(const NiladicBuiltinFunction&) { }
            void visit(const UnaryBuiltinFunction&) { }
            void visit(const BinaryBuiltinFunction&) { }
            void visit(const TernaryBuiltinFunction&) { }
            void visit(const DependentNiladicBuiltinFunction&) { }
            void visit(const DependentUnaryBuiltinFunction&) { }
            void visit(const DependentBinaryBuiltinFunction&) { }
            void visit(const DependentTernaryBuiltinFunction&) { }
            void visit(const Block& x) {
               for (auto arg: x.statements()) get_reads(elabs, arg);
            }
            void visit(const Lambda&) { }
            void visit(const Read& x) { get_reads(elabs, x.address()); }
            void visit(const Component& x) { elabs.push_back({ t, &x }); }
            void visit(const DotSelection& x) { get_reads(elabs, x.whole()); }
            void visit(const UnaryExpression& x) {
               get_reads(elabs, x.function());
               get_reads(elabs, x.argument());
            }
            void visit(const BinaryExpression& x) {
               get_reads(elabs, x.function());
               get_reads(elabs, x.lhs());
               get_reads(elabs, x.rhs());
            }
            void visit(const BinaryLogical& x) {
               get_reads(elabs, x.lhs());
               get_reads(elabs, x.rhs());
            }
            void visit(const CallExpression& x) {
               get_reads(elabs, x.function());
               for (auto arg: x.arguments())
                  get_reads(elabs, arg);
            }
         };
         V v(elabs, e.type());
         e.code()->accept(v);
      }

      std::vector<Elaboration> get_vars(Elaboration e) {
         Elabs reads;
         get_reads(reads, e);
         return reads;
      }
   }

   // FIXME: Unimplemented.
   bool consistent(const EquivalenceQuotient&) { return true; }

   // -- VarIndexedList --
   const std::vector<VarIndexedList::ElabRef>*
   VarIndexedList::get_refs(Elaboration var) {
      for (auto& link: links)
         if (structural_equivalence(var, link.first))
            return &link.second;
      return nullptr;
   }

   void
   VarIndexedList::push_link(Elaboration var, ElabRef i) {
      for (auto& link: links)
         if (structural_equivalence(var, link.first)) {
            link.second.push_back(i);
            return;
         }
      std::vector<ElabRef> is = { i };
      links.push_back(std::make_pair(var.code(), is));
   }

   void
   VarIndexedList::push_back(Elaboration e) {
      elabs.push_front(e);
      ElabRef i = elabs.begin();
      for (auto x: get_vars(e))
         push_link(x, i);
   }

   void
   VarIndexedList::erase_if_has(Elaboration e) {
      // Find the list of things.
      if (auto refs = get_refs(e)) {
         for (auto elab_ref: *refs)
            elabs.erase(elab_ref);
         // Remove it from all others.
         for (auto elab_ref: *refs)
            for (auto& link: links) {
               auto& elab_refs = link.second;
               for (std::size_t i = 0; i != elab_refs.size();) {
                  if (elab_refs[i] == elab_ref)
                     elab_refs.erase(elab_refs.begin() + i);
                  else
                     ++i;
               }
            }
      }
   }

   // -- AssumptionSet --
   void AssumptionSet::assume_local(Elaboration e) {
      if (not is_local(e))
         local_exprs.push_back(e);
   }

   bool
   AssumptionSet::is_local(Elaboration e) const {
      for (auto local: local_exprs)
         if (structural_equivalence(local, e.code()))
            return true;
      return false;
   }

   bool
   AssumptionSet::contains(Elaboration e) const {
      auto i = std::find(elabs.begin(), elabs.end(), e);
      return i != elabs.end();
   }

   void
   AssumptionSet::discharge(Elaboration e) {
      erase_if_has(e);
   }

   void
   AssumptionSet::assume(Elaboration pred) {
      push_back(pred);
   }
}
