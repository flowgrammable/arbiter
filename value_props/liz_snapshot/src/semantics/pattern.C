// Copyright (C) 2010-2012, Texas A&M University.
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

#include "Elaborator.H"
namespace liz {
   // -------------------------------------------
   // -- Template Argument deduction machinery --
   // -------------------------------------------

   // -- Return true if the expression `expr' uses templates
   // -- that are in deducible context.
   static bool has_deducible_formals(Elaboration);

   namespace {
      struct DeducibleFormalVisitor : Expression::Visitor {
         bool result;
         DeducibleFormalVisitor() : result(false) { }

         void visit(const Expression&) { }

         void visit(const ReferenceType& x) {
            result = has_deducible_formals(x.referee());
         }

         void visit(const ArrowType& x) {
            if (has_deducible_formals(x.target()))
               result = true;
            for (std::size_t i = 0; i < x.arity(); ++i)
               if (has_deducible_formals(x.argument(i)))
                  result = true;
         }

         void visit(const ReadonlyType& x) {
            result = has_deducible_formals(x.type());
         }

         void visit(const TypeExpression& x) {
            result = has_deducible_formals(x.expr());
         }

         void visit(const Formal&) { result = true; }
      };
   }

   static bool
   has_deducible_formals(Elaboration expr) {
      DeducibleFormalVisitor v;
      if (expr.code() != nullptr)
         expr.code()->accept(v);
      return v.result;
   }



   // ---------------------------------------------
   // -- Pattern matching of general expressions --
   // ---------------------------------------------
   template<typename T>
   static const T*
   same_sort(Elaboration e, const T&, Substitution& subst) {
      struct V : Expression::Visitor {
         Substitution& subst;
         const T* result;
         V(Substitution& c) : subst(c), result() { }
         void visit(const Expression&) { subst.set_failed(); }
         void visit(const T& x) { result = &x; }
      };

      V  v { subst };
      e.code()->accept(v);
      return v.result;
   }

   static bool
   match(const Sequence<Formal>& exprs, const Sequence<Formal>& pats,
         Substitution& subst) {
      const auto n = exprs.size();
      if (n == pats.size()) {
         bool ok = true;
         for (std::size_t i = 0; i < n and ok; ++i)
            ok = match({ exprs[i]->type(), exprs[i] }, pats[i], subst);
      }
      else
         subst.set_failed();
      return not subst.failed();
   }   

   template<typename T>
   static bool
   match(const vector<T>& exprs, const vector<T>& pats,
         Substitution& subst) {
      const auto n = exprs.size();
      if (n == pats.size()) {
         bool ok = true;
         for (std::size_t i = 0; i < n and ok; ++i)
            ok = match(exprs[i], pats[i], subst);
      }
      else
         subst.set_failed();
      return not subst.failed();
   }

   static bool
   match(const vector<Elaboration>& exprs, const vector<Elaboration>& pats,
         Substitution& subst) {
      const auto n = exprs.size();
      if (n == pats.size()) {
         bool ok = true;
         for (std::size_t i = 0; i < n and ok; ++i)
            ok = match(exprs[i], pats[i].code(), subst);
      }
      else
         subst.set_failed();
      return not subst.failed();
   }

   static bool
   same_ctors(const VariantType& expr, const VariantType& ptrn,
              Substitution& subst)
   {
      auto& ectors = expr.constructors();
      auto& pctors = ptrn.constructors();
      const std::size_t n = ectors.size();
      if (pctors.size() != n)
         return subst.set_failed();
      for (std::size_t i = 0; i != n; ++i)
         if (not match({ ectors[i]->type(), ectors[i] }, pctors[i], subst))
            return subst.set_failed();
      return true;

   }

   static bool
   match(const Type* t, const Sequence<Value>& exprs,
         const Sequence<Value>& ptrns, Substitution& subst)
   {
      const auto n = exprs.size();
      InputTypes ts = source(t);
      if (n == ptrns.size()) {
         bool ok = true;
         for (std::size_t i = 0; i < n and ok; ++i)
            ok = match({ ts[i].code(), exprs[i] }, ptrns[i], subst);
      }
      else
         subst.set_failed();
      return not subst.failed();
   }

   static bool
   match(const Sequence<Value>& exprs,
         const Sequence<Value>& ptrns, Substitution& subst)
   {
      const auto n = exprs.size();
      if (n == ptrns.size()) {
         bool ok = true;
         for (std::size_t i = 0; i < n and ok; ++i)
            ok = match({ nullptr, exprs[i] }, ptrns[i], subst);
      }
      else
         subst.set_failed();
      return not subst.failed();
   }

   static bool
   match(const TagType* t, const TagType* x, Substitution& subst) {
      if (t->tag() != x->tag())
         return subst.set_failed();
      return match(t->type(), x->type(), subst);
   }

   static bool
   same_name(const Name* n0, const Name* n1) {
      if (n0 == nullptr and n1 == nullptr)
         return true;
      if (n0 != nullptr and n1 != nullptr)
         return n0->symbol() == n1->symbol();
      return false;
   }

   static bool
   same_formals(const Formals& fs0, const Formals& fs1, Substitution& subst) {
      const std::size_t n = fs0.size();
      if (n != fs1.size())
         return subst.set_failed();
      for (std::size_t i = 0; i != n; ++i) {
         if (not same_name(fs0[i]->name(), fs1[i]->name()))
            return subst.set_failed();
         if (not match(fs0[i]->type(), fs1[i]->type().code(), subst))
            return false;
      }
      return true;
   }

   // match(lam->body(), x.body().code(), subst);
   static bool
   match(const AssocArguments& exprs, const AssocArguments& ptrns,
         Substitution& subst)
   {
      const std::size_t n = exprs.size();
      if (n != ptrns.size())
         return subst.set_failed();
      for (std::size_t i = 0; i != n; ++i) {
         if (exprs[i].first != ptrns[i].first)
            return subst.set_failed();
         if (not match(exprs[i].second, ptrns[i].second.code(), subst))
            return false;
      }
      return true;
   }

   static const Formal*
   is_dependent(const Substitution& subst, Elaboration e) {
      if (auto te = is<TypeExpression>(e.code()))
         if (auto sym =  is<SymbolicValue>(te))
            if (subst.is_dep(&sym->formal()))
               return &sym->formal();
      if (auto sym =  is<SymbolicValue>(e.code()))
         if (subst.is_dep(&sym->formal()))
            return &sym->formal();
      return nullptr;
   }

   static bool
   match_dependent(Substitution& subst, const Formal* f, const Expression* e) {
      if (auto old_e = subst.dep_subst[f]) {
         if (old_e.code() != e) {
            subst.set_failed();
            return false;
         }
      } else
         subst.dep_subst[f] = { f->type(), e };
      return true;
   }

   bool
   match(Elaboration expr, const Expression* pat, Substitution& s) {
      if (auto f = is_dependent(s, expr))
         return match_dependent(s, f, pat);
      struct V : Expression::Visitor {
         Substitution& subst;
         Elaboration expr;
         V(Substitution& c, Elaboration e) : subst(c), expr(e) { }

         void visit(const Expression& x) {
            internal_error("match: " + quote(typeid(x).name()));
         }

         void visit(const Formal& x) {
            if (&x != expr.code())
               subst.set_failed();
         }

         void visit(const Instance& x) {
            if (auto inst = same_sort(expr, x, subst)) {
               if (x.constructor()->name() == inst->constructor()->name()) {
                  match(x.constructor()->type(), inst->arguments(),
                        x.arguments(), subst);
               } else
                  subst.set_failed();
            }
         }

         void visit(const Constructor& x) {
            if (auto ctor = same_sort(expr, x, subst))
               if (x.name() != ctor->name())
                  subst.set_failed();

         }

         void visit(const CallExpression& x) {
            if (auto call = same_sort(expr, x, subst))
               match(x.function(), call->function().code(), subst) and
                  match(call->arguments(), x.arguments(), subst);
         }

         void visit(const Key& x) {
            if (auto k = same_sort(expr, x, subst)) {
               if (not key_equal(x, *k))
                  subst.set_failed();
            }
         }

         void visit(const TagType& x) {
            if (auto tg = same_sort(expr, x, subst))
               match(tg->type(), x.type(), subst);
         }

         void visit(const ReferenceType& x) {
            if (auto rt = same_sort(expr, x, subst))
               match(rt->referee(), x.referee(), subst);
         }

         void visit(const ArrayType& x) {
            if (auto arr_t = same_sort(expr, x, subst))
               match(arr_t->elem_type(), x.elem_type(), subst);
         }

         void visit(const FixedArrayType& x) {
            if (auto arr_t = same_sort(expr, x, subst))
               match(arr_t->elem_type(), x.elem_type().code(), subst)
                  and match(arr_t->length(), x.length().code(), subst);
         }

         void visit(const UintType& x) {
            if (auto y = same_sort(expr, x, subst))
               match(y->expr(), &x.width(), subst);
         }

         void visit(const ArrowType& x) {
            if (auto at = same_sort(expr, x, subst))
               match(at->source(), x.source(), subst)
                  and match(at->target(), x.target(), subst);
         }

         void visit(const RecordType& x) {
            if (auto rt = same_sort(expr, x, subst))
               match(rt->components(), x.components(), subst);
         }

         void visit(const ReadonlyType& x) {
            if (auto rt = same_sort(expr, x, subst))
               match(rt->type(), x.type(), subst);
         }

         void visit(const RestrictedType& x) {
            if (auto rt = same_sort(expr, x, subst))
               match(rt->type(), x.type(), subst)
                  and match(rt->condition(), x.condition().code(), subst);
         }

         void visit(const TypeExpression& x) {
            Substitution old_subst(subst);
            if (auto te = same_sort(expr, x, subst))
               match(te->expr(), x.expr().code(), subst);
            // FIXME: Don't like this special case. Need symbolic values.
            if (subst.failed())
               if (auto sym = is<SymbolicValue>(x.expr().code())) {
                  subst = old_subst;
                  sym->accept(*this);
               }
         }

         void visit(const ProductType& x) {
            if (auto pt = same_sort(expr, x, subst))
               match(pt->source(), x.source(), subst)
                  and match(pt->target(), x.target(), subst);
         }

         void visit(const Plus& x) {
            if (auto pl = same_sort(expr, x, subst))
               match(pl->lhs(), x.lhs().code(), subst)
                  and match(pl->rhs(), x.rhs().code(), subst);
         }

         void visit(const GenerativeType& x) {
            if (auto gt = same_sort(expr, x, subst)) {
               if (x.name()->symbol() == gt->name()->symbol())
                  match(gt->value(), x.value(), subst);
               else
                 subst.set_failed();
            }
         }

         void visit(const VariantType& x) {
            if (auto vt = same_sort(expr, x, subst))
               same_ctors(x, *vt, subst);
         }

         void visit(const BasicType& x) {
            if (auto pt = same_sort(expr, x, subst))
               if (x.name() != pt->name())
                  subst.set_failed();
         }

         void visit(const Read& x) {
            auto y = is<Read>(expr); 
            if (y == nullptr)
               x.address().code()->accept(*this);
            else
               match(y->address(), x.address().code(), subst);
         }

         void visit(const Int& x) {
            if (auto int_e = same_sort(expr, x, subst))
               if(x.rep() != int_e->rep())
                  subst.set_failed();
         }

         void visit(const Bool& x) {
            if (auto bool_e = same_sort(expr, x, subst))
               if(x.rep() != bool_e->rep())
                  subst.set_failed();
         }

         void visit(const Component& x) {
            if (auto cmpt = same_sort(expr, x, subst)) {
               if (x.symbol() == cmpt->symbol())
                  match(cmpt->whole(), x.whole().code(), subst);
               else
                  subst.set_failed();
            }
         }

         void visit(const Namespace& x) {
            if (auto ns = same_sort(expr, x, subst))
               if (ns != &x)
                  subst.set_failed();
         }

         void visit(const SymbolicValue& x) {
            // Attempt to unify.
            auto p = &x.formal();
            if (not subst.has(p))
               subst[p] = expr;
            else if (subst[p].code() != expr.code())
               subst.set_failed();
         }

         void visit(const Signature& x) {
            // FIXME: Pointer comparison?
            if (auto sig = same_sort(expr, x, subst)) {
               auto& xlnk = x.link_name();
               auto& siglnk = sig->link_name();
               if (xlnk.name()->symbol() != siglnk.name()->symbol())
                  match(siglnk.type(), xlnk.type(), subst);
            }
         }

         void visit(const LinkName& x) {
            if (auto lnk = same_sort(expr, x, subst))
               if (x.name()->symbol() != lnk->name()->symbol())
                  match(lnk->type(), x.type(), subst);
         }

         void visit(const DotSelection& x) {
            if (auto dot = same_sort(expr, x, subst)) {
               // FIXME: Check types.
               if (dot->symbol() != x.symbol())
                  subst.set_failed();
               match(dot->whole(), x.whole().code(), subst);
            }
         }

         void visit(const Lambda& x) {
            if (auto lam = same_sort(expr, x, subst))
               if (same_name(lam->name(), x.name()))
                  if (same_formals(lam->formals(), x.formals(), subst))
                     match(lam->body(), x.body().code(), subst);
         }

         void visit(const UnaryBuiltinFunction& x) {
            if (auto fun = same_sort(expr, x, subst))
               if (fun->code() != x.code())
                  subst.set_failed();
         }

         void visit(const Initializer& x) {
            if (auto init = same_sort(expr, x, subst))
               match(*init, x, subst);
         }

         void visit(const BinaryLogical& x) {
            if (auto conn = same_sort(expr, x, subst)) {
               if (conn->operation() != x.operation())
                  subst.set_failed();
               else {
                  match(conn->lhs(), x.lhs().code(), subst)
                     and match(conn->rhs(), x.rhs().code(), subst);
               }
            }
         }

         void visit(const Langle& x) {
            if (auto bin_op = same_sort(expr, x, subst))
               match(bin_op->lhs(), x.lhs().code(), subst)
                  and match(bin_op->rhs(), x.rhs().code(), subst);
         }

         void visit(const Rangle& x) {
            if (auto bin_op = same_sort(expr, x, subst))
               match(bin_op->lhs(), x.lhs().code(), subst)
                  and match(bin_op->rhs(), x.rhs().code(), subst);
         }

         void visit(const Langleq& x) {
            if (auto bin_op = same_sort(expr, x, subst))
               match(bin_op->lhs(), x.lhs().code(), subst)
                  and match(bin_op->rhs(), x.rhs().code(), subst);
         }

         void visit(const Rangleq& x) {
            if (auto bin_op = same_sort(expr, x, subst))
               match(bin_op->lhs(), x.lhs().code(), subst)
                  and match(bin_op->rhs(), x.rhs().code(), subst);
         }

         void visit(const Eqeq& x) {
            if (auto eqeq = same_sort(expr, x, subst))
               match(eqeq->lhs(), x.lhs().code(), subst)
                  and match(eqeq->rhs(), x.rhs().code(), subst);
         }

         void visit(const Excleq& x) {
            if (auto bin_op = same_sort(expr, x, subst))
               match(bin_op->lhs(), x.lhs().code(), subst)
                  and match(bin_op->rhs(), x.rhs().code(), subst);
         }

         void visit(const Constraint& x) {
            if (auto cst = same_sort(expr, x, subst)) {
               if (cst->constructor() != x.constructor())
                  subst.set_failed();
               match(cst->arguments(), x.arguments(), subst);
            }
         }
      };

      V v { s, expr };
      pat->accept(v);
      return bool(v.subst);
   }

   Substitution
   match_dependent(Elaboration e, const Expression* p, const Formals& fs) {
      Substitution subst(fs);
      match(e, p, subst);
      return subst;
   }
}
