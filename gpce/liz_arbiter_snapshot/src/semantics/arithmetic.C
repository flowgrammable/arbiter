// Copyright (C) 2009-2013, Texas A&M University.
// Copyright (C) 2015, Gabriel Dos Reis.
// All rights reserved.
// Written by Michael Lopez.
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

#include "lp_lib.h"
#include "Elaborator.H"
#include <limits>

// This file implements an arithmetic solver based on the paper "A Practical
// Decision Procedure for Arithmetic with Function Symbols" by Robert E.
// Shostak.

// The subset of arithmetic expressions is defined as
//   l   ::= l => l | l <=> l | l /\ l | l \/ l | e `rel` e
//   rel ::= == | != | < | <= | > | >=
//   e   ::= e + e | e - e | n * e | e * n | n | v
//   n   ::= integer constant
//   v   ::= variable (any other expression is treated as a variable)

//  TODOS
//    * Solver does not account for closed terms (add a call to simplify)

namespace liz {
namespace arith {
 
   struct ArithmeticContext {
   ArithmeticContext(Elaborator& ctx)
      : elab(&ctx),
        NN_to_B(ctx.make_arrow_type(ctx.get_bool(),
                { ctx.get_int(), ctx.get_int() })),
        B_to_B(ctx.make_arrow_type(ctx.get_bool(), { ctx.get_bool() })),
        not_elab(lookup_operator(&ctx, "not", B_to_B)),
        less_elab(lookup_operator(&ctx, "<", NN_to_B)),
        less_eq_elab(lookup_operator(&ctx, "<=", NN_to_B)),
        greater_elab(lookup_operator(&ctx, ">", NN_to_B)),
        greater_eq_elab(lookup_operator(&ctx, ">=", NN_to_B)),
        eq_elab(lookup_operator(&ctx, "==", NN_to_B)),
        not_eq_elab(lookup_operator(&ctx, "!=", NN_to_B))
      { }
      Elaborator* elaborator() const { return elab; }
      Elaborator* operator->() const { return elaborator(); }
      Elaboration make_conjunct(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool() ,
                  elab->build_logical(logical::conjunction, lhs, rhs) };
      }
      Elaboration make_disjunct(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool() ,
                  elab->build_logical(logical::disjunction, lhs, rhs) };
      }
      Elaboration make_not(Elaboration e) {
         return { elab->get_bool(),
                  elab->build_not(not_elab, e) };
      }
      Elaboration make_less(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool(),
                  elab->build_langle(less_elab, lhs, rhs) };
      }
      Elaboration make_less_eq(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool(),
                  elab->build_langleq(less_eq_elab, lhs, rhs) };
      }
      Elaboration make_greater(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool(),
                  elab->build_rangle(greater_elab, lhs, rhs) };
      }
      Elaboration make_greater_eq(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool(),
                  elab->build_rangleq(greater_eq_elab, lhs, rhs) };
      }
      Elaboration make_eq(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool(),
                  elab->build_eqeq(eq_elab, lhs, rhs) };
      }
      Elaboration make_not_eq(Elaboration lhs, Elaboration rhs) {
         return { elab->get_bool(),
                  elab->build_excleq(not_eq_elab, lhs, rhs) };
      }
   private:
      Elaborator* const elab;
      const ArrowType* NN_to_B;
      const ArrowType* B_to_B;
      FunctionElaboration not_elab;
      FunctionElaboration less_elab;
      FunctionElaboration less_eq_elab;
      FunctionElaboration greater_elab;
      FunctionElaboration greater_eq_elab;
      FunctionElaboration eq_elab;
      FunctionElaboration not_eq_elab;
   };

   // Returns the set of arithmetic assumptions
   std::vector<Elaboration>
   gather_presburger_axioms(ArithmeticContext& ctx) {
      std::vector<Elaboration> ariths;
      for (auto assumption: ctx->assumptions())
         if (assumption.type() == ctx->get_arithmetic())
            ariths.push_back({ ctx->get_bool(), assumption.code() });
      return ariths;
   }

   Elaboration
   conjunct_propositions(ArithmeticContext& ctx,
                         const std::vector<Elaboration>& props)
   {
      auto i = props.begin();
      if (i == props.end())
         return { };
      Elaboration conjunct = *i++;
      for (; i != props.end(); ++i)
         conjunct = ctx.make_conjunct(conjunct, *i);
      return conjunct;
   }

   // Builds the proof obligation into a proposition
   Elaboration
   build_proof_obligation(ArithmeticContext& ctx, Elaboration goal) {
      auto arith_assumptions = gather_presburger_axioms(ctx);
      if (arith_assumptions.empty()) {
         Elaboration conjunct
            { ctx->get_bool(), ctx->build_bool(true, ctx->get_bool()) };
         return ctx.make_disjunct(ctx.make_not(conjunct), goal);
      } else {
         auto conjunct = conjunct_propositions(ctx, arith_assumptions);
         return ctx.make_disjunct(ctx.make_not(conjunct), goal);
      }
   }

   // Remove implications and equivalences from the expression
   Elaboration
   remove_impl_and_bicond(ArithmeticContext& ctx, Elaboration e) {
      struct V : Expression::Visitor {
         ArithmeticContext& ctx;
         Elaboration result;
         V(ArithmeticContext& ctx, Elaboration e)
            : ctx(ctx), result(e) { }
         void visit(const Expression&) { }
         void visit(const BinaryLogical& x) {
            if (x.operation() == logical::implication) {
               auto lhs = remove_impl_and_bicond(ctx, x.lhs());
               auto rhs = remove_impl_and_bicond(ctx, x.rhs());
               result = ctx.make_disjunct(ctx.make_not(lhs), rhs);
            } else if (x.operation() == logical::equivalence) {
               Elaboration lhs_to_rhs {
                  ctx->get_bool(),
                  ctx->build_logical(logical::implication, x.lhs(), x.rhs()) };
               Elaboration rhs_to_lhs {
                  ctx->get_bool(),
                  ctx->build_logical(logical::implication, x.rhs(), x.lhs()) };
               auto lhs = remove_impl_and_bicond(ctx, lhs_to_rhs);
               auto rhs = remove_impl_and_bicond(ctx, rhs_to_lhs);
               result = ctx.make_conjunct(lhs, rhs);
            } else {
               auto lhs = remove_impl_and_bicond(ctx, x.lhs());
               auto rhs = remove_impl_and_bicond(ctx, x.rhs());
               result.code(ctx->build_logical(x.operation(), lhs, rhs));
            }
         }
         void visit(const Not& x) {
            result = ctx.make_not(remove_impl_and_bicond(ctx, x.argument()));
         } 
      };
      V v(ctx, e);
      e.code()->accept(v);
      return v.result;
   }

   Elaboration remove_negations(ArithmeticContext&, Elaboration);

   Elaboration
   push_not_dowm(ArithmeticContext& ctx, Elaboration e) {
      struct V : Expression::Visitor {
         ArithmeticContext& ctx;
         Elaboration result;
         V(ArithmeticContext& ctx, Elaboration e)
            : ctx(ctx), result(e) { }
         void visit(const Expression&) { }
         void visit(const Not& x) {
            result = remove_negations(ctx, x.argument());
         }
         void visit(const BinaryLogical& x) {
            if (x.operation() == logical::implication or
                x.operation() == logical::equivalence)
               abort();
            auto op = x.operation() == logical::conjunction
                    ? logical::disjunction : logical::conjunction;
            auto lhs = push_not_dowm(ctx, x.lhs());
            auto rhs = push_not_dowm(ctx, x.rhs());
            result.code(ctx->build_logical(op, lhs, rhs));
         }
         void visit(const Langle& x) {
            result = ctx.make_greater_eq(x.lhs(), x.rhs());
         }
         void visit(const Langleq& x) {
            result = ctx.make_greater(x.lhs(), x.rhs());
         }
         void visit(const Rangle& x) {
            result = ctx.make_less_eq(x.lhs(), x.rhs());
         }
         void visit(const Rangleq& x) {
            result = ctx.make_less(x.lhs(), x.rhs());
         }
         void visit(const Eqeq& x) {
            result = ctx.make_not_eq(x.lhs(), x.rhs());
         }
         void visit(const Excleq& x) {
            result = ctx.make_eq(x.lhs(), x.rhs());
         }
      };
      V v(ctx, e);
      e.code()->accept(v);
      return v.result;
   }

   Elaboration
   remove_negations(ArithmeticContext& ctx, Elaboration e) {
      struct V : Expression::Visitor {
         ArithmeticContext& ctx;
         Elaboration result;
         V(ArithmeticContext& ctx, Elaboration e)
            : ctx(ctx), result(e) { }
         void visit(const Expression&) { }
         void visit(const Not& x) { result = push_not_dowm(ctx, x.argument()); }
         void visit(const BinaryLogical& x) {
            if (x.operation() == logical::implication or
                x.operation() == logical::equivalence)
               abort();
            auto op = x.operation();
            auto lhs = remove_negations(ctx, x.lhs());
            auto rhs = remove_negations(ctx, x.rhs());
            result.code(ctx->build_logical(op, lhs, rhs));
         }
      };
      V v(ctx, e);
      e.code()->accept(v);
      return v.result;
   }

   const BinaryLogical*
   is_binary_and(const Expression* e) {
      if (auto bin = is<BinaryLogical>(e))
         if (bin->operation() == logical::conjunction)
            return bin;
      return nullptr;
   }

   const BinaryLogical*
   is_binary_or(const Expression* e) {
      if (auto bin = is<BinaryLogical>(e))
         if (bin->operation() == logical::disjunction)
            return bin;
      return nullptr;
   }

   Elaboration push_conjuncts_down(ArithmeticContext&, Elaboration);

   // Assume p,q,r,s were of the form (p\/q)/\(r\/s). Transforms  this to
   // ((p/\r)\/(p/\s))\/((q/\r)\/(q/\s)).
   Elaboration
   double_distribute_conjunction(ArithmeticContext& ctx, Elaboration p,
                                 Elaboration q, Elaboration r, Elaboration s) {
      auto pr = push_conjuncts_down(ctx, ctx.make_conjunct(p,r));
      auto ps = push_conjuncts_down(ctx, ctx.make_conjunct(p,s));
      auto qr = push_conjuncts_down(ctx, ctx.make_conjunct(q,r));
      auto qs = push_conjuncts_down(ctx, ctx.make_conjunct(q,s));
      return ctx.make_disjunct(
                ctx.make_disjunct(pr,ps),
                ctx.make_disjunct(qr,qs));
   }

   // Distributes conjunction through disjunctions
   Elaboration
   push_conjuncts_down(ArithmeticContext& ctx, Elaboration e) {
      if (auto conj = is_binary_and(e.code())) {
         if (auto lhs = is_binary_or(conj->lhs().code())) {
            if (auto rhs = is_binary_or(conj->rhs().code())) {
               // (p\/q)/\(r\/s) => ((p/\r)\/(p/\s))\/((q/\r)\/(q/\s))
               return double_distribute_conjunction(ctx,
                  lhs->lhs(), lhs->rhs(), rhs->lhs(), rhs->rhs());
            } else {
               // (p\/q) /\ r => (p/\r) \/ (q/\r)
               auto pr = push_conjuncts_down(ctx,
                  ctx.make_conjunct(lhs->lhs(), conj->rhs()));
               auto qr = push_conjuncts_down(ctx,
                  ctx.make_conjunct(lhs->rhs(), conj->rhs()));
               return ctx.make_disjunct(pr, qr);
            }
         } else if (auto rhs = is_binary_or(conj->rhs().code())) {
            // p /\ (q\/r) => (p/\q) \/ (p/\r)
            auto pq = push_conjuncts_down(ctx,
                         ctx.make_conjunct(conj->lhs(), rhs->lhs()));
            auto pr = push_conjuncts_down(ctx,
                         ctx.make_conjunct(conj->lhs(), rhs->rhs()));
            return ctx.make_disjunct(pq, pr);
         }
      }
      return e;
   }

   // Transforms a logical operation into disjunctive normal form
   Elaboration
   to_disjunctive_normal_form(ArithmeticContext& ctx, Elaboration e) {
      e = remove_impl_and_bicond(ctx, e);
      e = remove_negations(ctx, e);
      return push_conjuncts_down(ctx, e);
   }

   // Builds the proof obligation into a proposition in dijunctive normal form
   Elaboration
   build_normal_presburger_proposition(Elaborator& ctx, Elaboration e) {
      ArithmeticContext arith_ctx { ctx };
      // Turns assumptions and the goal e into a proposition.
      auto prop = build_proof_obligation(arith_ctx, e);
      // This is a verification problem. Hunt for counter examples.
      prop = arith_ctx.make_not(prop);
      prop = to_disjunctive_normal_form(arith_ctx, prop);
      // Simplify closed airthmetic expressions and short circuit logicals
      return evaluate(arith_ctx.elaborator(), prop);
   }

   // Structure that tracks 
   // FIXME: If the equivalence operation gets hairier, abstract it out.
   struct VariableClasses {
      VariableClasses() : index(), next_index(0) { }
      std::size_t get_class(const Expression* e) const {
         auto i = index.find(e);
         if (i == index.end())
            return null_class;
         return i->second;
      }
      void insert_variable(const Expression* e) {
         if (auto r = is<Read>(e))
            if (auto f = is<Formal>(r->address().code())) {
               insert_read_formal(r, *f);
               return;
            }
         auto eq_class = get_class(e);
         index[e] = eq_class == null_class ? next_index++ : eq_class;
      }
      static constexpr std::size_t null_class =
         std::numeric_limits<std::size_t>::max();

      // Helpers
      void insert_read_formal(const Read* rf, const Formal& f) {
         for(auto i: index)
            if (auto r = is<Read>(i.first))
               if (auto g = is<Formal>(r->address().code()))
                  if (f.type() == g->type() and f.name() == g->name()) {
                     index[rf] = index[i.first];
                     return;
                  }
         index[rf] = next_index++;
      }
      std::size_t class_count() const { return next_index; }
      // Members
      std::map<const Expression*, std::size_t> index;
      std::size_t next_index;
   };

   static void
   do_get_vars(const Expression* e, VariableClasses& vars) {
      struct V : Expression::Visitor {
         VariableClasses& vars;
         V(VariableClasses& vars) : vars(vars) { }
         // All else are variables
         void visit(const Expression& x) { vars.insert_variable(&x); }
         // Integers cannot be variables
         void visit(const Int&) { }
         void visit(const BinaryLogical& x) {
            if (x.operation() == logical::conjunction) {
               do_get_vars(x.lhs().code(), vars);
               do_get_vars(x.rhs().code(), vars);
            } else
               vars.insert_variable(&x);
         }
         // Helper for the rest of the binary functions
         inline void get_vars(const BinaryExpression& bin) {
            do_get_vars(bin.lhs().code(), vars);
            do_get_vars(bin.rhs().code(), vars);
         }
         void visit(const Langle& x)  { get_vars(x); }
         void visit(const Langleq& x) { get_vars(x); }
         void visit(const Rangle& x)  { get_vars(x); }
         void visit(const Rangleq& x) { get_vars(x); }
         void visit(const Eqeq& x)    { get_vars(x); }
         void visit(const Excleq& x)  { get_vars(x); }
         void visit(const Plus& x)    { get_vars(x); }
         void visit(const Dash& x)    { get_vars(x); }
      };
      V v(vars);
      e->accept(v);
   }

   static VariableClasses
   get_vars(const Expression* e) {
      VariableClasses vars;
      do_get_vars(e,vars);
      return vars;
   }

   enum class integer_constraint_relation {
      equal, not_equal,
      less_than, less_than_equal,
      greater_than, greater_than_equal
   };

   // Represents an integer constraint in an ILP
   // Interpret this as: coefficients * X relation bound
   struct integer_constraint {
      intmax_t bound;
      integer_constraint_relation relation;
      std::vector<intmax_t> coefficients;
   };

   static const BinaryLogical*
   is_logical_and(const Expression* e) {
      if (auto l = is<BinaryLogical>(e))
         if (l->operation() == logical::conjunction)
            return l;
      return nullptr;
   }

   // FIXME: Most of these errors can be statically avoided. Consider building
   //        an arithmetic structure.
   static void
   decompose_conjunct(const Expression* e, Sequence<BinaryExpression>& vars) {
      if (auto and_expr = is_logical_and(e)) {
         decompose_conjunct(and_expr->lhs().code(), vars);
         decompose_conjunct(and_expr->rhs().code(), vars);
      } else if (auto bin = is<BinaryExpression>(e)) {
         vars.push_back(bin);
      } else
         internal_error("expression does not represent an integer constraint");
   }

   static std::vector<const BinaryExpression*>
   decompose_conjunct(const Expression* e) {
      Sequence<BinaryExpression> vars;
      decompose_conjunct(e,vars);
      return vars;
   }

   static integer_constraint_relation
   get_constraint_relation(const BinaryExpression* e) {
      struct V : Expression::Visitor {
         integer_constraint_relation result;
         V() : result(integer_constraint_relation::equal) { }
         void visit(const Expression&) {
            internal_error("binary expression is not an equality or "
                           "inequality");
         }
         void visit(const Langle&) {
            result = integer_constraint_relation::less_than;
         }
         void visit(const Langleq&) {
            result = integer_constraint_relation::less_than_equal;
         }
         void visit(const Rangle&) {
            result = integer_constraint_relation::greater_than;
         }
         void visit(const Rangleq&) {
            result = integer_constraint_relation::greater_than_equal;
         }
         void visit(const Eqeq&) {
            result = integer_constraint_relation::equal;
         }
         void visit(const Excleq&) {
            result = integer_constraint_relation::not_equal;
         }
      };
      V v;
      e->accept(v);
      return v.result;
   }

   // Considers the left hand side of a constraint
   static void
   consider_lhs(const Expression* e, integer_constraint& c,
                const VariableClasses& vars)
   {
      // Just account for the constraints we see now.
      if (auto x = is<Int>(e))
         c.bound -= x->rep();
      else if (auto x = is<Read>(e))
         ++c.coefficients[vars.get_class(x)];
      else
         internal_error("integer solver got an expression it couldn't handle");
   }

   // Considers the left hand side of a constraint
   static void
   consider_rhs(const Expression* e, integer_constraint& c,
                const VariableClasses& vars)
   {
      // Just account for the constraints we see now.
      if (auto x = is<Int>(e))
         c.bound += x->rep();
      else if (auto x = is<Read>(e))
         --c.coefficients[vars.get_class(x)];
      else
         internal_error("integer solver got an expression it couldn't handle");
   }

   static integer_constraint
   to_integer_constraint(const BinaryExpression* x, const VariableClasses& vars)
   {
      integer_constraint c { 0, get_constraint_relation(x),
                             std::vector<intmax_t>(vars.class_count(), 0) };
      consider_lhs(x->lhs().code(), c, vars);
      consider_rhs(x->rhs().code(), c, vars);
      return c;
   }

   static std::vector<integer_constraint>
   to_constraints(const Expression* e, const VariableClasses& vars) {
      auto exprs = decompose_conjunct(e);
      std::vector<integer_constraint> constraints;
      for (auto e: exprs)
         constraints.push_back(to_integer_constraint(e, vars));
      constraints.shrink_to_fit();
      return constraints;
   }

   struct ILP {
      // The expression given is a conjunction of arithmetic stuffs.
      ILP(const Expression* e, const VariableClasses& vars)
            : constraints(to_constraints(e, vars)) { }
      std::vector<integer_constraint> constraints;
   };

   void do_to_ilps(const Expression* e, const VariableClasses& vars,
                   std::vector<ILP>& ilps) {
      if (auto bin_or = is_binary_or(e)) {
         do_to_ilps(bin_or->lhs().code(), vars, ilps);
         do_to_ilps(bin_or->rhs().code(), vars, ilps);
      } else {
         ilps.push_back(ILP(e,vars));
      }

   }

   // Transforms a simplified disjunctive into a sequence of ILPs.
   std::vector<ILP>
   to_ilps(const Expression* e, const VariableClasses& vars) {
      std::vector<ILP> ilps;
      do_to_ilps(e, vars, ilps);
      return ilps;
   }

   enum class SolverStatus { initialized, found_solution, no_solution };

   struct ArithmeticSolver {
      ArithmeticSolver(const Expression* e)
           // Does this need to be here?
         : vars(get_vars(e)),
           // FIXME: This should decompose a disjunctive into many ilps
           ilps(to_ilps(e, vars)),
           status(SolverStatus::initialized) { }

      SolverStatus solve(const ILP& ilp) {
         //print_ILP(ilp);
         const int num_vars = vars.class_count();
         lprec* lp;
         // Make number of variables.
         lp = make_lp(0, vars.class_count());
         set_verbose(lp, NEUTRAL); // Don't write that many error messages.
         std::vector<REAL> row_buffer(1 + num_vars, 0);
         // Make an lp row by row.
         set_add_rowmode(lp, TRUE);
         for (auto c: ilp.constraints) {
            auto c_ala_lp = adjust_constraint_for_lpsolve(c);
            set_row_buffer(row_buffer, c_ala_lp);
            add_constraint(
               lp,
               row_buffer.data(),
               to_lp_rel(c_ala_lp.relation),
               c_ala_lp.bound);
         }
         // Done building the model.
         set_add_rowmode(lp, FALSE);
         // Set the objective function
         std::fill(row_buffer.begin(), row_buffer.end(), 0);
         row_buffer[1] = 1;
         set_obj_fn(lp, row_buffer.data());
         set_maxim(lp);  // Make it a maximization problem
         // Write the ilp
         //write_LP(lp, stderr);
         // Set all variables to be integer
         for (int i = 0; i != num_vars; ++i) {
            if(set_int(lp, i+1, true) == 0)
               abort();
         }
         auto res = ::solve(lp);
         if (res == INFEASIBLE)
            return SolverStatus::no_solution;
         else
            return SolverStatus::found_solution;
         // Tidy the place
         delete_lp(lp);
      }

      void solve() {
         status = SolverStatus::no_solution;
         for (auto ilp: ilps)
            if (solve(ilp) == SolverStatus::found_solution) {
               status = SolverStatus::found_solution;
               return;
            }
      }

      // Helper functions for lpsolve
      integer_constraint adjust_constraint_for_lpsolve(integer_constraint c) const
      {
         // Adjust relation to allow only ==, >=, or <=
         if (c.relation == integer_constraint_relation::less_than) {
            c.relation = integer_constraint_relation::less_than_equal;
            --c.bound;
         } else if (c.relation == integer_constraint_relation::greater_than) {
            c.relation = integer_constraint_relation::greater_than_equal;
            ++c.bound;
         } else if (c.relation == integer_constraint_relation::not_equal) {
            internal_error("error in integer solver");
         }
         return c;
      }
      void set_row_buffer(std::vector<REAL>& row_buffer,
                          const integer_constraint& c)
      {
         for(std::size_t i = 0; i != vars.class_count(); ++i)
            row_buffer[i+1] = c.coefficients[i];
      }
      inline int to_lp_rel(integer_constraint_relation r) const {
         switch (r) {
            case integer_constraint_relation::equal:
              return EQ;
            case integer_constraint_relation::less_than_equal:
              return LE;
            case integer_constraint_relation::greater_than_equal:
              return GE;
            case integer_constraint_relation::not_equal:
            case integer_constraint_relation::less_than:
            case integer_constraint_relation::greater_than:
               internal_error("error in integer solver");
         }
              return EQ;
      }

      VariableClasses vars;
      std::vector<ILP> ilps;
      SolverStatus status;
   };

   // Returns true iff the elaboration is an arithmetic constraint.
   bool
   is_presburger(Elaboration e, const Type* int_t) {
      struct V : Expression::Visitor {
         const Type* int_t;
         bool result;
         V(const Type* int_t) : int_t(int_t), result(false) { }
         inline bool integer_args(const BinaryExpression& e) {
            return e.lhs().type() == int_t and e.rhs().type() == int_t;
         }
         inline bool presburger_args(const BinaryLogical& e) {
            return is_presburger(e.lhs(), int_t)
               and is_presburger(e.rhs(), int_t);
         }
         void visit(const Expression&)      { result = false; }
         void visit(const Langle& x)        { result = integer_args(x); }
         void visit(const Langleq& x)       { result = integer_args(x); }
         void visit(const Rangle& x)        { result = integer_args(x); }
         void visit(const Rangleq& x)       { result = integer_args(x); }
         void visit(const Eqeq& x)          { result = integer_args(x); }
         void visit(const Excleq& x)        { result = integer_args(x); }
         void visit(const BinaryLogical& x) { result = presburger_args(x); }
         void visit(const Not& x) {
            result = is_presburger(x.argument(), int_t);
         }
      };
      V v(int_t);
      e.code()->accept(v);
      return v.result;
   }

} // namespace arith

   // True iff the arithmetic elaboration can be derived from the assumptions
   bool
   can_prove_arithmetic(Elaborator& ctx, Elaboration e) {
      using namespace arith;
      e = { ctx.get_bool(), e.code() };
      auto prop = build_normal_presburger_proposition(ctx, e);
      if (auto b = is<Bool>(prop.code()))
         return not b->rep();
      ArithmeticSolver solver(prop.code());
      solver.solve();
      return solver.status == SolverStatus::no_solution;
   }

   bool
   is_arithmetic_constraint(Elaboration e, const Type* int_t) {
      return arith::is_presburger(e, int_t);
   }

} // namespace liz