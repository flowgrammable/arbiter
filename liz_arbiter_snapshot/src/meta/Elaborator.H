// Copyright (C) 2009-2013, Texas A&M University.
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


#ifndef LIZ_ELABORATOR_INCLUDED
#define LIZ_ELABORATOR_INCLUDED


#include <list>
#include <vector>
#include <map>
#include <stack>
#include <liz/Ast>
#include "Parser.H"
#include "Translator.H"

namespace liz {
   // --------------
   // -- Evidence --
   // --------------
   // Instantiation evidence; like a substituion, but maps only
   // signatures to functions.
   struct Evidence : std::map<const Signature*, const Expression*> {
      Evidence();
      const Expression* operator()(const Signature*) const;
      const Expression* operator()(const LinkName*) const;
      bool good() const { return ok; }
      void set_bad() { ok = false; }
      // convenience conversion function to be used in
      // conditional context to test for well-formedness.
      operator const void*() const { return good() ? this : 0; }

      void send_to(const Signature*, const Expression*);
      void send_to(const Expression*, const Evidence*);

   private:
      using ConstraintMap = std::map<const Expression*, const Evidence*>;
      bool ok;                  // true if this is a good evidence.
      std::map<const Expression*, const Evidence*> sub_evidence_map;
   };


   // ------------------------------
   // -- SpecializationRepository --
   // ------------------------------
   // Repository for specializations of a given template.
   struct SpecializationRepository : std::map<Substitution,
                                              const Expression*> {
   };

   // ----------------
   // -- Elaborator --
   // ----------------
   struct Elaborator: Translator, ExpressionFactory, ScopeStack {
      struct DeclContext;

      // -- Source-level location manager
      struct LocationManager {
         LocationManager(Elaborator*, const Token&);
         ~LocationManager();
      private:
         Elaborator* ctx;
      };

      Elaborator(Reader&);
      ~Elaborator();

      std::ostream& output() { return reader()->output_stream(); }
      std::ostream& error() { return reader()->error_stream(); }
      std::ostream& debug() { return reader()->debug_stream(); }

      // Override Translator::elaborate.
      Elaboration elaborate(const Ast* x, const Type* t = 0);

      // Generate code that coeces a value to a target type, otherwise
      // raise an exception.
      Elaboration coerce(Elaboration, const Type*);

      // -- We have an expression that semantically designates a type.
      // -- Return the typeful version representation reflecting that
      // -- knowledge.
      TypeElaboration coerce_to_type(Elaboration);

      // Current source-level location
      const Token& current_location() const;

      // Manage the type-stack of value-producing blocks.
      void push_return_type(const Type*);
      const Type* get_return_type() const;
      void pop_return_type();
      int frame_depth() const;

      // Management of declaration contexts.
      DeclContext* top_decl_context() const;
      DeclContext* previous_decl_context() const;
      void remember_specialization(Elaboration);

      // Manage parameter declaration level.
      void increase_parameter_depth() { ++parms_level; }
      void decrease_parameter_depth() { --parms_level; }
      int get_parameter_depth() const { return parms_level; }


      Namespace* global_scope() const { return global_ns; }
      Namespace* liz_scope() const { return liz_ns; }
      const BasicType* get_concept() const { return concept_type; }
      const BasicType* get_prop() const;
      const BasicType* get_axiom() const { return axiom_type; }
      const BasicType* get_arithmetic() const { return arithmetic_type; }
      const Constructor* get_Function() const { return Function_ctor; }
      const Constructor* get_Record() const { return Record_ctor; }
      const Constructor* get_SubKey() const { return SubKey_ctor; }
      const Constructor* get_ValidKey() const { return ValidKey_ctor; }
      const Constructor* get_ProjectionOf() const { return ProjectionOf_ctor; }
      const Lambda* get_InstanceOf() const { return InstanceOf_ctor; }
      TypeElaboration get_typename() const;
      TypeElaboration get_LoadUnit() const;
      TypeElaboration get_namespace() const;
      TypeElaboration get_address() const;
      TypeElaboration get_void() const;
      TypeElaboration get_bool() const;
      TypeElaboration get_byte() const;
      TypeElaboration get_char() const;
      TypeElaboration get_int() const;
      TypeElaboration get_key() const;
      TypeElaboration get_double() const;
      TypeElaboration get_string() const;
      TypeElaboration get_tiny_client() const;
      TypeElaboration get_flow_mod() const;

      FunctionElaboration get_arity() const { return arity_fun; }
      FunctionElaboration get_input_type() const { return input_type_fun; }
      FunctionElaboration get_codomain() const { return codomain_fun; }

      // Return the function type of the (builtin) equality
      // operator on types.
      FunctionElaboration get_type_eq() const { return type_eq_fun; }

      const Evidence* register_evidence(const Expression*, const Evidence&);
      const Evidence* find_evidence(const Expression*) const;

      // template specialization management management.
      const Expression*
      retrieve_specialization(const Expression*, const Substitution&) const;
      void register_specialization(const Expression*, const Expression*,
                                   const Substitution&);

      void sorry(const std::string&);

      Reader* reader() { return rdr; }

      // Return a pointer to a module with a given name, if already loaded.
      LoadUnit* get_unit_if_loaded(const Name*) const;
      
   protected:
      using ModeStack = std::stack<const Type*>;
      using EvidenceRepository = std::map<const Expression*, Evidence>;
      using AllSpecs = std::map<const Expression*, SpecializationRepository>;
      Reader* rdr;
      std::stack<Token> locs;
      std::map<const Name*, LoadUnit*> loaded_modules;
      const BasicType* const concept_type;
      const BasicType* const typename_type;
      const BasicType* const LoadUnit_type;
      const BasicType* const prop_type;
      const BasicType* const axiom_type;
      const BasicType* const arithmetic_type;
      const BasicType* const key_type;
      const Constructor* const Function_ctor;
      const Constructor* const Record_ctor;
      const Constructor* const Regular_ctor;
      const Constructor* const SubKey_ctor;
      const Constructor* const ValidKey_ctor;
      const Constructor* const ProjectionOf_ctor;
      const Lambda* const InstanceOf_ctor;
      const BasicType* const namespace_type;
      const BasicType* const void_type;
      const BasicType* const address_type;
      const BasicType* const bool_type;
      const BasicType* const byte_type;
      const BasicType* const char_type;
      const BasicType* const int_type;
      const BasicType* const double_type;
      const BasicType* const string_type;
      const BasicType* const tiny_client_type;
      const GenerativeType* const tiny_flow_mod_type;
      Namespace* const global_ns;
      Namespace* const liz_ns;
      ModeStack exit_modes;  // stack of expected return values.
      int parms_level;       // current parameters nesing level;
      int new_formal_counter;
      FunctionElaboration arity_fun; // built-in function Arity
      FunctionElaboration input_type_fun; // built-in function InputType
      FunctionElaboration codomain_fun;   // built-in function Codomain
      FunctionElaboration type_eq_fun; // holds equality operator on types.
      EvidenceRepository evidences;
   public:
      std::vector<DeclContext*> decls;
   private:
      AllSpecs specs;
      
      void define_binary_builtin(const std::string&, BinaryBuiltinFunction,
                                 const Type*, const Type*, const Type*);
      
      // define all builtin types in the global environment.
      void define_builtin_types();

      // define all intrinsic operations.
      void define_intrinsics();
   };

   // Return default elaboration of a type expression.
   TypeElaboration type_elaboration(Elaborator*, const Type*);

   // Return a function type for a binary predicate taking arguments
   // of type denotated by `t'.
   const ArrowType* get_binary_predicate_type(Elaborator*, TypeElaboration);

   FunctionElaboration
   lookup_operator(Elaborator*, const std::string&, const ArrowType*);

   // Return a pointer to the declaration of equality operator
   // predicate with a given argument type.
   const Function* select_equality_operator(Elaborator*, const Type*);

   const Lambda* substitute(Elaborator*, const Lambda*, Substitution);
   FunctionElaboration
   substitute(Elaborator*, FunctionElaboration, const Substitution&);
   SimpleFuncElaboration
   substitute(Elaborator*, SimpleFuncElaboration, const Substitution&);
   DependentElaboration
   substitute(Elaborator*, DependentElaboration, const Substitution&);

   Elaboration
   subst_expr(Elaborator*, Elaboration, const Substitution&);

   FunctionElaboration
   substitute(Elaborator*, FunctionElaboration, const Evidence&);
   SimpleFuncElaboration
   substitute(Elaborator*, SimpleFuncElaboration, const Evidence&);
   DependentElaboration
   substitute(Elaborator*, DependentElaboration, const Evidence&);

   Elaboration subst_expr(Elaborator*, Elaboration, const Evidence&);

   // Pattern match an expression (first argument) against a pattern
   // (second argument), and return the matching substitution;
   Substitution pattern_match(Elaboration, const Expression*);

   // Attempt to simplify an elaboration as much as it is safe
   // to do.  This routine is most useful during elaboration and
   // and property deduction.
   Elaboration evaluate(Elaborator*, Elaboration);
   TypeElaboration evaluate(Elaborator*, TypeElaboration);

   // Attempt to prove an arithmetic expression from the assumptions in the
   // AssumptionSet.
   bool can_prove_arithmetic(Elaborator&, Elaboration);

   Elaboration eq_close_term_if_can(EquivalenceQuotient&, Elaboration);

   inline TypeElaboration
   convert_syntax_to_type(Elaborator* context, const char* s) {
      auto expr = context->get_unambiguous_binding(make_identifier(context, s));
      return context->coerce_to_type(*expr);
   }

   // ------------------
   // -- ScopeManager --
   // ------------------
   // This is a helper class automating push/pop of name binding contours.
   struct ScopeManager {
      ScopeManager(Elaborator*, Scope&);
      Elaborator* context() const { return ctx; }
      ~ScopeManager();
   private:
      Elaborator* ctx;
   };

   // ------------------------
   // -- UnificationContext --
   // ------------------------
   struct UnificationContext {
      const Elaborator* elab;
      Formals formals;
      Substitution subst;

      bool unifiable(const Formal*) const;
      bool has(const Formal*) const;
      const Elaborator* elaborator() const { return elab; }

      // -- Pattern Matching
      // match_type(expr, pat) holds if pattern `pat' can match
      // type expression `expr' in the current unification context.
      bool match_type(TypeElaboration, const Type*);
   };

}

#endif  // LIZ_ELABORATOR_INCLUDED
