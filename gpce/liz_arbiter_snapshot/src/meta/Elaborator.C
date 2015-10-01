// Copyright (C) 2012-2013, Texas A&M University.
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

#include <limits.h>
#include <algorithm>
#include "Elaborator.H"

namespace liz {
   // -- UnificationContext
   bool
   UnificationContext::unifiable(const Formal* p) const {
      return std::find(formals.begin(), formals.end(), p) != formals.end();
   }

   bool
   UnificationContext::match_type(TypeElaboration t, const Type* x) {
      return match(t, x, subst);
   }

   // -- Elaborator::LocationManager
   Elaborator::LocationManager::LocationManager(Elaborator* c, const Token& t)
         : ctx(c) {
      ctx->locs.push(t);
   }

   Elaborator::LocationManager::~LocationManager() {
      ctx->locs.pop();
   }

   const Token&
   Elaborator::current_location() const {
      return locs.top();
   }

   void
   Elaborator::sorry(const std::string& m) {
      throw BasicError(m);
   }

   TypeElaboration
   Elaborator::get_typename() const {
      return TypeElaboration(typename_type, typename_type);
   }

   TypeElaboration
   Elaborator::get_LoadUnit() const {
      return { typename_type, LoadUnit_type };
   }

   TypeElaboration
   Elaborator::get_namespace() const {
      return TypeElaboration(typename_type, namespace_type);
   }

   const BasicType*
   Elaborator::get_prop() const {
      return prop_type;
   }

   TypeElaboration
   Elaborator::get_address() const {
      return TypeElaboration(typename_type, address_type);
   }

   TypeElaboration
   Elaborator::get_void() const {
      return TypeElaboration(typename_type, void_type);
   }

   TypeElaboration
   Elaborator::get_bool() const {
      return TypeElaboration(typename_type, bool_type);
   }

   TypeElaboration
   Elaborator::get_byte() const {
      return TypeElaboration(typename_type, byte_type);
   }

   TypeElaboration
   Elaborator::get_char() const {
      return TypeElaboration(typename_type, char_type);
   }

   TypeElaboration
   Elaborator::get_int() const {
      return TypeElaboration(typename_type, int_type);
   }

   TypeElaboration
   Elaborator::get_key() const {
      return TypeElaboration(typename_type, key_type);
   }

   TypeElaboration
   Elaborator::get_double() const {
      return TypeElaboration(typename_type, double_type);
   }

   TypeElaboration
   Elaborator::get_string() const {
      return TypeElaboration(typename_type, string_type);
   }

   TypeElaboration
   Elaborator::get_tiny_client() const {
      return TypeElaboration(typename_type, tiny_client_type);
   }

   TypeElaboration
   Elaborator::get_flow_mod() const {
      return TypeElaboration(typename_type, tiny_flow_mod_type);
   }

   const Evidence*
   Elaborator::find_evidence(const Expression* c) const {
      EvidenceRepository::const_iterator p = evidences.find(c);
      return p == evidences.end() ? nullptr : &p->second;
   }

   const Evidence*
   Elaborator::register_evidence(const Expression* c, const Evidence& e) {
      auto result = evidences.insert(std::make_pair(c, e));
      if (not result.second)
         internal_error("evidence for constraint " + quote(show_expr(c))
                         + " already in effect");
      return &result.first->second;
   }

   const Expression*
   Elaborator::retrieve_specialization(const Expression* t,
                                       const Substitution& args) const {
      AllSpecs::const_iterator p = specs.find(t);
      if (p == specs.end())
         return nullptr;
      SpecializationRepository::const_iterator q = p->second.find(args);
      if (q == p->second.end())
         return nullptr;
      return q->second;
   }

   void
   Elaborator::register_specialization(const Expression* x,
                                       const Expression* t,
                                       const Substitution& args) {
      specs[t][args] = x;
   }

   void
   Elaborator::push_return_type(const Type* t) {
      exit_modes.push(t);
   }

   const Type*
   Elaborator::get_return_type() const {
      return exit_modes.top();
   }

   void
   Elaborator::pop_return_type() {
      exit_modes.pop();
   }

   int
   Elaborator::frame_depth() const {
      return exit_modes.size();
   }

   Elaborator::DeclContext*
   Elaborator::top_decl_context() const {
      return decls.empty() ? nullptr : decls.back();
   }

   Elaborator::DeclContext*
   Elaborator::previous_decl_context() const {
      auto n = decls.size();
      if (n > 1)
         return decls[n - 2];
      return nullptr;
   }

   LoadUnit*
   Elaborator::get_unit_if_loaded(const Name* n) const {
      auto p = loaded_modules.find(n);
      if (p == loaded_modules.end())
         return nullptr;
      return p->second;
   }

   static void
   define_basic_type(Elaborator* ctx, const BasicType* value) {
      auto name = value->name();
      ctx->global_scope()->define(name, ctx->get_typename(), value);
   }
   
   static void
   define_concept_ctor(Elaborator* ctx, const Constructor* value) {
      auto name = value->name();
      ctx->global_scope()->define(name, value->type(), value);
   }

   static void
   define_quantified_concept_ctor(Elaborator* ctx, const Lambda* value) {
      auto name = value->name();
      ctx->global_scope()->define(name, value->type(), value);
   }

   static Elaboration
   read(Elaborator& ctx, const Formal* f) {
      auto rt = ctx.make_reference_type(f->type());
      return { f->type(), ctx.build_read({ rt, f }) };
   }

   static const Lambda*
   make_coerce_function(Elaborator& ctx, const Name* name,
                        const Type* s, const Type* t) {
      TypeElaboration target = { ctx.get_typename(), t };
      InputTypes source = { { ctx.get_typename(), s} };
      auto ft = ctx.make_arrow_type(target, source);
      auto parm = ctx.build_formal(0, 0, source.front(),
                                   { ctx.fresh_name(), source.front() });
      return ctx.build_lambda({ name, ft }, Formals{ parm }, read(ctx, parm));
   }

   static void
   define_coercion(Elaborator& ctx, const char* n, const Type* s, const Type* t)
   {
      auto name = make_identifier(&ctx, n);
      auto fun = make_coerce_function(ctx, name, s, t);
      ctx.global_scope()->define(name, fun->type(), fun);
   }

   static void
   define_generative_type(Elaborator& ctx, const GenerativeType& gen_t) {
      auto name = gen_t.name();
      ctx.global_scope()->define(name, ctx.get_typename(), &gen_t);
      auto rep_t = gen_t.value().code();
      define_coercion(ctx, "rep", &gen_t, rep_t);
      define_coercion(ctx, "per", rep_t, &gen_t);
   }

   void
   Elaborator::define_builtin_types() {
      define_basic_type(this, typename_type);
      define_basic_type(this, LoadUnit_type);
      define_basic_type(this, namespace_type);
      define_basic_type(this, concept_type);
      define_basic_type(this, axiom_type);
      define_basic_type(this, prop_type);
      define_basic_type(this, arithmetic_type);
      define_concept_ctor(this, Function_ctor);
      define_concept_ctor(this, Record_ctor);
      define_concept_ctor(this, Regular_ctor);
      define_concept_ctor(this, SubKey_ctor);
      define_concept_ctor(this, ValidKey_ctor);
      define_concept_ctor(this, ProjectionOf_ctor);
      define_quantified_concept_ctor(this, InstanceOf_ctor);
      
      define_basic_type(this, void_type);
      define_basic_type(this, address_type);
      define_basic_type(this, bool_type);
      define_basic_type(this, byte_type);
      define_basic_type(this, char_type);
      define_basic_type(this, int_type);
      define_basic_type(this, key_type);
      define_basic_type(this, double_type);
      define_basic_type(this, string_type);
      define_basic_type(this, tiny_client_type);
      define_generative_type(*this, *tiny_flow_mod_type);
   }


   static void
   define_constant_compile_time_only(Elaborator* ctx, const std::string& name,
                                     const Value* value, const Type* type) {
      ctx->global_scope()->bind(make_identifier(ctx, name), { type, value });
   }

   static void
   define_constant(Elaborator* ctx, const std::string& name,
                   const Value* value, const Type* type) {
      ctx->global_scope()->define(make_identifier(ctx, name), type, value);
   }
   
   // define all builtin values in the global environment.
   static void
   define_builtin_values(Elaborator* ctx) {
      define_constant_compile_time_only
         (ctx, "false", ctx->build_bool(false, ctx->get_bool()), ctx->get_bool());
      define_constant_compile_time_only
         (ctx, "true", ctx->build_bool(true, ctx->get_bool()), ctx->get_bool());
      define_constant(ctx, "Liz", ctx->liz_scope(), ctx->get_namespace());
   }

   // -- typename data property
   static void
   fmt_typename(Data::Abstract, std::ostream& os, Data::Value v) {
      // Doesn't work when this is an instance.
      auto e = static_cast<const Expression*>(Data::Abstract(v));
      if (is<Instance>(e))
         os << pretty(e);
      else
         Data::to_type(v)->print_on(os);
   }

   static const Data::Property typename_prop = {
      Data::Mode::Pointer, sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract), { fmt_typename, nullptr }
   };

   // -- LoadUnit data property
   static void
   fmt_LoadUnit(Data::Abstract, std::ostream& os, Data::Value v) {
      os << Data::to_LoadUnit(v)->path();
   }

   static const Data::Property LoadUnit_traits = {
      Data::Mode::Pointer, sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract), { fmt_LoadUnit, nullptr }
   };

   // -- Logical proposition
   static void
   fmt_prop(Data::Abstract, std::ostream& os, Data::Value v) {
      os << pretty(Data::to_quote(v));
   }

   static const Data::Property prop_traits = {
      Data::Mode::Pointer,
      sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract),
      { fmt_prop, nullptr }
   };

   // -- Concept data property
   static void
   fmt_concept(Data::Abstract, std::ostream& os, Data::Value v) {
      auto inst = static_cast<const Instance*>(Data::Abstract(v));
      os << *inst;
   }

   static const Data::Property concept_prop = {
      Data::Mode::Pointer, sizeof(Data::Abstract) * CHAR_BIT,
      alignof(Data::Abstract), { fmt_concept, nullptr }
   };

   // Return a function type for a binary predicate
   // taking arguments of type denotated by `t'.
   const ArrowType*
   get_binary_predicate_type(Elaborator* context, TypeElaboration t) {
      return context->make_arrow_type(context->get_bool(), InputTypes(2, t));
   }
   
   // -- Return the builtin declaration for type equality operator
   static FunctionElaboration
   type_equality_operator(Elaborator* context) {
      return lookup_function
         (context->global_env(), make_operator(context, "=="),
          get_binary_predicate_type(context, context->get_typename()));
   }
   
   static const BasicType*
   ref_to_type(Elaborator* context, const char* n, const Data::Property* p) {
      return context->make_basic_type(make_identifier(context, n), p);
   }

   static const BasicType*
   ref_to_type(Elaborator* context, const char* n, Data::Mode m) {
      return ref_to_type(context, n, Data::property(m));
   }

   static TypeElaboration
   make_uint_type(Elaborator& ctx, int bit_width) {
      auto int_e = ctx.build_int(bit_width, ctx.get_int());
      return { ctx.get_typename(),
               ctx.make_uint_type({ ctx.get_int(), int_e }) };
   }

   static TypeElaboration
   make_array_type(Elaborator& ctx, TypeElaboration t) {
      TypeElaboration arr_t(ctx.get_typename(), ctx.make_array_type(t)); 
      return arr_t;
   }

   static const GenerativeType*
   make_flow_mod_type(Elaborator& ctx) {
      Sequence<TagType> components;
      components.reserve(6);
      components.push_back(
         ctx.make_tag_type(make_identifier(&ctx, "match_set"),
                           make_array_type(ctx, ctx.get_char())));
      components.push_back(
         ctx.make_tag_type(make_identifier(&ctx, "command"),
                           make_uint_type(ctx, 8)));
      components.push_back(
         ctx.make_tag_type(make_identifier(&ctx, "hard_timeout"),
                           make_uint_type(ctx,16)));
      components.push_back(
         ctx.make_tag_type(make_identifier(&ctx, "buffer_id"),
                           make_uint_type(ctx,32)));
      components.push_back(
         ctx.make_tag_type(make_identifier(&ctx, "apply"),
                           make_array_type(ctx, make_uint_type(ctx,32))));
      components.push_back(
         ctx.make_tag_type(make_identifier(&ctx, "goto_id"),
                           make_uint_type(ctx,8)));
      TypeElaboration rec_t(ctx.get_typename(),
                            ctx.make_record_type(components));
      auto name = make_identifier(&ctx, "TinyFlowMod");
      return ctx.make_generative_type(name, rec_t, ctx.current_env().base());
   }

   static const Constructor*
   builtin_unary_concept_ctor(Elaborator* ctx, const char* n) {
      InputTypes source { { ctx->get_typename(), ctx->get_typename() } };
      TypeElaboration target { ctx->get_typename(), ctx->get_concept() };
      auto type = ctx->make_arrow_type(target, source);
      LinkName lnk { make_identifier(ctx, n), type };
      return ctx->build_constructor(lnk, { });
   }

   static const Constructor*
   builtin_binary_concept_ctor(Elaborator* ctx, const char* n) {
      InputTypes source { { ctx->get_typename(), ctx->get_typename() },
                          { ctx->get_typename(), ctx->get_typename() } };
      TypeElaboration target { ctx->get_typename(), ctx->get_concept() };
      auto type = ctx->make_arrow_type(target, source);
      LinkName lnk { make_identifier(ctx, n), type };
      return ctx->build_constructor(lnk, { });
   }

   static const Type*
   to_type_var(Elaborator& ctx, const Formal* f) {
      const Read* r = ctx.build_read({ ctx.get_typename(), f });
      return ctx.make_type_expression({ ctx.get_typename() , r });
   }

   static const Lambda*
   builtin_instance_of_concept_ctor(Elaborator& ctx) {
      LinkName lnkF { make_identifier(&ctx, "F"), ctx.get_typename().code() };
      auto F = ctx.build_formal(0,0, ctx.get_typename(), lnkF);
      TypeElaboration F_t(ctx.get_typename(), to_type_var(ctx, F));
      InputTypes source = { ctx.get_typename(), F_t };
      TypeElaboration target { ctx.get_typename(), ctx.get_concept() };
      TypeElaboration t(ctx.get_typename(),
                        ctx.make_arrow_type(target, source));
      Elaboration Func(ctx.get_concept(),
                       ctx.build_constraint(ctx.get_Function(),Arguments{F_t}));
      auto type =
         ctx.make_quantified_type(Quantifier::forall, Formals(F), t, Func);
      auto name = make_identifier(&ctx, "InstanceOf");
      auto ctor = ctx.build_constructor({ name, t }, { });
      return ctx.build_lambda({ name, type }, Formals(F), { t, ctor });
   }

   static const Constructor*
   builtin_binary_key_concept_ctor(Elaborator* ctx, const char* n) {
      InputTypes source { { ctx->get_typename(), ctx->get_key() },
                          { ctx->get_typename(), ctx->get_key() } };
      TypeElaboration target { ctx->get_typename(), ctx->get_concept() };
      auto type = ctx->make_arrow_type(target, source);
      LinkName lnk { make_identifier(ctx, n), type };
      return ctx->build_constructor(lnk, { });
   }

   static const Constructor*
   builtin_unary_key_concept_ctor(Elaborator* ctx, const char* n) {
      InputTypes source { { ctx->get_typename(), ctx->get_key() } };
      TypeElaboration target { ctx->get_typename(), ctx->get_concept() };
      auto type = ctx->make_arrow_type(target, source);
      LinkName lnk { make_identifier(ctx, n), type };
      return ctx->build_constructor(lnk, { });
   }

   Elaborator::Elaborator(Reader& r)
         : rdr(&r),
           concept_type(ref_to_type(this, "concept", &concept_prop)),
           typename_type(ref_to_type(this, "type", &typename_prop)),
           LoadUnit_type(ref_to_type(this, "LoadUnit", &LoadUnit_traits)),
           prop_type(ref_to_type(this, "prop", &prop_traits)),
           axiom_type(ref_to_type(this, "axiom", Data::Mode::Pointer)),
           arithmetic_type(ref_to_type(this, "arithmetic",Data::Mode::Pointer)),
           key_type(ref_to_type(this, "Key", Data::Mode::Pointer)),
           Function_ctor(builtin_unary_concept_ctor(this, "Function")),
           Record_ctor(builtin_unary_concept_ctor(this, "Record")),
           Regular_ctor(builtin_unary_concept_ctor(this, "Regular")),
           SubKey_ctor(builtin_binary_key_concept_ctor(this, "SubKey")),
           ValidKey_ctor(builtin_unary_key_concept_ctor(this, "ValidKey")),
           ProjectionOf_ctor(builtin_binary_concept_ctor(this, "ProjectionOf")),
           InstanceOf_ctor(builtin_instance_of_concept_ctor(*this)),
           
           namespace_type(ref_to_type(this, "namespace", Data::Mode::Pointer)),

           void_type(ref_to_type(this, "void", Data::Mode::Void)),
           address_type(ref_to_type(this, "address", Data::Mode::Pointer)),
           bool_type(ref_to_type(this, "bool", Data::Mode::Bool)),
           byte_type(ref_to_type(this, "byte", Data::Mode::Byte)),
           char_type(ref_to_type(this, "char", Data::Mode::Char)),
           int_type(ref_to_type(this, "int", Data::Mode::Int)),
           double_type(ref_to_type(this, "double", Data::Mode::Dfloat)),
           string_type(ref_to_type(this, "string", Data::Mode::String)),
           tiny_client_type(ref_to_type(this, "TinyClient", Data::Mode::Int)),
           tiny_flow_mod_type(make_flow_mod_type(*this)),
           global_ns(build_namespace(nullptr, nullptr)),
      liz_ns(build_namespace(make_identifier(this, "Liz"), global_ns)),
           parms_level(-1),
           arity_fun(), input_type_fun(), codomain_fun(), type_eq_fun()
   {
      push(global_ns);       // install global scope

      define_builtin_types();
      define_builtin_values(this);
      define_intrinsics();
      type_eq_fun = type_equality_operator(this);

      // Cache some builtin functions.
      Elaboration* decl = get_unambiguous_binding(make_identifier(this, "Arity"));
      if (decl == nullptr)
         internal_error("lookup for " + quote("Arity") + " failed");
      arity_fun = make_elaboration(to_dependent_function(decl->code()));
      decl = get_unambiguous_binding(make_identifier(this, "InputType"));
      if (decl == nullptr)
         internal_error("lookup for " + quote("InputType") + " failed");
      input_type_fun = make_elaboration(to_dependent_function(decl->code()));
      decl = get_unambiguous_binding(make_identifier(this, "Codomain"));
      if (decl == nullptr)
         internal_error("lookup for " + quote("Codomain") + " failed");
      codomain_fun = make_elaboration(to_dependent_function(decl->code()));
   }

   Elaborator::~Elaborator()
   { }
}
