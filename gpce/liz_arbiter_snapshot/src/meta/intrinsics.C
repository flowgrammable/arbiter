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


// This file implements Liz intrinsic operations.

#include <functional>
#include <fstream>
#include <cstdlib>
#include <algorithm>
#include "Evaluator.H"

namespace liz {
   // -- Builtin names.
   struct BuiltinName {
      const char* text;
      const bool is_op;
   };

   static const Name*
   builtin_name(Elaborator* ctx, const BuiltinName& x) {
      if (x.is_op)
         return make_operator(ctx, x.text);
      return make_identifier(ctx, x.text);
   }

   struct UnaryFunctionType {
      const char* target;
      const char* source;
   };

   static const ArrowType*
   make_type(Elaborator* ctx, const UnaryFunctionType& x) {
      InputTypes src(1, convert_syntax_to_type(ctx, x.source));
      auto tgt = convert_syntax_to_type(ctx, x.target);
      return ctx->make_arrow_type(tgt, src);
   }

   struct BinaryFunctionType {
      const char* target;
      const char* source[2];
   };

   static const ArrowType*
   make_type(Elaborator* ctx, const BinaryFunctionType& x) {
      InputTypes src(2);
      src[0] = convert_syntax_to_type(ctx, x.source[0]);
      src[1] = convert_syntax_to_type(ctx, x.source[1]);
      return ctx->make_arrow_type(convert_syntax_to_type(ctx, x.target), src);
   }

   template<typename C, typename T>
   struct Intrinsic {
      BuiltinName name;
      C code;
      T type;
   };

   template<typename C, typename T, int N>
   static void
   define_intrinsics(Elaborator* ctx, const Intrinsic<C, T> (&table)[N]) {
      for (auto& op : table) {
         auto n = builtin_name(ctx, op.name);
         const ArrowType* t = make_type(ctx, op.type);
         ctx->global_scope()->define(n, t, ctx->build_builtin(n, t, op.code, Arguments()));
      }
   }

   // --------------------------------
   // -- Unary intrinsic operations --
   // --------------------------------

   // Evaluate the instantiation of the intrinsic type constructor
   // 'reference' with 't'
   static Data::Value
   intrinsic_reference(Data::Abstract env, Data::Value v) {
      auto ctx = static_cast<Evaluator*>(env);
      auto t = static_cast<const Type*>(Data::Location(v));
      TypeElaboration referee(ctx->get_typename(), t);
      return Data::Value(ctx->make_reference_type(referee));
   }

   static Data::Value
   intrinsic_rep_type(Data::Abstract, Data::Value v) {
      auto t = static_cast<const Type*>(Data::Location(v));
      if (auto gt = is<GenerativeType>(t))
        return gt->value().code();
      return Data::Value(t);
   }

   static Data::Value
   intrinsic_uint(Data::Abstract env, Data::Value v) {
      auto ctx = static_cast<Evaluator*>(env);
      auto e = static_cast<const Expression*>(Data::Abstract(v));
      Elaboration width { ctx->get_int(), e };
      return Data::Value(ctx->make_uint_type(width));
   }

   template<typename T, typename Op>
   static Data::Value
   builtin_unary(Data::Abstract, const Arguments&, Data::Value v) {
      return Data::Value(Op()(T(v)));
   }

   template <Data::Value Op(Data::Abstract, Data::Value)>
   static Data::Value
   no_params_un(Data::Abstract c, const Arguments&, Data::Value v) {
      return Data::Value(Op(c, v));
   }

   template <Data::Value Op(Data::Abstract, Data::Value, Data::Value)>
   static Data::Value
   no_params_2(Data::Abstract c, const Arguments&, Data::Value v0,
               Data::Value v1)
   {
      return Data::Value(Op(c, v0, v1));
   }

   template <Data::Value Op(Data::Abstract,Data::Value,Data::Value,Data::Value)>
   static Data::Value
   no_params_3(Data::Abstract c, const Arguments&, Data::Value v0,
               Data::Value v1, Data::Value v2)
   {
      return Data::Value(Op(c, v0, v1, v2));
   }

   // -- implement intrisic exit operation
   static Data::Value
   intrinsic_exit(Data::Abstract env, Data::Value value) {
      auto ctx = static_cast<Evaluator*>(env);
      ctx->output() << std::endl;
      flush(ctx->error());
      flush(ctx->debug());
      std::exit(intmax_t(value));
      return { };               // never executed.
   }
   
   static Data::Value
   intrinsic_print_string(Data::Abstract env,  Data::Value v) {
      auto ctx = static_cast<Evaluator*>(env);
      Symbol s = v;
      ctx->output() << s.string();
      return { };
   }
   
   static Data::Value
   intrinsic_print_int(Data::Abstract env, Data::Value value) {
      auto ctx = static_cast<Evaluator*>(env);
      ctx->output() << intmax_t(value);
      return { };
   }
   
   static Data::Value
   intrinsic_print_double(Data::Abstract env, Data::Value v) {
      auto ctx = static_cast<Evaluator*>(env);
      ctx->output() << double(v);
      return { };
   }
   
   static Data::Value
   intrinsic_print_char(Data::Abstract env, Data::Value v) {
      auto ctx = static_cast<Evaluator*>(env);
      ctx->output() << Character(v);
      return { };
   }

   // Implement the `bits' function
   static Data::Value
   intrinsic_bits(Data::Abstract, Data::Value v) {
      auto t = static_cast<const Type*>(Data::Location(v));
      return Data::Value(intmax_t(minimum_bit_width(t)));
   }

   // Implement the `bits' function
   static Data::Value
   intrinsic_bytes(Data::Abstract, Data::Value v) {
      auto t = static_cast<const Type*>(Data::Location(v));
      return Data::Value(intmax_t(minimum_bit_width(t)/8));
   }

   // Implement array type constructor
   static Data::Value
   intrinsic_array_ctor(Data::Abstract env, Data::Value v) {
      auto ctx = static_cast<Evaluator*>(env);
      auto t = static_cast<const Type*>(Data::Location(v));
      return Data::Value(ctx->make_array_type({ ctx->get_typename(), t }));
   }

   struct complement {
      template<typename T>
      T operator()(T t) const { return ~t; }
   };

   // -- registration of unary intrinsics --

   // This datatype help collect static description about
   // unary intrinsic operations.
   using UnaryIntrinsic = Intrinsic<Function::Unary, UnaryFunctionType>;

   // This table lists all unary intrinsic operations currently
   // implemented as C++ code.
   const UnaryIntrinsic unary_intrinsic_table[] = {
      { { "ref", false },
        no_params_un<intrinsic_reference>, { "type", "type" } },
      { { "RepType", false },
        no_params_un<intrinsic_rep_type>, { "type", "type" } },
      { { "uint", false }, no_params_un<intrinsic_uint>, { "type", "int" } },
      { { "not", true },
        builtin_unary<bool, std::logical_not<bool>>,
        { "bool", "bool" } },
      { { "~", true }, builtin_unary<byte, complement>, { "byte", "byte" } },
      { { "~", true }, builtin_unary<intmax_t, complement>, { "int", "int" } },
      { { "-", true },
        builtin_unary<intmax_t, std::negate<intmax_t>>,
        { "int", "int" } },
      { { "-", true },
        builtin_unary<double, std::negate<double>>,
        { "double", "double" } },
      { { "exit", false }, no_params_un<intrinsic_exit>, { "void", "int" } },
      { { "bits", false }, no_params_un<intrinsic_bits>, { "int", "type" } },
      { { "bytes", false }, no_params_un<intrinsic_bytes>, { "int", "type" } },
      { { "print", false },
        no_params_un<intrinsic_print_int>, { "void", "int" } },
      { { "print", false },
        no_params_un<intrinsic_print_double>, { "void", "double" } },
      { { "print", false },
        no_params_un<intrinsic_print_char>, { "void", "char" } },
      { { "print", false },
        no_params_un<intrinsic_print_string>, { "void", "string" } },
      { { "Array", false },
        no_params_un<intrinsic_array_ctor>, { "type", "type" } }
   };

   // ---------------------------------
   // -- Binary intrinsic operations --
   // ---------------------------------

   // -- builtin functions

   template<typename T, typename Op>
   static Data::Value
   builtin_binary(Data::Abstract, Data::Value lhs, Data::Value rhs) {
      return Data::Value(Op()(T(lhs), T(rhs)));
   }

   template<typename Op>
   Data::Value
   builtin_div_int(Data::Abstract, Data::Value lhs, Data::Value rhs) {
      if (intmax_t(rhs) == 0)
         evaluation_error("integer division by zero");
      return Data::Value(Op()(intmax_t(lhs), intmax_t(rhs)));
   }

   static Data::Value
   intrinsic_key_eq(Data::Abstract, Data::Value lhs, Data::Value rhs) {
      SubsetKeyValue k_lhs = *Data::to_key(lhs);
      SubsetKeyValue k_rhs = *Data::to_key(rhs);
      return Data::Value(key_equal(k_lhs, k_rhs));
   }

   static Data::Value
   intrinsic_key_ineq(Data::Abstract, Data::Value lhs, Data::Value rhs) {
      SubsetKeyValue k_lhs = *Data::to_key(lhs);
      SubsetKeyValue k_rhs = *Data::to_key(rhs);
      return Data::Value(not key_equal(k_lhs, k_rhs));
   }

   // Implement builtin type equality
   static Data::Value
   intrinsic_type_eq(Data::Abstract, Data::Value lhs, Data::Value rhs) {
      return Data::Value(Data::to_type(lhs) == Data::to_type(rhs));
   }

   // Implement array type constructor
   static Data::Value
   intrinsic_fixed_array_ctor(Data::Abstract env, Data::Value v1,
                              Data::Value v2)
   {
      auto ctx = static_cast<Evaluator*>(env);
      TypeElaboration t { ctx->get_typename(),
                          static_cast<const Type*>(Data::Location(v1)) };
      Elaboration e = { ctx->get_int(),
                        static_cast<const Expression*>(Data::Abstract(v2)) };
      return Data::Value(ctx->make_fixed_array_type(t,e));
   }


   // -- Registration of binary intrinsics --

   using BinaryIntrinsic  = Intrinsic<Function::Binary, BinaryFunctionType>;

   const BinaryIntrinsic binary_intrinsic_table[] = {
      { { "/\\", true },
        no_params_2<builtin_binary<bool, std::logical_and<bool>>>,
        { "bool", { "bool", "bool" } } },
      { { "\\/", true },
        no_params_2<builtin_binary<bool, std::logical_or<bool>>>,
        { "bool", { "bool", "bool" } } },
      { { "/\\", true },
        no_params_2<builtin_binary<byte, std::bit_and<byte>>>,
        { "byte", { "byte", "byte" } } }, 
      { { "\\/", true },
        no_params_2<builtin_binary<byte, std::bit_or<byte>>>,
        { "byte", { "byte", "byte" } } }, 
      { { "/\\", true },
        no_params_2<builtin_binary<intmax_t, std::bit_and<intmax_t>>>,
        { "int", { "int", "int" } } }, 
      { { "\\/", true },
        no_params_2<builtin_binary<intmax_t, std::bit_or<intmax_t>>>,
        { "int", { "int", "int" } } },
      { { "+", true },
        no_params_2<builtin_binary<intmax_t, std::plus<intmax_t>>>,
        { "int", { "int", "int" } } },
      { { "+", true },
        no_params_2<builtin_binary<double, std::plus<double>>>,
        { "double", { "double", "double" } } },
      { { "-", true },
        no_params_2<builtin_binary<intmax_t, std::minus<intmax_t>>>,
        { "int", { "int", "int" } } },
      { { "-", true },
        no_params_2<builtin_binary<double, std::minus<double>>>,
        { "double", { "double", "double" } } },
      { { "*", true },
        no_params_2<builtin_binary<intmax_t, std::multiplies<intmax_t>>>,
        { "int", { "int", "int" } } },
      { { "*", true },
        no_params_2<builtin_binary<double, std::multiplies<double>>>,
        { "double", { "double", "double" } } },
      { { "div", true },
        no_params_2<builtin_div_int<std::divides<intmax_t>>>,
        { "int", { "int", "int" } } },
      { { "rem", true },
        no_params_2<builtin_binary<intmax_t, std::modulus<intmax_t>>>,
        { "int", { "int", "int" } } },
      { { "/", true },
        no_params_2<builtin_binary<double, std::divides<double>>>,
        { "double", { "double", "double" } } },
      { { ">", true },
        no_params_2<builtin_binary<byte, std::greater<byte>>>,
        { "bool", { "byte", "byte" } } },
      { { ">", true },
        no_params_2<builtin_binary<intmax_t, std::greater<intmax_t>>>,
        { "bool", { "int", "int" } } },
      { { ">", true },
        no_params_2<builtin_binary<double, std::greater<double>>>,
        { "bool", { "double", "double" } } },
      { { ">=", true },
        no_params_2<builtin_binary<byte, std::greater_equal<byte>>>,
        { "bool", { "byte", "byte" } } },
      { { ">=", true },
        no_params_2<builtin_binary<intmax_t, std::greater_equal<intmax_t>>>,
        { "bool", { "int", "int" } } },
      { { ">=", true },
        no_params_2<builtin_binary<double, std::greater_equal<double>>>,
        { "bool", { "double", "double" } } },
      { { "<", true },
        no_params_2<builtin_binary<byte, std::less<byte>>>,
        { "bool", { "byte", "byte" } } },
      { { "<", true },
        no_params_2<builtin_binary<intmax_t, std::less<intmax_t>>>,
        { "bool", { "int", "int" } } },
      { { "<", true },
        no_params_2<builtin_binary<double, std::less<double>>>,
        { "bool", { "double", "double" } } },
      { { "<=", true },
        no_params_2<builtin_binary<byte, std::less_equal<byte>>>,
        { "bool", { "byte", "byte" } } },
      { { "<=", true },
        no_params_2<builtin_binary<intmax_t, std::less_equal<intmax_t>>>,
        { "bool", { "int", "int" } } },
      { { "<=", true },
        no_params_2<builtin_binary<double, std::less_equal<double>>>,
        { "bool", { "double", "double" } } },
      { { "==", true },
        no_params_2<builtin_binary<bool, std::equal_to<bool>>>,
        { "bool", { "bool", "bool" } } },
      { { "==", true },
        no_params_2<builtin_binary<byte, std::equal_to<byte>>>,
        { "bool", { "byte", "byte" } } },
      { { "==", true },
        no_params_2<builtin_binary<intmax_t, std::equal_to<intmax_t>>>,
        { "bool", { "int", "int" } } },
      { { "==", true },
        no_params_2<builtin_binary<double, std::equal_to<double>>>,
        { "bool", { "double", "double" } } },
      { { "==", true }, no_params_2<intrinsic_key_eq>,
        { "bool", { "Key", "Key" } } },
      { { "!=", true }, no_params_2<intrinsic_key_ineq>,
        { "bool", { "Key", "Key" } } },
      { { "!=", true },
        no_params_2<builtin_binary<bool, std::not_equal_to<bool>>>,
        { "bool", { "bool", "bool" } } },
      { { "!=", true },
        no_params_2<builtin_binary<byte, std::not_equal_to<byte>>>,
        { "bool", { "byte", "byte" } } },
      { { "!=", true },
        no_params_2<builtin_binary<intmax_t, std::not_equal_to<intmax_t>>>,
        { "bool", { "int", "int" } } },
      { { "!=", true },
        no_params_2<builtin_binary<double, std::not_equal_to<double>>>,
        { "bool", { "double", "double" } } },
      { { "==", true }, no_params_2<intrinsic_type_eq>,
        { "bool", { "type", "type" } } },
      { { "FixedArray", false },
        no_params_2<intrinsic_fixed_array_ctor>, { "type", "type", "int" } }
   };

   namespace {
      static const Formal*
      gen_intvar_formal(Elaborator* ctx, const char* n) {
         auto name = make_identifier(ctx, n);
         const LinkName lnk { name, ctx->get_int() };
         TypeElaboration tt(ctx->get_typename(), ctx->get_int());
         return ctx->build_formal(0, 0, tt, lnk);
      }

      static const Formal*
      gen_stringvar_formal(Elaborator* ctx, const char* n) {
         auto name = make_identifier(ctx, n);
         const LinkName lnk { name, ctx->get_string() };
         TypeElaboration tt(ctx->get_typename(), ctx->get_string());
         return ctx->build_formal(0, 0, tt, lnk);
      }

      static Elaboration
      read_formal(Elaborator* ctx, const Formal* f) {
         auto rt = ctx->make_reference_type(f->type());
         return { f->type(), ctx->build_read({ rt, f }) };
      }

      template <typename Func>
      static Elaboration
      abstract_builtin_over(Elaborator* ctx, const Formals& fs, const Type* t,
                            const Func* func) {
         auto qt = ctx->make_quantified_type(Quantifier::forall, fs,
            TypeElaboration{ ctx->get_typename(), t }, { });
         auto tlam = ctx->build_lambda(func->link_name(), fs, { t, func });
         return { qt, tlam };
      }

      // This should be in its own namespace for defining more complex built-ins
      static const Formal*
      gen_tvar_formal(Elaborator* ctx, const char* n) {
         auto name = make_identifier(ctx, n);
         const LinkName lnk { name, ctx->get_typename() };
         TypeElaboration tt(ctx->get_typename(), ctx->get_typename());
         return ctx->build_formal(0, 0, tt, lnk);
      }

      static const Formal*
      gen_keyvar_formal(Elaborator& ctx, const char* n) {
         auto name = make_identifier(&ctx, n);
         const LinkName lnk { name, ctx.get_key() };
         TypeElaboration kt(ctx.get_typename(), ctx.get_key());
         return ctx.build_formal(0, 0, kt, lnk);
      }

      static const Type*
      to_type_var(Elaborator* ctx, const Formal* f) {
         const Read* r = ctx->build_read({ ctx->get_typename(), f });
         return ctx->make_type_expression({ ctx->get_typename() , r });
      }

      static const ArrowType*
      make_arrow_type(Elaborator* ctx, const Type* target, const Type* s) {
         TypeElaboration target_e = { ctx->get_typename(), target };
         TypeElaboration s_e = { ctx->get_typename(), s };
         InputTypes source_e(1, s_e);
         return ctx->make_arrow_type(target_e, source_e);
      }

      static const ArrowType*
      make_arrow_type(Elaborator* ctx, const Type* target, const Type* s0,
                      const Type* s1)
      {
         TypeElaboration target_e = { ctx->get_typename(), target };
         TypeElaboration s0_e = { ctx->get_typename(), s0 };
         TypeElaboration s1_e = { ctx->get_typename(), s1 };
         InputTypes source_e(2);
         source_e[0] = s0_e;
         source_e[1] = s1_e;
         return ctx->make_arrow_type(target_e, source_e);
      }

      static const ArrowType*
      make_arrow_type(Elaborator* ctx, const Type* target, const Type* s0,
                      const Type* s1, const Type* s2)
      {
         TypeElaboration target_e = { ctx->get_typename(), target };
         TypeElaboration s0_e = { ctx->get_typename(), s0 };
         TypeElaboration s1_e = { ctx->get_typename(), s1 };
         TypeElaboration s2_e = { ctx->get_typename(), s2 };
         InputTypes source_e(3);
         source_e[0] = s0_e;
         source_e[1] = s1_e;
         source_e[2] = s2_e;
         return ctx->make_arrow_type(target_e, source_e);
      }

      // -- Array instrinsics --

      static Data::Value
      intrinsic_mk_array(Data::Abstract env, Data::Value v) {
         auto ctx = static_cast<Evaluator*>(env);
         intmax_t size = v;
         if (size < 0)
            internal_error("Trying to reserve an array of negative length");
         Data::Array vec;
         if (size > 0)
            vec = Data::Array(size, Data::Value(nullptr));
         auto val = Data::Value(ctx->intern_array(vec));
         return Data::Abstract(val);
      }

      void define_mk_array(Elaborator* ctx) {
         auto n = make_identifier(ctx, "mk_array");
         auto x_formal = gen_tvar_formal(ctx, "X");
         auto x_t = to_type_var(ctx, x_formal);
         TypeElaboration array_t {
            ctx->get_typename(),
            ctx->make_array_type({ctx->get_typename(), x_t })
         };
         auto abs_t = make_arrow_type(ctx, array_t, ctx->get_int());
         const Function* func =
            ctx->build_builtin(n, abs_t, no_params_un<intrinsic_mk_array>, { });
         auto e = abstract_builtin_over(ctx, Formals(x_formal), abs_t, func);
         ctx->global_scope()->define(n, e.type(), e.code());
      }

      // Size of an array
      static Data::Value
      intrinsic_size_array(Data::Abstract, Data::Value v) {
         Data::Array& arr = **static_cast<Data::Array**>(Data::Location(v));
         intmax_t size = arr.size();
         if (size < 0)
            internal_error("found an array of negative size");
         return size;
      }

      void
      define_array_size(Elaborator* ctx) {
         auto n = make_identifier(ctx, "size");
         auto x_formal = gen_tvar_formal(ctx, "X");
         auto x_t = to_type_var(ctx, x_formal);
         TypeElaboration array_t {
            ctx->get_typename(),
            ctx->make_array_type({ctx->get_typename(), x_t })
         };
         auto c_array_x_t = ctx->make_readonly_type({ ctx->get_typename(), 
                                                      array_t });
         auto rc_array_x_t = ctx->make_reference_type({ ctx->get_typename(),
                                                        c_array_x_t });
         // Make it const ref.
         auto abs_t = make_arrow_type(ctx, ctx->get_int(), rc_array_x_t);
         const Function* func =
            ctx->build_builtin(n, abs_t, no_params_un<intrinsic_size_array>, { });
         auto e = abstract_builtin_over(ctx, Formals(x_formal), abs_t, func);
         ctx->global_scope()->define(n, e.type(), e.code());
      }

      static Data::Value
      intrinsic_array_at(Data::Abstract, const Arguments&, Data::Value ref_v,
                         Data::Value n_v)
      {
         Data::Array& v = *static_cast<Data::Array*>(Data::Location(ref_v));
         intmax_t n = n_v;
         if (n < 0)
            evaluation_error("array error: out of bounds");
         std::size_t m(n);
         if (v.size() <= m)
            evaluation_error("array error: out of bounds");
         return Data::Value(v[m]);
      }

      void
      define_array_get(Elaborator* ctx) {
         auto n = make_identifier(ctx, "get");
         auto x_formal = gen_tvar_formal(ctx, "X");
         auto x_t = to_type_var(ctx, x_formal);
         auto array_x_t = ctx->make_array_type({ ctx->get_typename(), x_t });
         auto abs_t = make_arrow_type(ctx, x_t, array_x_t, ctx->get_int());
         const Function* func = ctx->build_builtin(n, abs_t,intrinsic_array_at, { });
         auto e = abstract_builtin_over(ctx, Formals(x_formal), abs_t, func);
         ctx->global_scope()->define(n, e.type(), e.code());
      }

      static Data::Value
      intrinsic_array_set(Data::Abstract, Data::Value a1, Data::Value a2,
                          Data::Value a3)
      {
         Data::Array& v = **static_cast<Data::Array**>(a1);
         intmax_t n = a2;
         if (n < 0)
            evaluation_error("array error: out of bounds");
         std::size_t m(n);
         if (v.size() <= m)
            evaluation_error("array error: out of bounds");
         v[m] = a3;
         return Data::Abstract(nullptr);
      }

      void
      define_array_set(Elaborator* ctx) {
         auto n = make_identifier(ctx, "set");
         auto x_formal = gen_tvar_formal(ctx, "X");
         auto x_t = to_type_var(ctx, x_formal);
         auto array_x_t = ctx->make_array_type({ ctx->get_typename(), x_t });
         auto r_array_x_t = ctx->make_reference_type({ ctx->get_typename(), array_x_t });
         auto abs_t = make_arrow_type(ctx, ctx->get_void(),
            r_array_x_t, ctx->get_int(), x_t);
         const Function* func =
            ctx->build_builtin(n, abs_t, no_params_3<intrinsic_array_set>, { });
         auto e = abstract_builtin_over(ctx, Formals(x_formal), abs_t, func);
         ctx->global_scope()->define(n, e.type(), e.code());
      }

      const Type*
      make_fixed_array_type(Elaborator* ctx, const Formal* x, const Formal* n) {
         TypeElaboration x_t { ctx->get_typename(), to_type_var(ctx, x) };
         return ctx->make_fixed_array_type(x_t, read_formal(ctx, n));
      }

      const ArrowType*
      make_to_fixed_array_type(Elaborator* ctx, const Formal* x,
                               const Formal* n)
      {
         TypeElaboration arr_t {
            ctx->get_typename(),
            make_fixed_array_type(ctx, x, n) };
         return ctx->make_arrow_type(arr_t, { });
      }

      void define_mk_fixed_array(Elaborator* ctx) {
         auto name = make_identifier(ctx, "mk_fixed_array");
         auto x = gen_tvar_formal(ctx, "X");
         auto n = gen_intvar_formal(ctx, "n");
         Formals formals(2); formals[0] = x; formals[1] = n;
         auto to_fixed_array_t = make_to_fixed_array_type(ctx, x, n);
         const Function* func =
            ctx->build_builtin(name, to_fixed_array_t, no_params_un<intrinsic_mk_array>, { });
         auto call = ctx->build_call({ func->type(), func },
                                     { read_formal(ctx, n) });
         LinkName lnk { name, to_fixed_array_t };
         auto lam = ctx->build_lambda(lnk, { },
                                      { to_fixed_array_t->target(), call });
         auto e = abstract_builtin_over(ctx, formals, to_fixed_array_t, lam);
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      const ReferenceType*
      make_ref_const(Elaborator* ctx, const Type* t) {
         TypeElaboration te { ctx->get_typename(), t };
         auto c_t = ctx->make_readonly_type(te);
         te = { ctx->get_typename(), c_t };
         return ctx->make_reference_type(te);
      }

      void
      define_fixed_array_size(Elaborator* ctx) {
         auto name = make_identifier(ctx, "size");
         auto x = gen_tvar_formal(ctx, "X");
         auto n = gen_intvar_formal(ctx, "n");
         Formals formals(2); formals[0] = x; formals[1] = n;
         auto fixed_t = make_fixed_array_type(ctx, x, n);
         auto rc_fixed_t = make_ref_const(ctx, fixed_t);
         auto arrow_t = make_arrow_type(ctx, ctx->get_int(), rc_fixed_t);
         LinkName lnkf { make_identifier(ctx, "arr"), rc_fixed_t };
         auto f = ctx->build_formal(0, ctx->get_parameter_depth(),
                                    { ctx->get_typename(), rc_fixed_t }, lnkf);
         LinkName lnk { name, arrow_t };
         auto lam = ctx->build_lambda(lnk, Formals{ f }, read_formal(ctx, n));
         auto e = abstract_builtin_over(ctx, formals, arrow_t, lam);
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      void
      define_fixed_array_get(Elaborator* ctx) {
         auto name = make_identifier(ctx, "get");
         auto x = gen_tvar_formal(ctx, "X");
         auto n = gen_intvar_formal(ctx, "n");
         Formals formals(2); formals[0] = x; formals[1] = n;
         auto fixed_t = make_fixed_array_type(ctx, x, n);
         auto x_t = to_type_var(ctx, x);
         auto arrow_t = make_arrow_type(ctx, x_t, fixed_t, ctx->get_int());
         const Function* func = ctx->build_builtin(name, arrow_t,
                                                   intrinsic_array_at, { });
         auto e = abstract_builtin_over(ctx, formals, arrow_t, func);
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      void
      define_fixed_array_set(Elaborator* ctx) {
         auto name = make_identifier(ctx, "set");
         auto x = gen_tvar_formal(ctx, "X");
         auto n = gen_intvar_formal(ctx, "n");
         Formals formals(2); formals[0] = x; formals[1] = n;
         auto fixed_t = make_fixed_array_type(ctx, x, n);
         auto r_fixed_t = ctx->make_reference_type({ ctx->get_typename(),
                                                     fixed_t });
         auto x_t = to_type_var(ctx, x);
         auto arrow_t = make_arrow_type(ctx, ctx->get_void(),
                                        r_fixed_t, ctx->get_int(), x_t);
         const Function* func =
            ctx->build_builtin(name, arrow_t, no_params_3<intrinsic_array_set>,
                               { });
         auto e = abstract_builtin_over(ctx, formals, arrow_t, func);
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      void
      copy_simple_to_bytes_char(Evaluator* ctx, Data::Value v,
                                std::back_insert_iterator<Data::Array> i)
      {
         *i = ctx->build_char(v, ctx->get_char());
         ++i;
      }

      void
      copy_simple_to_bytes_int(Evaluator* ctx, Data::Value v,
                               std::back_insert_iterator<Data::Array> i)
      {
         constexpr std::size_t n = sizeof(Data::Mode::Int);
         for (auto j = (char*)(&v); j != (char*)(&v) + n; ++i, ++j)
            *i = ctx->build_char(Character(*j), ctx->get_char());
      }

      inline std::size_t
      get_uint_byte_width(const UintType& t) {
         intmax_t width = is<Int>(t.width().code())->rep();
         if (width < 0)
            internal_error("found a negative width `uint`");
         return std::size_t(width/8);
      }

      void
      copy_uint_to_bytes(Evaluator* ctx, Data::Value v, std::size_t byte_width,
                         std::back_insert_iterator<Data::Array> i)
      {
         constexpr uintmax_t n = sizeof(uintmax_t);
         // FIXME: Need padding until we support gmp
         for (std::size_t j = sizeof(uintmax_t); j < byte_width; ++j, ++j)
            *i = ctx->build_char(Character(0), ctx->get_char());
         std::size_t delta = n > byte_width ? n - byte_width : 0;
         for (auto j = (char*)(&v) + delta; j != (char*)(&v) + n; ++j, ++i)
            *i = ctx->build_char(Character(*j), ctx->get_char());
      }

      void
      copy_bytes(Evaluator*, const Type*, Data::Value,
                 std::back_insert_iterator<Data::Array>);

      void
      copy_record_to_bytes(Evaluator* ctx, const Record& rec_v,
                           std::back_insert_iterator<Data::Array> i)
      {
         for (auto binding: rec_v)
            copy_bytes(ctx, binding.second.type(), binding.second.value(), i);
      }

      void
      copy_bytes(Evaluator* ctx, const Type* t, Data::Value v,
                 std::back_insert_iterator<Data::Array> i)
      {
         if (t == ctx->get_int().code())
            copy_simple_to_bytes_int(ctx, v, i);
         else if (t == ctx->get_char().code())
            copy_simple_to_bytes_char(ctx, v, i);
         else if (auto uint_t = is<UintType>(t))
            copy_uint_to_bytes(ctx, v, get_uint_byte_width(*uint_t), i);
         else if (is<RecordType>(t))
            copy_record_to_bytes(ctx, *static_cast<const Record*>(v), i);
         else
            internal_error("sorry: `to_bytes` got an unsupported type");
      }

      static Data::Value
      intrinsic_to_bytes(Data::Abstract env, const Arguments& args,
                         Data::Value v)
      {
         auto ctx = static_cast<Evaluator*>(env);
         auto t = is<Type>(args.front().code());
         if (args.size() != 1 or t == nullptr)
            internal_error("`to_bytes` is not properly defined");
         Data::Array arr;
         copy_bytes(ctx, t, v, std::back_inserter(arr));
         arr.shrink_to_fit();
         return Data::Abstract(ctx->intern_array(arr));
      }

      void
      define_to_bytes(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "to_bytes");
         auto x = gen_tvar_formal(&ctx, "X");
         auto arr_char_t = ctx.make_array_type(ctx.get_char());
         auto x_t = to_type_var(&ctx, x);
         auto arrow_t = make_arrow_type(&ctx, arr_char_t, x_t);
         Formals formals(x);
         Elaboration param(x->type(), x);
         const Function* func =
           ctx.build_builtin(name, arrow_t, intrinsic_to_bytes, { param });
         auto e = abstract_builtin_over(&ctx, Formals(x), arrow_t, func);
         ctx.global_scope()->define(name, e.type(), e.code());
      }

      void
      define_array_intrinsics(Elaborator* ctx) {
         define_mk_array(ctx);
         define_array_size(ctx);
         define_array_get(ctx);
         define_array_set(ctx);
         define_mk_fixed_array(ctx);
         define_fixed_array_size(ctx);
         define_fixed_array_get(ctx);
         define_fixed_array_set(ctx);
         define_to_bytes(*ctx);
      }

      // -- Uint operations --

      const ArrowType*
      uint_binary_op_type(Elaborator* ctx, const Formal* f) {
         auto uint_f = ctx->make_uint_type(read_formal(ctx, f));
         TypeElaboration t { ctx->get_typename(), uint_f };
         return ctx->make_arrow_type(t, { t, t });
      }

      const ArrowType*
      uint_binary_pred_type(Elaborator* ctx, const Formal* f) {
         auto uint_f = ctx->make_uint_type(read_formal(ctx, f));
         TypeElaboration t { ctx->get_typename(), uint_f };
         return ctx->make_arrow_type(ctx->get_bool(), { t, t });
      }

      template<template <typename> class Op>
      void
      define_uint_bin_op(Elaborator* ctx, const char* op_name) {
         auto name = make_operator(ctx, op_name);
         auto n = gen_intvar_formal(ctx, "n");
         auto arrow_t = uint_binary_op_type(ctx, n);
         TypeElaboration t { ctx->get_typename(), arrow_t };
         auto quant_t = ctx->make_quantified_type(Quantifier::forall,
                                                  Formals{ n }, t, { });
         const Function* func = ctx->build_builtin(name, arrow_t,
            no_params_2<builtin_binary<uintmax_t, Op<uintmax_t>>>, { });
         auto lam = ctx->build_lambda(func->link_name(), Formals{ n },
                                      { arrow_t, func });
         Elaboration e { quant_t, lam };
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      template<template <typename> class Op>
      void
      define_uint_bin_func(Elaborator* ctx, const char* op_name) {
         auto name = make_identifier(ctx, op_name);
         auto n = gen_intvar_formal(ctx, "n");
         auto arrow_t = uint_binary_op_type(ctx, n);
         TypeElaboration t { ctx->get_typename(), arrow_t };
         auto quant_t = ctx->make_quantified_type(Quantifier::forall,
                                                  Formals{ n }, t, { });
         const Function* func = ctx->build_builtin(name, arrow_t,
            no_params_2<builtin_binary<uintmax_t, Op<uintmax_t>>>, { });
         auto lam = ctx->build_lambda(func->link_name(), Formals{ n },
                                      { arrow_t, func });
         Elaboration e { quant_t, lam };
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      template<template <typename> class Op>
      void
      define_uint_bin_pred(Elaborator* ctx, const char* op_name) {
         auto name = make_operator(ctx, op_name);
         auto n = gen_intvar_formal(ctx, "n");
         auto arrow_t = uint_binary_pred_type(ctx, n);
         TypeElaboration t { ctx->get_typename(), arrow_t };
         auto quant_t = ctx->make_quantified_type(Quantifier::forall,
                                                  Formals{ n }, t, { });
         const Function* func = ctx->build_builtin(name, arrow_t,
            no_params_2<builtin_binary<uintmax_t, Op<uintmax_t>>>, { });
         auto lam = ctx->build_lambda(func->link_name(), Formals{ n },
                                      { arrow_t, func });
         Elaboration e { quant_t, lam };
         ctx->global_scope()->define(name, e.type(), e.code());
      }

      void
      define_uint_intrinsics (Elaborator* ctx) {
         define_uint_bin_op<std::plus>(ctx, "+");
         define_uint_bin_op<std::minus>(ctx, "-");
         define_uint_bin_func<std::bit_and>(ctx, "bit_and");
         define_uint_bin_func<std::bit_or>(ctx, "bit_or");
         define_uint_bin_pred<std::less>(ctx, "<");
         define_uint_bin_pred<std::less_equal>(ctx, "<=");
         define_uint_bin_pred<std::greater>(ctx, ">");
         define_uint_bin_pred<std::greater_equal>(ctx, ">=");
         define_uint_bin_pred<std::equal_to>(ctx, "==");
         define_uint_bin_pred<std::not_equal_to>(ctx, "!=");
      }

      // -- Record intrinsics --

      Data::Value
      intrinsic_dimension(Data::Abstract, Data::Value v) {
         auto rt = static_cast<const RecordType*>(Data::Location(v));
         return Data::Value(intmax_t(rt->components().size()));
      }

      Data::Value
      intrinsic_field_type(Data::Abstract, Data::Value rt, Data::Value idx) {
         auto rtype = static_cast<const RecordType*>(Data::Location(rt));
         auto e = static_cast<const Expression*>(idx);
         if (auto i_int = is<Int>(e)) {
            const std::size_t i = intmax_t(i_int->rep());
            if (i >= rtype->components().size())
               evaluation_error("index greater than arity of function");
            return Data::Value(rtype->components()[i]->type().code());
         } else {
            evaluation_error("type functions may only accept values");
            return Data::Abstract(nullptr);
         }
      }

      Data::Value
      intrinsic_has_field(Data::Abstract, Data::Value rt, Data::Value str,
                          Data::Value ftype)
      {
         auto& rec_t = *static_cast<const RecordType*>(Data::Location(rt));
         Symbol s = str;
         auto field_type = static_cast<const Type*>(Data::Location(ftype));
         for (auto tag_t: rec_t.components())
            if (s == tag_t->tag()->symbol())
               if (structural_equivalence(tag_t->type(), field_type))
                  return true;
         return Data::Value(false);
      }

      Elaboration
      make_Record_restriction(Elaborator* ctx, const Formal* x) {
         auto args = Arguments { read_formal(ctx, x) };
         auto func_cst = ctx->build_constraint(ctx->get_Record(), args);
         return { ctx->get_concept(), func_cst };
      }

      void
      define_dimension(Elaborator* ctx) {
         auto name = make_identifier(ctx, "Dimension");
         auto x = gen_tvar_formal(ctx, "X");
         auto res = make_Record_restriction(ctx, x);
         auto t = ctx->make_product_type(Formals{ x }, ctx->get_int(), res);
         auto func = ctx->build_builtin(name, t, intrinsic_dimension);
         ctx->global_scope()->define(name, t, func);
      }

      void
      define_field_type(Elaborator* ctx) {
         auto name = make_identifier(ctx, "FieldType");
         auto x = gen_tvar_formal(ctx, "X");
         auto n = gen_intvar_formal(ctx, "N");
         Formals fs(2); fs[0] = x; fs[1] = n;
         auto res = make_Record_restriction(ctx, x);
         auto t = ctx->make_product_type(fs, ctx->get_typename(), res);
         auto func = ctx->build_builtin(name, t, intrinsic_field_type);
         ctx->global_scope()->define(name, t, func);
      }

      void
      define_has_field(Elaborator* ctx) {
         auto name = make_identifier(ctx, "HasField");
         auto x = gen_tvar_formal(ctx, "X");
         auto s = gen_stringvar_formal(ctx, "S");
         auto t_var = gen_tvar_formal(ctx, "T");
         Formals fs(3); fs[0] = x; fs[1] = s; fs[2] = t_var;
         auto res = make_Record_restriction(ctx, x);
         auto t = ctx->make_product_type(fs, ctx->get_bool(), res);
         auto func = ctx->build_builtin(name, t, intrinsic_has_field);
         ctx->global_scope()->define(name, t, func);
      }

      void
      define_Record_intrinsics(Elaborator* ctx) {
         define_dimension(ctx);
         define_field_type(ctx);
         define_has_field(ctx);
      }

      // -- Function intrinsics --

      // Implement the `Arity' function
      Data::Value
      intrinsic_arity(Data::Abstract, Data::Value v) {
         auto ft = static_cast<const ArrowType*>(Data::Location(v));
         return Data::Value(intmax_t(ft->arity()));
      }

      // Implement the 'Codomain' type function
      Data::Value
      intrinsic_codomain(Data::Abstract, Data::Value v) {
         auto ft = static_cast<const ArrowType*>(Data::Location(v));
         return Data::Value(ft->target().code());
      }

      // Implement the `InputType' type function
      Data::Value
      intrinsic_input_type(Data::Abstract, Data::Value ft, Data::Value idx) {
         auto ftype = Data::to_arrow_type(ft);
         auto e = static_cast<const Expression*>(idx);
         if (auto i_int = is<Int>(e)) {
            const std::size_t i = intmax_t(i_int->rep());
            if (i >= ftype->arity())
               evaluation_error("index greater than arity of function");
            return Data::Value(ftype->argument(i).code());
         } else {
            evaluation_error("type functions may only accept values");
            return Data::Abstract(nullptr);
         }
      }

      Elaboration
      make_Function_restriction(Elaborator* ctx, const Formal* x) {
         Arguments args { read_formal(ctx, x) };
         auto func_cst = ctx->build_constraint(ctx->get_Function(), args);
         return { ctx->get_concept(), func_cst };
      }

      void
      define_arity(Elaborator* ctx) {
         auto x = gen_tvar_formal(ctx, "X");
         auto res = make_Function_restriction(ctx, x);
         auto name = make_identifier(ctx, "Arity");
         auto t = ctx->make_product_type(Formals{ x }, ctx->get_int(), res);
         auto func = ctx->build_builtin(name, t, intrinsic_arity);
         ctx->global_scope()->define(name, t, func);
      }

      void
      define_codomain(Elaborator* ctx) {
         auto name = make_identifier(ctx, "Codomain");
         auto x = gen_tvar_formal(ctx, "X");
         auto res = make_Function_restriction(ctx, x);
         auto t = ctx->make_product_type(Formals{ x },ctx->get_typename(),res);
         auto func = ctx->build_builtin(name, t, intrinsic_codomain);
         ctx->global_scope()->define(name, t, func);
      }

      void
      define_input_type(Elaborator* ctx) {
         auto name = make_identifier(ctx, "InputType");
         auto x = gen_tvar_formal(ctx, "X");
         auto n = gen_intvar_formal(ctx, "N");
         Formals fs(2); fs[0] = x; fs[1] = n;
         auto res = make_Function_restriction(ctx, x);
         auto t = ctx->make_product_type(fs, ctx->get_typename(), res);
         auto func = ctx->build_builtin(name, t, intrinsic_input_type);
         ctx->global_scope()->define(name, t, func);
      }

      void
      define_Function_intrinsics(Elaborator* ctx) {
         define_arity(ctx);
         define_codomain(ctx);
         define_input_type(ctx);
      }

      // -- SubsetKey intrinsics --

      intmax_t
      get_bit_width(intmax_t proto, intmax_t field) {
         if (proto == subset_key::internal_val
             and field == subset_key::in_port_val)
            return 32;
         else if (proto == subset_key::internal_val
                  and field == subset_key::in_phy_port_val)
            return 32;
         else if (proto == subset_key::internal_val
                  and field == subset_key::tunnel_id_val)
            return 32;
         else if (proto == subset_key::internal_val
                  and field == subset_key::metadata_val)
            return 48;
         else if (proto == subset_key::ethernet_val
                  and field == subset_key::src_val)
            return 48;
         else if (proto == subset_key::ethernet_val
                  and field == subset_key::dst_val)
            return 48;
         else if (proto == subset_key::ethernet_val
                  and field == subset_key::typ_val)
            return 16;
         else if (proto == subset_key::ipv4_val
                  and field == subset_key::dscp_val)
            return 6;
         else if (proto == subset_key::ipv4_val
                  and field == subset_key::ecn_val)
            return 2;
         else if (proto == subset_key::ipv4_val
                  and field == subset_key::ttl_val)
            return 8;
         else if (proto == subset_key::ipv4_val
                  and field == subset_key::proto_val)
            return 32;
         else if (proto == subset_key::ipv4_val
                  and field == subset_key::src_val)
            return 32;
         else if (proto == subset_key::ipv4_val
                  and field == subset_key::dst_val)
            return 32;
         else if (proto == subset_key::ipv6_val
                  and field == subset_key::flabel_val)
            return 20;
         else if (proto == subset_key::ipv6_val
                  and field == subset_key::src_val)
            return 128;
         else if (proto == subset_key::ipv6_val
                  and field == subset_key::dst_val)
            return 128;
         else if (proto == subset_key::ipv6_val
                  and field == subset_key::ttl_val)
            return 8;
         else if (proto == subset_key::tcp_val
                  and field == subset_key::src_val)
            return 16;
         else if (proto == subset_key::tcp_val
                  and field == subset_key::dst_val)
            return 16;
         else if (proto == subset_key::udp_val
                  and field == subset_key::src_val)
            return 16;
         else if (proto == subset_key::udp_val
                  and field == subset_key::dst_val)
            return 16;
         else if (proto == subset_key::sctp_val
                  and field == subset_key::src_val)
            return 16;
         else if (proto == subset_key::sctp_val
                  and field == subset_key::dst_val)
            return 16;
         evaluation_error("invalid protocol-field pair");
         return 0;
      }

      TypeElaboration
      make_uint_type(Elaborator* ctx, intmax_t n) {
         Elaboration e { ctx->get_int(), ctx->build_int(n, ctx->get_int()) };
         return { ctx->get_typename(), ctx->make_uint_type(e) };
      }

      std::string
      mk_name_protocol_field(std::pair<intmax_t,intmax_t> k) {
         std::string name(subset_key::protocolname_table[k.first]);
         name += "_";
         name += subset_key::fieldname_table[k.second];
         name += "_val";
         return name;
      }

      const RecordType*
      record_type_from_key(Elaborator* ctx, const SubsetKeyValue& key) {
         const std::size_t n = key.size();
         Sequence<TagType> ttypes(key.size());
         for (std::size_t i = 0; i != n; ++i) {
            auto name = make_identifier(ctx, mk_name_protocol_field(key[i]));
            auto t = make_uint_type(ctx,
               get_bit_width(key[i].first, key[i].second));
            ttypes[i] = ctx->make_tag_type(name, t);
         }
         auto r = ctx->make_record_type(ttypes);
         return r;
      }

      Data::Value
      intrinsic_match_set_type(Data::Abstract env, Data::Value k) {
        auto ctx = static_cast<Evaluator*>(env);
        auto e = static_cast<const Expression*>(k);
        if (auto key = is<Key>(e)) {
          return Data::Value(record_type_from_key(ctx, *key));
        } else {
          evaluation_error("type functions may only accept values");
          return Data::Abstract(nullptr);
        }
      }

      const Function*
      make_match_set_type(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "MatchSet");
         auto arrow_t = make_arrow_type(&ctx,ctx.get_typename(),ctx.get_key());
         return ctx.build_builtin(name, arrow_t, no_params_un<intrinsic_match_set_type>, { });
      }

      void
      define_match_set_type(Elaborator* ctx) {
         const Function* func = make_match_set_type(*ctx);
         ctx->global_scope()->define(func->name(), func->type(), func);
      }

      TypeElaboration
      ms_type(Elaborator& ctx, const Formal& f) {
         auto func_e = make_match_set_type(ctx);
         Arguments args(1);
         auto read_e = ctx.build_read({ctx.make_reference_type(f.type()), &f});
         args[0] = { f.type(), read_e };
         auto call_e = ctx.build_call({ func_e->type(), func_e }, args);
         return { ctx.get_typename(),
                  ctx.make_type_expression({ ctx.get_typename(), call_e }) };
      }

      intmax_t
      get_match_set_value(Elaborator& ctx, const Record& rec,
                          std::pair<intmax_t,intmax_t> classifier)
      {
         auto n = ctx.intern(mk_name_protocol_field(classifier));
         auto bindings = rec.lookup(n);
         if (bindings.size() != 1)
            evaluation_error("ambiguous record");
         return bindings.begin()->value().value();
      }

      inline bool
      is_eth_typ(std::pair<intmax_t,intmax_t> k) {
         return k.first == subset_key::ethernet_val
            and k.second == subset_key::typ_val;
      }

      void
      remember_classifier(Elaborator& ctx, std::vector<intmax_t>& delta,
                          std::pair<intmax_t,intmax_t> classifier,
                          const Record& rec)
      {
         if (is_eth_typ(classifier)) {
            intmax_t n = get_match_set_value(ctx, rec, classifier);
            if (n == 0x0800)        // IPv4
               delta.push_back(subset_key::ipv4_val);
            else if (n == 0x86dd)   // IPv6
               delta.push_back(subset_key::ipv4_val);
         }
      }

      static Data::Value
      intrinsic_match_set_safety(Data::Abstract env, const Arguments& args,
                                 Data::Value v, Data::Value)
      {
         auto ctx = static_cast<Evaluator*>(env);
         auto& rec = *static_cast<const Record*>(v);
         std::vector<intmax_t> delta =
            { subset_key::internal_val, subset_key::ethernet_val };
         if (args.size() != 1)
            internal_error("dude, someone messed up the definition of "
                           " `packet_safe`");
         auto key = is<Key>(args.front().code());
         if (key == nullptr)
            internal_error("couldn't reduce key of `packet_safe`");
         for (auto k: *key) {
            if (std::find(delta.begin(), delta.end(), k.first) == delta.end())
               return Data::Value(false);
            remember_classifier(*ctx, delta, k, rec);
         }
         return Data::Value(true);
      }

      void
      define_match_set_safety(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "packet_safe");
         auto k = gen_keyvar_formal(ctx, "k");
         auto M_f = gen_tvar_formal(&ctx, "M");
         auto I_f = gen_tvar_formal(&ctx, "I");
         auto M_t = to_type_var(&ctx, M_f);
         auto I_t = to_type_var(&ctx, I_f);
         auto arrow_t =
            make_arrow_type(&ctx, ctx.get_bool(), M_t, I_t);
         Arguments args(3);
            args[0] = { ctx.get_key(), k };
            args[1] = { ctx.get_typename(), M_t };
            args[2] = { ctx.get_typename(), I_t };
         const Function* func =
            ctx.build_builtin(name, arrow_t, intrinsic_match_set_safety, args);
         Formals fs(3); fs[0] = k; fs[1] = M_f; fs[2] = I_f;
         auto e = abstract_builtin_over(&ctx, fs, arrow_t, func);
         ctx.global_scope()->define(name, e.type(), e.code());
      }

      void
      define_SubsetKey_intrinsics(Elaborator* ctx) {
        define_match_set_type(ctx);
        define_match_set_safety(*ctx);
      }

      Data::Value
      intrinsic_nullary_no_op(Data::Abstract)
      { return nullptr; }

      Data::Value
      intrinsic_binary_no_op(Data::Abstract, const Arguments&, Data::Value,
                             Data::Value)
      { return nullptr; }

      Data::Value
      intrinsic_ternary_no_op(Data::Abstract, const Arguments&, Data::Value,
                              Data::Value, Data::Value)
      { return nullptr; }

      const ArrowType*
      make_tiny_add_flow_type(Elaborator& ctx) {
         auto target = ctx.get_void();
         auto s0 = ctx.get_tiny_client();
         auto s1 = ctx.get_int();
         auto s2 = ctx.get_flow_mod();
         return make_arrow_type(&ctx, target, s0, s1, s2);
      }

      void
      define_tiny_add_flow(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "tiny_add_flow");
         auto arrow_t = make_tiny_add_flow_type(ctx);
         const Function* func =
            ctx.build_builtin(name, arrow_t, intrinsic_ternary_no_op, { });
         ctx.global_scope()->define(name, arrow_t, func);
      }

      static TypeElaboration
      make_uint_type(Elaborator& ctx, int bit_width) {
         auto int_e = ctx.build_int(bit_width, ctx.get_int());
         return { ctx.get_typename(),
                  ctx.make_uint_type({ ctx.get_int(), int_e }) };
      }

      Elaboration
      mk_config_type(Elaborator& ctx) {
         Sequence<TagType> components;
         components.push_back(
            ctx.make_tag_type(make_identifier(&ctx, "command"),
                              make_uint_type(ctx, 8)));
         components.push_back(
            ctx.make_tag_type(make_identifier(&ctx, "hard_timeout"),
                              make_uint_type(ctx,16)));
         components.push_back(
            ctx.make_tag_type(make_identifier(&ctx, "buffer_id"),
                              make_uint_type(ctx,32)));
         return { ctx.get_typename(), ctx.make_record_type(components) };
      }

      static TypeElaboration
      make_array_type(Elaborator& ctx, TypeElaboration t) {
         TypeElaboration arr_t(ctx.get_typename(), ctx.make_array_type(t)); 
         return arr_t;
      }

      Elaboration
      mk_inst_type(Elaborator& ctx) {
         Sequence<TagType> components;
         components.push_back(
            ctx.make_tag_type(make_identifier(&ctx, "apply"),
                              make_array_type(ctx, ctx.get_char())));
         components.push_back(
            ctx.make_tag_type(make_identifier(&ctx, "goto_id"),
                              make_uint_type(ctx, 8)));
         return { ctx.get_typename(), ctx.make_record_type(components) };
      }

      Elaboration
      config_instruction_constraint(Elaborator& ctx, const Formal* C_f,
                                    const Formal* I_f)
      {
         // Constrain config
         Arguments cargs = { read_formal(&ctx, C_f), mk_config_type(ctx) };
         auto cfunc_cst = ctx.build_constraint(ctx.get_ProjectionOf(), cargs);
         Elaboration c_req(ctx.get_concept(), cfunc_cst);
         // Constrain Instructions
         Arguments iargs = { read_formal(&ctx, I_f), mk_inst_type(ctx) };
         auto ifunc_cst = ctx.build_constraint(ctx.get_ProjectionOf(), iargs);
         Elaboration i_req(ctx.get_concept(), ifunc_cst);
         // Conjunction.
         auto conjunct = ctx.build_logical(logical::Operation::conjunction,
                                           c_req, i_req);
         return { ctx.get_concept(), conjunct };
      }

      template <typename Func>
      Elaboration
      abstract_tiny_make_flow_mod(Elaborator& ctx, const Formal* M_f,
                                  const Formal* C_f, const Formal* I_f,
                                  const Type* t, const Func* func)
      {
         Formals fs(3); fs[0] = M_f; fs[1] = C_f; fs[2] = I_f;
         auto qt = ctx.make_quantified_type(Quantifier::forall, fs,
            TypeElaboration{ ctx.get_typename(), t },
            config_instruction_constraint(ctx, C_f, I_f));
         auto tlam = ctx.build_lambda(func->link_name(), fs, { t, func });
         return { qt, tlam };
      }

      void
      define_tiny_make_flow_mod(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "tiny_make_flow_mod");
         auto M_f = gen_tvar_formal(&ctx, "M");
         auto C_f = gen_tvar_formal(&ctx, "C");
         auto I_f = gen_tvar_formal(&ctx, "I");
         //auto array_t = ctx.make_array_type(ctx.get_char());
         auto M_t = to_type_var(&ctx, M_f);
         auto C_t = to_type_var(&ctx, C_f);
         auto I_t = to_type_var(&ctx, I_f);
         auto arrow_t = make_arrow_type(&ctx, ctx.get_flow_mod(),
                                        M_t, C_t, I_t);
         const Function* func =
            ctx.build_builtin(name, arrow_t, intrinsic_ternary_no_op, { });
         auto e = abstract_tiny_make_flow_mod(ctx, M_f, C_f, I_f, arrow_t, func);
         ctx.global_scope()->define(name, e.type(), e.code());
      }

      void
      define_tiny_acquire_client(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "tiny_acquire_client");
         auto arrow_t = ctx.make_arrow_type(ctx.get_tiny_client(), { });
         const Function* func =
            ctx.build_builtin(name, arrow_t, intrinsic_nullary_no_op, { });
         ctx.global_scope()->define(name, arrow_t, func);
      }

      const ArrowType*
      make_tiny_responder_type(Elaborator& ctx, const Formal* D_f,
                               const Formal* E_f)
      {
         auto D_t = to_type_var(&ctx, D_f);
         auto E_t = to_type_var(&ctx, E_f);
         auto respond_t = make_arrow_type(&ctx, ctx.get_void(), D_t, E_t);
         return make_arrow_type(&ctx, ctx.get_void(), D_t, respond_t);
      }

      void
      define_tiny_responder(Elaborator& ctx) {
         auto name = make_identifier(&ctx, "tiny_responder");
         auto D_f = gen_tvar_formal(&ctx, "D");
         auto E_f = gen_tvar_formal(&ctx, "E");
         auto arrow_t = make_tiny_responder_type(ctx, D_f, E_f);
         const Function* func =
            ctx.build_builtin(name, arrow_t, intrinsic_binary_no_op, { });
         Formals fs(2); fs[0] = D_f; fs[1] = E_f;
         auto e = abstract_builtin_over(&ctx, fs, arrow_t, func);
         ctx.global_scope()->define(name, e.type(), e.code());
      }

      void
      define_tiny_intrinsics(Elaborator& ctx) {
         define_tiny_add_flow(ctx);
         define_tiny_make_flow_mod(ctx);
         define_tiny_acquire_client(ctx);
         define_tiny_responder(ctx);
      }

   } // namespace [local]

   void
   Elaborator::define_intrinsics() {
      liz::define_intrinsics(this, unary_intrinsic_table);
      liz::define_intrinsics(this, binary_intrinsic_table);
      liz::define_uint_intrinsics(this);
      liz::define_array_intrinsics(this);
      liz::define_Function_intrinsics(this);
      liz::define_Record_intrinsics(this);
      liz::define_SubsetKey_intrinsics(this);
      liz::define_tiny_intrinsics(*this);
   }
}
