// Copyright (C) 2012-2013, Texas A&M University
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
//     - Neither the name of the Texas A&M University. nor the name Liz,
//       nor the names of its contributors may be used to endorse or promote 
//       products derived from this software without specific prior
//       written permission.
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
#include <cstring>
#include <cstdlib>
#include <limits.h>
#include <liz/utility>
#include "Expression.H"

namespace liz {

   namespace {
      constexpr std::size_t address_width = sizeof(Data::Location) * CHAR_BIT;
      constexpr std::size_t int_width = sizeof(int) * CHAR_BIT;
   }

   std::size_t minimum_bit_width(const Type* t) {
      struct MinBitWidthVisitor : Expression::Visitor {
         std::size_t width;
         MinBitWidthVisitor() : width(0) { }
         std::size_t width_of(const Sequence<TagType>& seq) {
            std::size_t cumulative_width = 0;
            for (auto i: seq)
               cumulative_width += minimum_bit_width(i->type().code());
            return cumulative_width;
         }
         std::size_t width_of(const vector<TypeElaboration>& ts) {
            std::size_t cumulative_width = 0;
            for (auto t: ts)
               cumulative_width += minimum_bit_width(t.code());
            return cumulative_width;
         }
         void visit(const Expression&) {
            internal_error("cannot copmute the width of an expression");
         }
         void visit(const Type&) {
            internal_error("computing width of unknown type");
         }
         void visit(const BasicType& t) { width = t.data_traits()->precision; }
         void visit(const GenerativeType& t) {
            width = minimum_bit_width(t.value().code());
         }
         void visit(const ReferenceType&) {
            width = address_width;
         }
         void visit(const ArrayType&) {
            width = address_width;
         }
         void visit(const FixedArrayType& t) {
            if (auto int_e = is<Int>(t.length().code()))
               width = int_e->rep() * minimum_bit_width(t.elem_type().code());
            else
               width = 0;
         }
         void visit(const UintType& t) {
            if (auto int_e = is<Int>(&t.width()))
               width = int_e->rep();
            else
               width = 0;
         }
         void visit(const RecordType& t) { width = width_of(t.components()); }
         void visit(const ReadonlyType& t) {
            width = minimum_bit_width(t.type().code());
         }
         void visit(const RestrictedType& t) {
            width = minimum_bit_width(t.type().code());
         }
         void visit(const TypeExpression&) { }
         void visit(const VariantType&) {
            // This has quite a 
            // std::size_t max_width = 0;
            // for (auto ctor: t.constructors()) {
            //    if (auto arrow_t = is<ArrowType>(ctor->type())) {
            //       std::size_t w = width_of(arrow_t->source());
            //       max_width = max_width < w ? w : max_width;
            //    }
            // }
            // Are these garbage collected?
            width = int_width + address_width;
         }
         void visit(const QuantifiedType&) { }
         void visit(const ArrowType&) {
            // Values of ArrowType are transformed into 
            width = address_width;
         }
         void visit(const ProductType&) {
            width = address_width;
         }
         void visit(const TagType& t) {
            width = minimum_bit_width(t.type().code());
         }
      };

      MinBitWidthVisitor v;
      t->accept(v);
      return v.width;
   }

   // ----------
   // -- Type --
   // ----------
   Formatter
   Type::formatter() const {
      return Formatter();
   }

   const Data::Property*
   Type::data_traits() const {
      return 0;
   }

   // -- BasicType --
   BasicType::BasicType(const Name* n, const Data::Property* d)
         : structure::unary<const Name*>(n), data_info(d)
   { }

   void
   BasicType::print_on(std::ostream& os) const {
      os << name()->symbol();
   }

   Formatter
   BasicType::formatter() const {
      return data_info == 0 ? Formatter() : data_info->formatter;
   }

   const Data::Property*
   BasicType::data_traits() const {
      return data_info;
   }

   // -- TagType --
   TagType::TagType(const Name* n, TypeElaboration t)
         : structure::binary<const Name*, TypeElaboration>(n, t)
   { }

   void
   TagType::print_on(std::ostream& os) const {
      os << tag()->symbol() << ": ";
      const Type* t = type();
      t->print_on(os);
   }

   const Data::Property*
   TagType::data_traits() const {
      const Type* t = type();
      return t->data_traits();
   }

   // -- GenerativeType --
   GenerativeType::GenerativeType(const Name* n, TypeElaboration t, const Scope* s)
         : structure::ternary<const Name*, TypeElaboration, const Scope*>(n, t, s)
   { }

   void
   GenerativeType::print_on(std::ostream& os) const {
      os << name()->symbol();
   }

   const Data::Property*
   GenerativeType::data_traits() const {
      return value().code()->data_traits();
   }

   Formatter
   GenerativeType::formatter() const {
      return value().code()->formatter();
   }

   // -- ReferenceType --

   ReferenceType::ReferenceType(TypeElaboration t)
         : structure::unary<TypeElaboration>(t)
   { }

   void
   ReferenceType::print_on(std::ostream& os) const {
      os << "ref(";
      referee().code()->print_on(os);
      os << ')';
   }

   // Format a value of reference type.  In traditional C++, references
   // are "automatically" dereferenced; we stick to that tradition.
   // Note: on reflection, we should not when printing.
   static void
   format_reference(Data::Location, std::ostream& os, Data::Value v) {
      os << '@' << Data::Location(v);
   }
   
   Formatter
   ReferenceType::formatter() const {
      return Data::closure(&format_reference);
   }

   const Data::Property*
   ReferenceType::data_traits() const {
      return Data::property(Data::Mode::Pointer);
   }

   // -- ArrayType --

   ArrayType::ArrayType(TypeElaboration t)
         : structure::unary<TypeElaboration>(t)
   { }

   void
   ArrayType::print_on(std::ostream& os) const {
      os << "Array(";
      elem_type().code()->print_on(os);
      os << ")";
   }

   // In interpretted mode, we stay safe.
   static void
   print_array_obj(std::ostream& os, Data::Value v, const Type* t) {
      if (static_cast<Data::Abstract>(v) == nullptr)
         os << ":nil:";
      else
         os << Object(t, v);
   }

   static void
   format_array(Data::Location l, std::ostream& os, Data::Value v) {
      auto elem_type = static_cast<const Type*>(l);
      os << "[ ";
      Data::ArrayHandle arr = v;
      if (not arr->empty()) {
         print_array_obj(os, arr->at(0), elem_type);
         for (std::size_t i = 1; i != arr->size(); ++i) {
            os << ", ";
            print_array_obj(os, arr->at(i), elem_type);
         }
      }
      os << " ]";
   }

   Formatter
   ArrayType::formatter() const {
      return Data::closure(&format_array, elem_type().code());
   }

   const Data::Property*
   ArrayType::data_traits() const {
      return Data::property(Data::Mode::Array);
   }

   // -- FixedArrayType --

   FixedArrayType::FixedArrayType(TypeElaboration t, Elaboration e)
         : structure::binary<TypeElaboration, Elaboration>(t,e)
   { }

   void
   FixedArrayType::print_on(std::ostream& os) const {
      os << "FixedArray(";
      elem_type().code()->print_on(os);
      os << ",";
      os << pretty(length().code());
      os << ")";
   }

   Formatter
   FixedArrayType::formatter() const {
      return Data::closure(&format_array, elem_type().code());
   }

   const Data::Property*
   FixedArrayType::data_traits() const {
      return Data::property(Data::Mode::Array);
   }

   // -- UintType --

   UintType::UintType(const Type* t, const Value* v)
         : structure::binary<const Type*, const Value*>(t, v)
   { }

   void
   UintType::print_on(std::ostream& os) const {
      os << "uint(" << to_string(&width()) << ")";
   }

   static void
   format_uint(Data::Location, std::ostream& os, Data::Value v) {
      os << uintmax_t(v);
   }

   Formatter
   UintType::formatter() const {
      // Not sure how to do this?
      return Data::closure(&format_uint);
   }

   const Data::Property*
   UintType::data_traits() const {
      return Data::property(Data::Mode::Uint);
   }

   // -- ProductType --
   ProductType::ProductType(Target t, const Formals& s, Elaboration c)
         : function_space<Formals>(t, s, c)
   { }

   void
   ProductType::print_on(std::ostream& os) const {
      os << "forall(";
      for (std::size_t i = 0; i < arity(); ++i) {
         if (i != 0)
            os << ',' << ' ';
         if (auto name = argument(i)->name())
            os << name->symbol() << ": ";
         argument(i)->type().code()->print_on(os);
      }
      os << ')' << '.' << pretty(restriction().code()) << ',';
      target().code()->print_on(os);
   }

   // -- ArrowType --

   ArrowType::ArrowType(TypeElaboration t, const InputTypes& s)
         : function_space<InputTypes>(t, s)
   { }

   void
   ArrowType::print_on(std::ostream& os) const {
      os << '(';
      for (std::size_t i = 0; i < arity(); ++i) {
         if (i != 0)
            os << ", ";
         argument(i).code()->print_on(os);
      }
      os << ") -> ";
      target().code()->print_on(os);
   }

   // Format an object of function type onto an output stream.
   static void
   format_function(Data::Location, std::ostream& os, Data::Value v) {
      const Function* f = static_cast<const Function*>(Data::Location(v));
      if (auto lam = is<Lambda>(f)) {
         if (auto name = lam->name())
            os << name->symbol();
         else
            os << "[unnamed]";
         return;
      } else if (auto ctor = is<Constructor>(f)) {
         os << ctor->name()->symbol();
         return;
      } else
         os << "builtin ";
      os << f->name()->symbol();
   }

   Formatter
   ArrowType::formatter() const {
      return Data::closure(&format_function);
   }

   // -- RecordType --
   RecordType::RecordType(const Sequence<TagType>& s)
         : Sequence<TagType>(s)
   { }

   void
   RecordType::print_on(std::ostream& os) const {
      os << "record {";
      bool need_comma = false;
      for (auto f : components()) {
         if (need_comma)
            os << ',';
         os << ' ';
         f->print_on(os);
         need_comma = true;
      }
      os << ' ' << '}';
   }

   static void
   format_record(Data::Location, std::ostream& os, Data::Value v) {
      const Record* r = Data::to_record(v);
      os << '{';
      bool need_comma = false;
      for (auto& f : *r) {
         if (need_comma)
            os << ',';
         os << ' ';
         os << f.name() << " = " << f.value();
         need_comma = true;
      }
      os << ' ' << '}';
   }

   Formatter
   RecordType::formatter() const {
      return Data::closure(&format_record);
   }

   // -- ReadonlyType --

   ReadonlyType::ReadonlyType(TypeElaboration t)
         : structure::unary<TypeElaboration>(t)
   { }

   void
   ReadonlyType::print_on(std::ostream& os) const {
      os << "const ";
      type().code()->print_on(os);
   }

   static void
   format_readonly(Data::Location e, std::ostream& os, Data::Value v) {
      const ReadonlyType* t = is<ReadonlyType>(static_cast<const Type*>(e));
      os << make_object(t->type(), v);
   }

   Formatter
   ReadonlyType::formatter() const {
      return Data::closure(&format_readonly, this);
   }

   // -- RestrictedType --
   RestrictedType::RestrictedType(TypeElaboration t, Elaboration c)
         : structure::binary<TypeElaboration, Elaboration>(t, c)
   { }

   void RestrictedType::print_on(std::ostream& os) const {
      if (auto ctor = is<Constructor>(condition()))
         os << ctor->name()->symbol();
      else {
         type().code()->print_on(os);
         os << " | ";
         os << *condition().code();
      }
   }

   Formatter
   RestrictedType::formatter() const {
      return type().code()->formatter();
   }

   // -- TypeExpression --
   TypeExpression::TypeExpression(Elaboration e)
         : structure::unary<Elaboration>(e)
   { }

   void
   TypeExpression::print_on(std::ostream& os) const {
      struct V : Expression::Visitor {
         std::ostream& os;
         V(std::ostream& os) : os(os) { }

         void visit(const Expression& x) { os << x; }
         void visit(const Instance& x) { os << x; }
         void visit(const Postulate& x) { os << x.name()->symbol(); }
         void visit(const Constructor& x) { os << x.name()->symbol(); }
      };

      V v(os);
      expr().code()->accept(v);
   }

   // -- QuantifiedType --
   QuantifiedType::QuantifiedType(Quantifier q, const Formals& fs,
                                  TypeElaboration t, Elaboration c)
         : Base(q, fs, t), guard(c)
   { }

   const Formal* QuantifiedType::formal(int i) const {
      return formals().at(i);
   }

   static void
   format_formals(const Formals& fs, std::ostream& os) {
      os << '(';
      for (std::size_t i = 0; i < fs.size(); ++i) {
         if (i != 0)
            os << ", ";
         const Formal* f = fs[i];
         if (auto n = f->name())
            os << n->symbol() << " : ";
         f->type().code()->print_on(os);
      }
      os << ')';
   }

   void QuantifiedType::print_on(std::ostream& os) const {
      os << quantifier();
      format_formals(formals(), os);
      os << ". ";
      if (guard)
         os << pretty(guard.code()) << ", ";
      abstract_instance().code()->print_on(os);
   }

   VariantType::VariantType(const Sequence<Constructor>& cs)
         : structure::unary<Sequence<Constructor>>(cs)
   { }

   void
   VariantType::print_on(std::ostream& os) const {
      const auto n = constructors().size();
      os << "variant" << '{' << ' ';
      for (std::size_t i = 0; i < n; ++i) {
         if (i != 0)
            os << ", ";
         os << pretty(constructors()[i])
            << ' ' << ':' << ' '
            << pretty(constructors()[i]->type());
      }
      os << ' ' << '}';
   }

   std::ostream&
   operator<<(std::ostream& os, const Type& type) {
      type.print_on(os);
      return os;
   }

   // -- TypeFactory --
   const BasicType*
   TypeFactory::make_basic_type(const Name* n, const Data::Property* d) {
      return basics.make(n, d);
   }

   const TagType*
   TypeFactory::make_tag_type(const Name* n, TypeElaboration t) {
      return tags.make(n, t);
   }

   const RecordType*
   TypeFactory::make_record_type(const Sequence<TagType>& s) {
      return records.make(s);
   }

   const ReferenceType*
   TypeFactory::make_reference_type(TypeElaboration t) {
      // The reference type constructor is idempotent
      // FIXME: well, after some thought, we don't do that now.
      // For we currenly to define an 'access' of having a reference
      // type.  We don't have the notion of 'automatic' dereference yet
      // so, we naturally get to the notion of reference to a reference.
      return refs.make(t);
   }

   const ArrayType*
   TypeFactory::make_array_type(TypeElaboration t) {
      return arrays.make(t);
   }

   const FixedArrayType*
   TypeFactory::make_fixed_array_type(TypeElaboration t, Elaboration e) {
      return farrays.make(t,e);
   }

   const UintType*
   TypeFactory::make_uint_type(const Type& t, const Value& width) {
      return unsigneds.make(&t, &width);
   }

   const ProductType*
   TypeFactory::make_product_type(const Formals& s,
                                  TypeElaboration t, Elaboration c) {
      return products.make(t, s, c);
   }

   const ArrowType*
   TypeFactory::make_arrow_type(TypeElaboration target,
                                const InputTypes& source) {
      return funs.make(target, source);
   }
   
   const ReadonlyType*
   TypeFactory::make_readonly_type(TypeElaboration t) {
      // the 'const' type constructor is idempotent
      if (auto u = is<ReadonlyType>(t))
         return u;
      return consts.make(t);
   }

   const RestrictedType*
   TypeFactory::make_restricted_type(TypeElaboration t, Elaboration c) {
      return restricts.make(t, c);
   }

   const TypeExpression*
   TypeFactory::make_type_expression(Elaboration e) {
      if (e.code() == 0)
         abort();
      if (const TypeExpression* t = is<TypeExpression>(e.code()))
         return t;
      return exprs.make(e);
   }

   const QuantifiedType*
   TypeFactory::make_quantified_type(Quantifier q, const Formals& fs,
                                     TypeElaboration t, Elaboration c) {
      return quants.make(q, fs, t, c);
   }

   const VariantType*
   TypeFactory::make_variant_type(const Sequence<Constructor>& cs) {
      return variants.make(cs);
   }

   const GenerativeType*
   TypeFactory::make_generative_type(const Name* n, TypeElaboration t,
                                     const Scope* s) {
      return generatives.make(n, t, s);
   }

   namespace {
      struct TypePatternVisitor : Expression::Visitor {
         bool result;
         TypePatternVisitor() : result(false) { }

         void visit(const Expression& x) {
            system_error("don't know whether "
                         + quote(show_cxx_type(&x))
                         + " is a type pattern");
         }

         void visit(const BasicType&) { }

         void visit(const ReferenceType& x) {
            result = is_type_pattern(x.referee());
         }

         void visit(const ArrowType& x) {
            result = is_type_pattern(x.target());
            for (std::size_t i = 0; i < x.arity() and not result; ++i)
               result = is_type_pattern(x.argument(i));
         }

         void visit(const ReadonlyType& x) {
            result = is_type_pattern(x.type());
         }

         void visit(const TypeExpression& x) {
            if (const Formal* f = is<Formal>(x.expr().code()))
               result = is_wildcard(f);
         }
      };
   }

   bool is_type_pattern(const Type* t) {
      TypePatternVisitor v;
      if (t != 0)
         t->accept(v);
      return v.result;
   }
}
