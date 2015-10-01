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

#ifndef LIZ_EXPRESSION_INCLUDED
#define LIZ_EXPRESSION_INCLUDED

#include <list>
#include <vector>
#include <map>
#include <stack>
#include <liz/Character>
#include <liz/Ast>
#include "Environment.H"
#include "Data.H"

namespace liz {
   using std::vector;
   using Data::Formatter;
   struct Type;
   struct Expression;

   // ------------
   // -- Object --
   // ------------
   // Typed values; mostly used by the interpreter.
   struct Object : private std::pair<const Type*, Data::Value> {
      Object(const Type* t, Data::Value v)
            : std::pair<const Type*, Data::Value>(t, v) { }
      Object() : std::pair<const Type*, Data::Value>
                 (nullptr,Data::Value()) { }

      const Type* type() const { return first; }
      const Data::Value& value() const { return second; }
      Data::Value& value() { return second; }
   };

   // -- Convenient object constructor function function.
   inline Object
   make_object(const Type* t, Data::Value v) {
      return Object(t, v);
   }

   // -- Print an object onto the output stream
   std::ostream& operator<<(std::ostream&, const Object&);

   // --------------
   // -- Variable --
   // --------------
   using Variable = Binding<Symbol, Object>;

   // -----------
   // -- Store --
   // -----------
   // Representation of runtime store.
   using Store = Environment<Variable>;

   // --------------
   // -- StoreRef --
   // --------------
   using StoreRef = EnvRef<Store>;

   // -----------------
   // -- Elaboration --
   // -----------------
   // This datatype represents the result of elaborating an input
   // AST object into an expression to evaluate.
   struct Elaboration : std::pair<const Expression*, const Type*> {
      using Base = std::pair<const Expression*, const Type*>;

      Elaboration() : Base() { }
      Elaboration(const Type* t, const Expression* x) : Base(x, t) { }
      
      void code(const Expression* e) { first = e; }
      const Expression* code() const { return first; }
      const Type* type() const { return second; }
      // Convenient conversion for contextual use.
      explicit operator bool() const { return type() and code(); }
   };

   inline Elaboration
   make_elaboration(const Type* t, const Expression* x) {
      if (t == nullptr)
         abort();
      return Elaboration(t, x);
   }

   // -- Return true if the operand is a closed expression, e.g.
   // -- it does not have any free symbol or formals.
   bool is_closed(Elaboration);

   // -- Return true iff the Elaboration is a presburger expression.
   // -- The type argument is the integer type of arithmetic constraints.
   bool is_arithmetic_constraint(Elaboration, const Type*);

   template<typename T>
   inline const T*
   is(const Elaboration& x) {
      return is<T>(x.code());
   }

   // ----------------
   // -- Expression --
   // ----------------
   // -- Root class of the hierarchy of expressions in the
   // -- intermediate language representation
   struct Expression {
      struct Visitor;           // expression visitor base class
      virtual void accept(Visitor&) const = 0;
   };

   template<typename T>
   inline const T*
   is(const Expression* expr) {
      return dynamic_cast<const T*>(expr);
   }

   // -- Return a string representation of an expression.
   std::string show(const Expression*);

   inline std::string
   show_expr(const Expression* e) {
      return show(e);
   }

   // -- Factorize boilerplate for overriding the accept()
   // -- virtual function for all expression classes.
   template<typename T>
   struct expr_impl : Expression {
      void accept(Expression::Visitor&) const;
   };

   // -- print elaboration expression in prefix form
   void prefix_form(std::ostream&, const Expression*);

   // -- pretty expression --
   struct pretty {
      explicit pretty(const Expression* x) : rep(x) { }
      const Expression* expression() const { return rep; }
   private:
      const Expression* rep;
   };

   std::ostream& operator<<(std::ostream& os, pretty);

   // -- pretty print an expression on an output stream;
   std::ostream& operator<<(std::ostream& os, const Expression&);

   std::string to_string(const Expression*);

   // -----------
   // -- Value --
   // -----------
   // -- A value is a self-evaluating expression in the
   // -- intermediate language.  For convenience and ease
   // -- of interoperation with the evaluator, a value
   // -- in the elaborator has essentially the same
   // -- representation as a value in the evaluator.
   struct Value : Expression {
      // -- return the runtime representation of this value.
      virtual Data::Value get() const = 0;
   };

   template<typename T>
   struct value_impl : Value {
      void accept(Visitor&) const;
   };

   template<typename T>
   struct structured_value : value_impl<T> {
      Data::Value get() const { return this; }
   };

   // -- Quote --
   struct Quote : structured_value<Quote>, structure::unary<Elaboration> {
      explicit Quote(Elaboration);
      Elaboration body() const { return operand(); }
   };

   namespace Data {
      inline const Quote*
      to_quote(Value v) {
         return static_cast<const Quote*>(Abstract(v));
      }
   }

   // ----------
   // -- Type --
   // ----------
   struct Type : structured_value<Type> {
      // Print a string representation of this type on the output stream.  
      virtual void print_on(std::ostream&) const = 0;
      virtual Formatter formatter() const;
      // Machine-level properties of data of this type.
      virtual const Data::Property* data_traits() const;
   };

   // Return true if the argument designates a type pattern, i.e.
   // a type expression with the equivalent of source level `auto'
   // inside.
   bool is_type_pattern(const Type*);

   // Computes the minimum number of bits needed to represent a type. If there
   // are no type expressions in the type, then this is exact. Type expressions
   // are assumed to have a width of at least 0 bits. Alignment is ignored as
   // this computes the minimum possible width.
   std::size_t minimum_bit_width(const Type*);

   namespace Data {
      inline const Type*
      to_type(Value v) {
         return static_cast<const Type*>(Abstract(v));
      }
   }

   // -- Return a string representation of an expression.
   inline std::string show_type(const Type* t) {
      return show(static_cast<const Expression*>(t));
   }

   // -- return the C++ compiler's string for the of the dynamic
   // -- type of an expression.
   std::string show_cxx_type(const Expression*);

   // ---------------------
   // -- TypeElaboration --
   // ---------------------
   struct TypeElaboration : std::pair<const Type*, const Type*> {
      using Rep = std::pair<const Type*, const Type*>;
      TypeElaboration() : Rep(nullptr, nullptr) { }
      TypeElaboration(const Type* t, const Type* x)
            : Rep(t, x) { }
      const Type* type() const { return first; }
      const Type* code() const { return second; }
      operator const Type*() const { return code(); }
      operator Elaboration() const { return { type(), code() }; }
   };

   template<typename T>
   struct type_impl : Type {
      void accept(Visitor&) const;
   };

   template<typename T>
   inline const T*
   is(const TypeElaboration& x) {
      return is<T>(x.code());
   }

   // ----------
   // -- Name -- 
   // ----------
   // Abstract name.
   struct Name {
      struct Visitor;
      virtual Symbol symbol() const = 0;
      virtual void accept(Visitor&) const = 0;
   };

   std::string quote(const Name*);

   // ----------------
   // -- Identifier --
   // ----------------
   // A plain identifier name.
   struct Identifier : Name, structure::unary<Symbol> {
      explicit Identifier(Symbol);
      void accept(Visitor&) const;
      Symbol symbol() const;
   };

   // --------------
   // -- Operator --
   // --------------
   // Name of an operator.
   struct Operator : Name, structure::unary<Symbol>  {
      explicit Operator(Symbol);
      bool alphabetic() const;  // return true if base name is alphabetic.
      void accept(Visitor&) const;
      Symbol symbol() const;
   };

   // -------------
   // -- Literal --
   // -------------
   struct Literal : Name, structure::binary<Symbol, const Type*> {
      Literal(Symbol, const Type*);
      Symbol symbol() const;
      void accept(Visitor&) const;
      const Type* pristine_type() const { return second(); }
   };

   // -- Name::Visitor --
   struct Name::Visitor {
      virtual void visit(const Identifier&) = 0;
      virtual void visit(const Operator&) = 0;
      virtual void visit(const Literal&) = 0;
   };


   // -- TagType --
   struct TagType : type_impl<TagType>,
                    structure::binary<const Name*, TypeElaboration> {
      TagType(const Name*, TypeElaboration);
      const Name* tag() const { return first(); }
      TypeElaboration type() const { return second(); }
      void print_on(std::ostream&) const;
      const Data::Property* data_traits() const;
   };

   // -- Helper class for function space types.
   template<typename Args>
   struct function_space : structure::ternary<TypeElaboration, Args,
                                              Elaboration>
   {
      using Target = TypeElaboration;
      using Argument = typename Args::value_type;
      function_space(Target t, const Args& s)
         : structure::ternary<TypeElaboration, Args, Elaboration>(t, s, { }) { }
      function_space(Target t, const Args& s, Elaboration e)
         : structure::ternary<TypeElaboration, Args, Elaboration>(t, s, e) { }
      Target target() const { return this->first(); }
      const Args& source() const { return this->second(); }
      const Argument& argument(int i) const { return source().at(i); }
      std::size_t arity() const { return source().size(); }
   };

   // -- ArrowType
   using InputTypes = vector<TypeElaboration>;

   struct Formal;

   // -------------
   // -- Formals --
   // -------------
   struct Formals : Sequence<Formal> {
      Formals();
      explicit Formals(std::size_t);
      explicit Formals(const Formal*);
      operator InputTypes() const;
   };

   // -- Dependent product type
   struct ProductType : type_impl<ProductType>,
                        function_space<Formals> {
      ProductType(Target, const Formals&, Elaboration);
      Elaboration restriction() const { return third(); }
      void print_on(std::ostream&) const;
   };

   // -- Representation of a function type, special case of ProductType.
   struct ArrowType : type_impl<ArrowType>, function_space<InputTypes> {
      ArrowType(Target, const InputTypes&);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
   };

   InputTypes source(const Type*);

   namespace Data {
      inline const ArrowType*
      to_arrow_type(Value v) {
         return is<ArrowType>(to_type(v));
      }
   }

   using Arguments = vector<Elaboration>;

   // --------------
   // -- LinkName --
   // --------------
   struct LinkName : expr_impl<LinkName>,
                     structure::binary<const Name*, const Type*> {
      LinkName(const Name*, const Type*);
      const Name* name() const { return first(); }
      const Type* type() const { return second(); }
      Symbol symbol() const { return name()->symbol(); }
   };

   inline bool
   operator==(const LinkName& x, const LinkName& y) {
      return x.name() == y.name() and x.type() == y.type();
   }

   inline bool
   operator!=(const LinkName& x, const LinkName& y) {
      return not(x == y);
   }

   // -----------
   // -- Macro --
   // -----------
   struct Macro : structured_value<Macro>,
                  structure::binary<LinkName, Elaboration> {
      Macro(const LinkName&, Elaboration);
      const Name* name() const { return link_name().name(); }
      const Type* type() const { return link_name().type(); }
      const LinkName& link_name() const { return first(); }
      Elaboration replacement() const { return second(); }
   };

   // --------------
   // -- Function --
   // --------------
   struct Function : structured_value<Function> {
      using Niladic = Data::Value (*)(Data::Abstract);
      using Unary = Data::Value (*)(Data::Abstract, const Arguments&, Data::Value);
      using Binary = Data::Value (*)(Data::Abstract, const Arguments&,
                                     Data::Value, Data::Value);
      using Ternary = Data::Value (*)(Data::Abstract, const Arguments&,
                                      Data::Value, Data::Value, Data::Value);

      const Name* name() const { return link_name().name(); }
      const ArrowType* type() const;
      virtual const LinkName& link_name() const = 0;
      int arity() const { return type()->arity(); }
   };

   namespace Data {
      inline const Function*
      to_function(Value v) {
         return static_cast<const Function*>(Abstract(v));
      }
   }

   // --------------
   // -- Function --
   // --------------
   struct DependentFunction : structured_value<DependentFunction> {
      using Niladic = Data::Value (*)(Data::Abstract);
      using Unary = Data::Value (*)(Data::Abstract, Data::Value);
      using Binary = Data::Value (*)(Data::Abstract, Data::Value, Data::Value);
      using Ternary = Data::Value (*)(Data::Abstract, Data::Value, Data::Value,
                                      Data::Value);

      const Name* name() const { return link_name().name(); }
      const ProductType* type() const;
      virtual const LinkName& link_name() const = 0;
      int arity() const { return type()->arity(); }
   };

   namespace Data {
      inline const DependentFunction*
      to_dependent_function(Value v) {
         return static_cast<const DependentFunction*>(Abstract(v));
      }
   }

   // -------------------------
   // -- FunctionElaboration --
   // -------------------------
   struct FunctionElaboration : std::pair<const Type*, const Expression*> {
      using Rep = std::pair<const Type*, const Expression*>;
      FunctionElaboration() : Rep(nullptr, nullptr) { }
      FunctionElaboration(const ArrowType* t, const Expression* x)
            : Rep(t, x) { }
      FunctionElaboration(const ProductType* t, const Expression* x)
            : Rep(t, x) { }
      const Type* type() const { return first; }
      const ArrowType* arrow_type() const { return is<ArrowType>(first); }
      const ProductType* product_type() const { return is<ProductType>(first); }
      const Expression* code() const { return second; }
      void code(const Expression* e) { second = e; }
      operator Elaboration() const { return { type(), code() }; }
      // Convenience conversion for use in conditionals.
      explicit operator bool() const { return type(); }
   };

   struct DependentElaboration : std::pair<const ProductType*,
                                           const Expression*> {
      using Rep = std::pair<const ProductType*, const Expression*>;
      DependentElaboration() : Rep(nullptr, nullptr) { }
      DependentElaboration(const ProductType* t, const Expression* x)
            : Rep(t, x) { }
      const ProductType* type() const { return first; }
      const Expression* code() const { return second; }
      void code(const Expression* e) { second = e; }
      operator Elaboration() const { return { type(), code() }; }
      operator FunctionElaboration() const { return { type(), code() }; }
      // Convenience conversion for use in conditionals.
      explicit operator bool() const { return type(); }
   };

      struct SimpleFuncElaboration : std::pair<const ArrowType*,
                                               const Expression*> {
      using Rep = std::pair<const ArrowType*, const Expression*>;
      SimpleFuncElaboration() : Rep(nullptr, nullptr) { }
      SimpleFuncElaboration(const ArrowType* t, const Expression* x)
            : Rep(t, x) { }
      const ArrowType* type() const { return first; }
      const Expression* code() const { return second; }
      void code(const Expression* e) { second = e; }
      operator Elaboration() const { return { type(), code() }; }
      operator FunctionElaboration() const { return { type(), code() }; }
      // Convenience conversion for use in conditionals.
      explicit operator bool() const { return type(); }
   };

   inline TypeElaboration
   get_target_type(FunctionElaboration fun) {
      if (auto t = fun.arrow_type())
         return t->target();
      return is<ProductType>(fun.type())->target();
   }

   inline SimpleFuncElaboration
   make_elaboration(const Function* f) {
      return { f->type(), f };
   }

   inline DependentElaboration
   make_elaboration(const DependentFunction* f) {
      return { f->type(), f };
   }

   // -- Helper base class for thingies that behave syntactically
   // -- like call expressions.
   template<typename F>
   struct call_expr_base : structure::binary<F, Arguments> {
      call_expr_base(F f, const Arguments& a)
            : structure::binary<F, Arguments>(f, a) { }
      int argument_count() const { return this->second().size(); }
      const Arguments& arguments() const { return this->second(); }
      Elaboration argument(int i) const { return arguments()[i]; }
   };

   // -- simple_value
   template<typename S>
   struct simple_value : value_impl<simple_value<S>>,
                         structure::binary<Data::Value, const Type*> {
      simple_value(S s , const Type* t)
         : structure::binary<Data::Value, const Type*>(s, t) { }
      S rep() const { return first(); }
      operator S() const { return rep(); }
      Data::Value get() const { return first(); }
      const Type* type() const { return second(); }
   };

   // ----------
   // -- Bool --
   // ----------
   using Bool = simple_value<bool>;

   // ----------
   // -- Byte --
   // ----------
   using Byte = simple_value<byte>;

   // ----------
   // -- Char --
   // ----------
   using Char = simple_value<Character>;

   // ---------
   // -- Int --
   // ---------
   using Int = simple_value<intmax_t>;

   // ----------
   // -- Uint --
   // ----------
   using Uint = simple_value<uintmax_t>;

   // ------------
   // -- Double --
   // ------------
   using Double = simple_value<double>;

   // ------------
   // -- String --
   // ------------
   using String = simple_value<Symbol>;

   // -----------
   // -- Array --
   // -----------
   using Array = simple_value<Data::ArrayHandle>;

   // ------------
   // -- Record --
   // ------------
   struct Record : structured_value<Record>, Store {
      Record();
   };

   // ---------
   // -- Key --
   // ---------
   using SubsetKeyValue = std::vector<intmax_t>;

   bool key_equal(const SubsetKeyValue&, const SubsetKeyValue&);

   struct Key : structured_value<Key>, SubsetKeyValue {
      Key(const SubsetKeyValue&);
   };

   namespace Data {
      inline Record*
      to_record(Value v) {
         return static_cast<Record*>(Abstract(v));
      }

      inline Key*
      to_key(Value v) {
         return static_cast<Key*>(Abstract(v));
      }
   }

   namespace subset_key {

      constexpr intmax_t internal_val = 0;
      constexpr intmax_t ethernet_val = 1;
      constexpr intmax_t vlan_val = 2;
      constexpr intmax_t mpls_val = 3;
      constexpr intmax_t arp_val = 4;
      constexpr intmax_t ipv4_val = 5;
      constexpr intmax_t icmpv4_val = 6;
      constexpr intmax_t ipv6_val = 7;
      constexpr intmax_t icmpv6_val = 8;
      constexpr intmax_t tcp_val = 9;
      constexpr intmax_t sctp_val = 10;
      constexpr intmax_t udp_val = 11;

      constexpr intmax_t in_port_val = 0;
      constexpr intmax_t in_phy_port_val = 1;
      constexpr intmax_t tunnel_id_val = 2;
      constexpr intmax_t metadata_val = 3;
      constexpr intmax_t src_val = 4;
      constexpr intmax_t dst_val = 5;
      constexpr intmax_t typ_val = 6;
      constexpr intmax_t pcp_val = 7;
      constexpr intmax_t vid_val = 8;
      constexpr intmax_t label_val = 9;
      constexpr intmax_t tc_val = 10;
      constexpr intmax_t bos_val = 11;
      constexpr intmax_t opcode_val = 12;
      constexpr intmax_t sha_val = 13;
      constexpr intmax_t spa_val = 14;
      constexpr intmax_t tha_val = 15;
      constexpr intmax_t tpa_val = 16;
      constexpr intmax_t dscp_val = 17;
      constexpr intmax_t ecn_val = 18;
      constexpr intmax_t proto_val = 19;
      constexpr intmax_t code_val = 20;
      constexpr intmax_t flabel_val = 21;
      constexpr intmax_t ttl_val = 22;

      extern const char* protocolname_table[12];
      extern const char* fieldname_table[22];

      // Information regarding 
      struct packet_field_info {
         const char* name;
         const intmax_t bit_width;
      };

      // Rename this to packet fields.
      extern packet_field_info fields[19];

      // -- Number of packet fields supported.
      constexpr std::size_t field_count = length(fields);
   }

   // -----------------
   // -- Constructor --
   // -----------------
   struct Constructor : structured_value<Constructor>,
                        structure::binary<LinkName, Elaboration> {
      Constructor(const LinkName&, Elaboration);
      const LinkName& link_name() const { return first(); }
      const Name* name() const { return link_name().name(); }
      const Type* type() const;
      Elaboration implementation() const { return second(); }
      std::size_t arity() const;
   };

   // --------------
   // -- Instance --
   // --------------
   struct Instance : structured_value<Instance>,
                     structure::binary<const Constructor*, Sequence<Value>> {
      Instance(const Constructor*, const Sequence<Value>&);
      const Constructor* constructor() const { return first(); }
      const Sequence<Value> arguments() const { return second(); }
      const Value* argument(int i) const { return arguments()[i]; }
   };

   // ----------------
   // -- Assumption --
   // ----------------
   struct Assumption : expr_impl<Assumption>, structure::unary<Elaboration> {
      explicit Assumption(Elaboration);
      Elaboration expr() const { return operand(); }
   };

   struct Concept;
   // ---------------
   // -- BasicView --
   // ---------------
   struct BasicView : call_expr_base<const Constructor*> {
      using Base = call_expr_base<const Constructor*>;
      BasicView(const Constructor* c, const Arguments& a)
            : Base(c, a) { }
      const Constructor* constructor() const { return first(); }
   };

   // -----------------
   // -- Declaration --
   // -----------------
   using Declaration = Binding<const Name*, Elaboration>;

   // -----------
   // -- Scope --
   // -----------
   struct Scope : Environment<Declaration> {
      Scope();
      virtual const Expression* origin() const = 0;
      // define a named constant.  The difference between this and
      // `bind' is that a residual runtime representation may be
      // generated for the evaluator.  See Namespace.
      virtual Elaboration* define(const Name*, const Type*, const Expression*);
   };

   // --------------
   // -- ScopeRef --
   // --------------
   using ScopeRef = EnvRef<Scope>;

   // Declare a non-function symbol in a given binding countour.
   Declaration* declare(ScopeRef, const LinkName&);
   Declaration* declare(ScopeRef, const LinkName&, const Expression*);

   // -- Postulate --
   struct Postulate : structured_value<Postulate>,
                      structure::unary<LinkName> {
      explicit Postulate(const LinkName&);
      const LinkName& link_name() const { return operand(); }
      const Name* name() const { return link_name().name(); }
      const Type* type() const { return link_name().type(); }
   };

   // -------------------
   // -- ToplevelScope --
   // -------------------
   struct ToplevelScope : Scope {
      using Iterator = Sequence<Expression>::const_iterator;

      explicit ToplevelScope(const Scope*);
      ScopeRef region() { return ScopeRef(this); }
      const Scope* parent() const { return prnt; }
      const vector<Elaboration>& statements() const { return stmts; }
      Elaboration statement(int) const;
      void add_stmt(Elaboration);
      std::size_t stmt_count() const { return stmts.size(); }

   private:
      vector<Elaboration> stmts;
      const Scope* prnt;
   };
   
   // --------------
   // -- LoadUnit --
   // --------------
   struct LoadUnit : structured_value<LoadUnit>, ToplevelScope {
      explicit LoadUnit(const Path&);
      const Path& path() const { return pth; }
      const Expression* origin() const { return this; }
   private:
      Path pth;
   };

   namespace Data {
      inline LoadUnit*
      to_LoadUnit(Value v) {
         return static_cast<LoadUnit*>(Abstract(v));
      }
   }

   // ---------------
   // -- Namespace --
   // ---------------
   struct Namespace : structured_value<Namespace>, ToplevelScope,
                      structure::unary<const Name*> {
      Namespace(const Name*, const Scope*);
      Elaboration* define(const Name*, const Type*, const Expression*);
      const Name* name() const { return operand(); }
      const Expression* origin() const { return this; }
      Store* store() { return &s; }
   private:
      Store s;
   };

   // ------------
   // -- Domain --
   // ------------
   struct Domain : structured_value<Domain>, BasicView, ToplevelScope {
      vector<const BasicView*> subviews;
      vector<const Expression*> properties;
      Domain(const Constructor*, const Arguments&);
   };

   // -----------
   // -- Field --
   // -----------
   struct Field : expr_impl<Field>,
                  structure::binary<LinkName, const Expression*> {
      using impl = structure::binary<LinkName, const Expression*>;
      Field(const LinkName&, const Expression*);
      const LinkName& link_name() const { return first(); }
      const Name* name() const { return link_name().name(); }
      const Type* type() const { return link_name().type(); }
      const Expression* initializer() const { return second(); }
   };

   // ----------------------
   // -- DeclarativeScope --
   // ----------------------
   struct DeclarativeScope : Scope {
      DeclarativeScope();
      explicit DeclarativeScope(const Sequence<Field>&);
      ScopeRef region() { return ScopeRef(this); }
      const Sequence<Field>& fields() const { return flds; }
      void add_field(const Field*);
   private:
      Sequence<Field> flds;
   };

   // ------------
   // -- Formal --
   // ------------
   // Representation of a template parameter as a formal value.
   // Think of this as the abstract, formal, value of a parameter.
   struct Formal : Expression,
                   structure::binary<TypeElaboration, LinkName> {
      Formal(int, int, TypeElaboration, const LinkName&);
      void accept(Visitor&) const;
      const LinkName& link_name() const { return second(); }
      TypeElaboration type() const { return first(); }
      const Name* name() const { return link_name().name(); }
      Symbol symbol() const { return link_name().symbol(); }
      int position() const { return coord.first; }
      int level() const { return coord.second; }
   private:
      const std::pair<int, int> coord;
   };

   bool is_wildcard(const Formal*);

   inline Elaboration
   make_elaboration(const Formal* parm) {
      return make_elaboration(parm->type(), parm);
   }

   // -------------------
   // -- SymbolicValue --
   // -------------------
   struct SymbolicValue : structured_value<SymbolicValue>,
                          structure::unary<const Formal*> {
      SymbolicValue(const Formal&);
      const Formal& formal() const { return *operand(); };
      const Name* name() const { return operand()->name(); }
      const Type* type() const { return operand()->type(); }
   };

   // ---------------------
   // -- PatternInstance --
   // ---------------------
   struct PatternInstance : expr_impl<PatternInstance>,
                            structure::binary<const Constructor*, Formals> {
      PatternInstance(const Constructor*, const Formals&);
      const Constructor* constructor() const { return first(); }
      const Formals& formals() const { return second(); }
   };

   // ------------------
   // -- Substitution --
   // ------------------
   struct Substitution : std::map<const Formal*, Elaboration> {
      Substitution() : good(true), dep_vars(), dep_subst() { }
      Substitution(const Formals& fs)
            : good(true), dep_vars(fs), dep_subst()
         { }

      Elaboration operator()(const Formal*) const;
      Elaboration has(const Formal*) const;
      bool failed() const { return not good; }
      bool set_failed() { return good = false; }

      bool is_dep(const Formal* f) const;

      void subsume(const Substitution&);

      // convenience conversion function to be used in
      // conditional context to test for well-formedness.
      operator const void*() const { return failed() ? 0 : this; }

      bool good;                // true if not failure substitution.
      Formals dep_vars;
      std::map<const Formal*, Elaboration> dep_subst;
   };

   // ---------------
   // -- SubstExpr --
   // ---------------
   // An explicit substitution expression.  We use these to delay
   // instantiations.
   struct SubstExpr : expr_impl<SubstExpr>,
                      structure::binary<Substitution, Elaboration> {
      SubstExpr(const Substitution&, Elaboration);
      const Substitution& substitution() const { return first(); }
      Elaboration expression() const { return second(); }
   };

   struct Signature;

   // -- Alias --
   struct Alias : expr_impl<Alias>,
                  structure::binary<const Signature*, Elaboration> {
      explicit Alias(const Signature&, Elaboration);
      const Signature& alias() const { return *first(); }
      Elaboration equality() const { return second(); }
   };

   // -------------
   // -- Concept --
   // -------------
   // Representation of a `concept'.
   struct Concept : expr_impl<Concept> {
      using Formulae = vector<Elaboration>;
      using Signatures = Sequence<Signature>;
      Concept();
      const Formulae& formulae() const { return conds; }
      const Signatures& signatures() const { return sigs; }
      const Signatures& existentials() const { return exes; }
      void postulate(const Signature* s) { sigs.push_back(s); }
      Elaboration formula(int i) const { return conds[i]; }
      void require(Elaboration e) { conds.push_back(e); }
      void existential(const Signature* ex) { exes.push_back(ex); }
   private:
      vector<Elaboration> conds;
      Signatures sigs;
      Signatures exes;
   };

   // Representation of a basic type.
   struct BasicType : type_impl<BasicType>, structure::unary<const Name*> {
      BasicType(const Name*, const Data::Property*);
      void print_on(std::ostream&) const;
      const Data::Property* data_traits() const;
      Formatter formatter() const;
      const Name* name() const { return operand(); }
   private:
      const Data::Property* const data_info;
   };

   struct GenerativeType : type_impl<GenerativeType>,
                           structure::ternary<const Name*, TypeElaboration,
                                              const Scope*> {
      GenerativeType(const Name*, TypeElaboration, const Scope*);
      const Name* name() const { return first(); }
      TypeElaboration value() const { return second(); }
      const Scope* scope() const { return third(); }
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      const Data::Property* data_traits() const;
   };

   // Representation of a reference type.
   struct ReferenceType : type_impl<ReferenceType>,
                          structure::unary<TypeElaboration> {
      explicit ReferenceType(TypeElaboration);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      const Data::Property* data_traits() const;
      TypeElaboration referee() const { return operand(); }
   };

   struct ArrayType : type_impl<ArrayType>,
                      structure::unary<TypeElaboration> {
      explicit ArrayType(TypeElaboration);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      const Data::Property* data_traits() const;
      TypeElaboration elem_type() const { return operand(); }
   };

   struct FixedArrayType : type_impl<FixedArrayType>,
                           structure::binary<TypeElaboration, Elaboration> {
      explicit FixedArrayType(TypeElaboration, Elaboration);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      const Data::Property* data_traits() const;
      TypeElaboration elem_type() const { return first(); }
      Elaboration length() const { return second(); }
   };

   struct UintType : type_impl<UintType>,
                     structure::binary<const Type*, const Value*> {
      explicit UintType(const Type*, const Value*);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      const Data::Property* data_traits() const;
      const Value& width() const { return *second(); }
      const Type& type() const { return *first(); }
      Elaboration expr() const { return { first(), second() }; }
   };

   struct RecordType : type_impl<RecordType>, Sequence<TagType> {
      explicit RecordType(const Sequence<TagType>&);
      const Sequence<TagType>& components() const { return *this; }
      Formatter formatter() const;
      void print_on(std::ostream&) const;
   };

   // `const'-qualified types.
   struct ReadonlyType : type_impl<ReadonlyType>,
                         structure::unary<TypeElaboration> {
      explicit ReadonlyType(TypeElaboration);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      TypeElaboration type() const { return operand(); }
   };

   // restricted types.
   struct RestrictedType : type_impl<RestrictedType>,
                           structure::binary<TypeElaboration, Elaboration> {
      RestrictedType(TypeElaboration, Elaboration);
      void print_on(std::ostream&) const;
      Formatter formatter() const;
      TypeElaboration type() const { return first(); }
      Elaboration condition() const { return second(); }
   };

   // General expressions with type denotations.
   struct TypeExpression : type_impl<TypeExpression>,
                           structure::unary<Elaboration> {
      explicit TypeExpression(Elaboration);
      void print_on(std::ostream&) const;
      Elaboration expr() const { return operand(); }
   };

   struct VariantType : type_impl<VariantType>,
                        structure::unary<Sequence<Constructor>> {
      explicit VariantType(const Sequence<Constructor>&);
      const Sequence<Constructor>& constructors() const { return operand(); }
      void print_on(std::ostream&) const;
   };

   // ----------------
   // -- Quantifier --
   // ----------------
   enum class Quantifier {
      exists, forall
   };

   std::ostream& operator<<(std::ostream&, Quantifier);

   // --------------------
   // -- QuantifiedType --
   // --------------------
   struct QuantifiedType : type_impl<QuantifiedType>,
                           structure::ternary<Quantifier,
                                              Formals, TypeElaboration> {
      QuantifiedType(Quantifier, const Formals&, TypeElaboration, Elaboration);
      void print_on(std::ostream&) const;
      Quantifier quantifier() const { return first(); }
      const Formals& formals() const { return second(); }
      const Formal* formal(int) const;
      TypeElaboration abstract_instance() const { return third(); }
      Elaboration constraint() const { return guard; }
   private:
      using Base = structure::ternary<Quantifier, Formals, TypeElaboration>;
      Elaboration guard;
   };

   // Print an external representation of a type onto the output stream
   std::ostream& operator<<(std::ostream&, const Type&);

   // ---------------------------
   // -- QuantifiedElaboration --
   // ---------------------------
   struct QuantifiedElaboration
      : std::pair<const QuantifiedType*, const Expression*>
   {
      using Rep = std::pair<const QuantifiedType*, const Expression*>;
      QuantifiedElaboration() : Rep(nullptr, nullptr) { }
      QuantifiedElaboration(const QuantifiedType* t, const Expression* e)
         : Rep(t, e) { }
      const QuantifiedType* type() const { return first; }
      const Expression* code() const { return second; }
      operator Elaboration() const { return Elaboration{ type(), code() }; }
      // Convenience conversion for use in conditionals.
      explicit operator bool() const { return type(); }
   };

   // ------------------
   // -- TypeLessThan --
   // ------------------
   // This class implements a function object for comparing Liz
   // internal representations of types.
   struct TypeLessThan {
      template<typename S, typename T>
      bool operator()(const std::pair<S, T>& lhs, const std::pair<S, T>& rhs) const {
         return lhs < rhs;
      }

      template<typename T>
      bool operator()(const structure::unary<T>& lhs,
                      const structure::unary<T>& rhs) const {
         return std::less<T>()(lhs.operand(), rhs.operand());
      }

      template<typename S, typename T>
      bool operator()(const structure::binary<S, T>& lhs,
                      const structure::binary<S, T>& rhs) const {
         using scmp = std::less<S>;
         using tcmp = std::less<T>;
         return scmp()(lhs.first(), rhs.first())
            or (lhs.first() == rhs.first()
                and tcmp()(lhs.second(), rhs.second()));
      }

      template<typename S, typename T, typename U>
      bool operator()(const structure::ternary<S, T, U>& lhs,
                      const structure::ternary<S, T, U>& rhs) const {
         using scmp = std::less<S>;
         using tcmp = std::less<T>;
         using ucmp = std::less<U>;
         if (scmp()(lhs.first(), rhs.first()))
            return true;
         if (lhs.first() != rhs.first())
            return false;
         if (tcmp()(lhs.second(), rhs.second()))
            return true;
         if (lhs.second() != rhs.second())
            return false;
         return ucmp()(lhs.third(), rhs.third());
      }

      template<typename T>
      bool operator()(const vector<T>& x, const vector<T>& y) const {
         return std::less<vector<T>>()(x, y);
      }
   };

   // -----------------
   // -- TypeFactory --
   // -----------------
   struct TypeFactory {
      const BasicType*
      make_basic_type(const Name*, const Data::Property*);
      const TagType* make_tag_type(const Name*, TypeElaboration);
      const RecordType* make_record_type(const Sequence<TagType>&);
      const ReferenceType* make_reference_type(TypeElaboration);
      const ArrayType* make_array_type(TypeElaboration);
      const FixedArrayType* make_fixed_array_type(TypeElaboration, Elaboration);
      const UintType* make_uint_type(const Type&, const Value&);
      const ArrowType* make_arrow_type(TypeElaboration, const InputTypes&);
      const ReadonlyType* make_readonly_type(TypeElaboration);
      const RestrictedType* make_restricted_type(TypeElaboration, Elaboration);
      const TypeExpression* make_type_expression(Elaboration);
      const ProductType*
      make_product_type(const Formals&, TypeElaboration, Elaboration);
      const QuantifiedType*
      make_quantified_type(Quantifier, const Formals&, TypeElaboration,
                           Elaboration);
      const VariantType* make_variant_type(const Sequence<Constructor>&);
      const GenerativeType*
      make_generative_type(const Name*, TypeElaboration, const Scope*);

   private:
      template<typename T>
      struct factory : std::set<T, TypeLessThan> {
         template<typename... Args>
         const T* make(Args... args) {
            return &*this->insert(T(args...)).first;
         }
      };
      factory<BasicType> basics;
      factory<TagType> tags;
      factory<VariantType> variants;
      factory<RecordType> records;
      factory<ReferenceType> refs;
      factory<ArrayType> arrays;
      factory<FixedArrayType> farrays;
      factory<UintType> unsigneds;
      factory<ArrowType> funs;
      factory<ReadonlyType> consts;
      factory<ProductType> products;
      factory<QuantifiedType> quants;
      factory<TypeExpression> exprs;
      factory<RestrictedType> restricts;
      Factory<GenerativeType> generatives;
   };

   // -------------
    // -- Formula --
   // -------------
   struct Formula : structured_value<Formula>,
                    structure::ternary<Quantifier, Formals, Elaboration>{
      Formula(Quantifier, const Formals&, Elaboration);
      Quantifier quantifier() const { return first(); }
      const Formals& parameters() const { return second(); }
      const Formal* parameter(int) const;
      Elaboration body() const { return third(); }
   };
   
   // -------------
   // -- Builtin --
   // -------------
   template<typename Code>
   struct Builtin : Function, structure::ternary<LinkName, Code, Arguments> {
      Builtin(const LinkName& n, Code c, const Arguments& e)
            : structure::ternary<LinkName, Code, Arguments>(n, c, e) { }
      const LinkName& link_name() const { return this->first(); }
      Code code() const { return this->second(); }
      const Arguments& params() const { return this->third(); }
      void accept(Visitor&) const;
   };

   using NiladicBuiltinFunction = Builtin<Function::Niladic>;
   using UnaryBuiltinFunction = Builtin<Function::Unary>;
   using BinaryBuiltinFunction = Builtin<Function::Binary>;
   using TernaryBuiltinFunction = Builtin<Function::Ternary>;

   // ----------------------
   // -- DependentBuiltin --
   // ----------------------
   template<typename Code>
   struct DependentBuiltin : DependentFunction,
                             structure::ternary<LinkName, Code,
                                Sequence<Expression>> {
      DependentBuiltin(const LinkName& n, Code c, const Sequence<Expression>& e)
            : structure::ternary<LinkName, Code, Sequence<Expression>>(n, c, e)
               { }
      const LinkName& link_name() const { return this->first(); }
      Code code() const { return this->second(); }
      const ProductType* type() const {
         return is<ProductType>(link_name().type());
      }
      Sequence<Expression> params() const { return this->third(); }
      void accept(Visitor&) const;
   };

   using DependentNiladicBuiltinFunction =
      DependentBuiltin<DependentFunction::Niladic>;
   using DependentUnaryBuiltinFunction =
      DependentBuiltin<DependentFunction::Unary>;
   using DependentBinaryBuiltinFunction =
      DependentBuiltin<DependentFunction::Binary>;
   using DependentTernaryBuiltinFunction =
      DependentBuiltin<DependentFunction::Ternary>;

   // -----------
   // -- Block --
   // -----------
   struct Block : expr_impl<Block>, structure::unary<Arguments> {
      explicit Block(const Arguments&);
      int size() const { return statements().size(); }
      Elaboration statement(int i) const;
      const Arguments& statements() const { return operand(); }
   };

   const Block* to_block(const Expression*);

   // ------------
   // -- Lambda --
   // ------------
   struct Lambda : structured_value<Lambda>,
                   structure::ternary<LinkName, Formals, Elaboration> {
      Lambda(const LinkName&, const Formals&, Elaboration);
      const LinkName& link_name() const { return first(); }
      const Formals& formals() const { return second(); }
      const Name* name() const { return link_name().name(); }
      const Type* type() const { return link_name().type(); }
      int arity() const { return formals().size(); }
      const Formal* parameter(int) const;
      Elaboration body() const { return third(); }
      void accept(Visitor&) const;
   };

   // ----------------
   // -- Constraint --
   // ----------------
   struct Constraint : expr_impl<Constraint>, BasicView {
      Constraint(const Constructor*, const Arguments&);
      const Lambda* abstraction() const;
      const Concept* original_concept() const;
   };

   // ---------------
   // -- Signature --
   // ---------------
   // A function signature specified in a concept definition.
   // A signature is a requirement, so is not a function.
   // It is a placeholder to be filled in later by concrete functions.
   struct Signature : expr_impl<Signature>,
                      structure::binary<LinkName, std::size_t> {
      explicit Signature(const LinkName&, std::size_t);
      const LinkName& link_name() const { return first(); }
      std::size_t sig_id() const { return second(); }
      void accept(Visitor&) const;
   };

   // -- Association list of argument list.
   using AssocArguments = vector<std::pair<Symbol, Elaboration>>;
   
   // -----------------
   // -- Initializer --
   // -----------------
   struct Initializer : expr_impl<Initializer>, AssocArguments {
      explicit Initializer(const AssocArguments&);
   };

   // ---------------------
   // -- InitializerList --
   // ---------------------
   struct InitializerList : expr_impl<InitializerList>, Arguments {
      explicit InitializerList(const Arguments&);
   };

   // -- LetExpression ---
   struct LetExpression : expr_impl<LetExpression>,
                          structure::binary<vector<Elaboration>, Elaboration> {
      LetExpression(const vector<Elaboration>&, Elaboration);
      const vector<Elaboration>& locals() const { return first(); }
      Elaboration body() const { return second(); }
   };

   // ----------
   // -- Read --
   // ----------
   struct Read : expr_impl<Read>, structure::unary<Elaboration> {
      explicit Read( Elaboration);
      Elaboration address() const { return operand(); }
   };

   // -----------
   // -- Write --
   // -----------
   struct Write : expr_impl<Write>, structure::binary<Elaboration> {
      Write(Elaboration, Elaboration);
      Elaboration location() const { return first(); }
      Elaboration value() const { return second(); }
   };

   // -------------------
   // -- PatternClause --
   // -------------------
   struct PatternClause : private std::pair<Elaboration, Elaboration> {
      PatternClause();
      PatternClause(Elaboration, Elaboration);
      Elaboration pattern() const { return first; }
      Elaboration action() const { return second; }
   };

   using PatternClauses = vector<PatternClause>;

   // ------------------
   // -- PatternMatch --
   // ------------------
   struct PatternMatch : expr_impl<PatternMatch>,
                         structure::binary<Elaboration, PatternClauses>
   {
      PatternMatch(Elaboration, const PatternClauses&);
      Elaboration scrutinee() const { return first(); }
      const PatternClauses& clauses() const { return second(); }
   };

   // ------------
   // -- Clause --
   // ------------
   struct Clause : private std::pair<Elaboration, Elaboration> {
      Clause();
      Clause(Elaboration, Elaboration);
      Elaboration pattern() const { return first; }
      Elaboration action() const { return second; }
   };

   using Clauses = vector<Clause>;

   // -----------
   // -- Match --
   // -----------
   struct Match : expr_impl<Match>, structure::binary<Elaboration, Clauses> {
      Match(Elaboration, const Clauses&);
      Elaboration scrutinee() const { return first(); }
      const Clauses& clauses() const { return second(); }
   };

   // ------------
   // -- Offset --
   // ------------
   // Lvalue access to a structure field or array slot
   struct Offset : expr_impl<Offset>, structure::binary<Elaboration> {
      Offset(Elaboration, Elaboration);
      Elaboration address() const { return first(); }
      Elaboration delta() const { return second(); }
   };

   // ---------------
   // -- Component --
   // ---------------
   // Rvalue access to a scope object member.
   struct Component : expr_impl<Component>,
                      structure::binary<Elaboration, LinkName> {
      Component(Elaboration, const LinkName&);
      Elaboration whole() const { return first(); }
      const LinkName& link_name() const { return second(); }
      const Name* name() const { return link_name().name(); }
      const Type* type() const { return link_name().type(); }
      Symbol symbol() const { return link_name().symbol(); }
   };

   // ------------------
   // -- DotSelection --
   // ------------------
   struct DotSelection : expr_impl<DotSelection>,
                         structure::binary<Elaboration, LinkName> {
      DotSelection(Elaboration, const LinkName&);
      Elaboration whole() const { return first(); }
      const LinkName& link_name() const { return second(); }
      const Name* name() const { return link_name().name(); }
      const Type* type() const { return link_name().type(); }
      Symbol symbol() const { return link_name().symbol(); }
   };

   // ---------------------
   // -- UnaryExpression --
   // ---------------------
   struct UnaryExpression : Expression,
                            structure::binary<FunctionElaboration,
                                              Elaboration> {
      UnaryExpression(FunctionElaboration, Elaboration);
      FunctionElaboration function() const { return first(); }
      const Elaboration argument() const { return second(); }
   };

   template<typename T>
   struct unary_impl : UnaryExpression {
      using impl = unary_impl;
      unary_impl(FunctionElaboration f, Elaboration a) : UnaryExpression(f, a)
      { }

      void accept(Visitor&) const;
   };

   struct Negate : unary_impl<Negate> {
      Negate(FunctionElaboration, Elaboration);
   };

   struct Not : unary_impl<Not> {
      Not(FunctionElaboration, Elaboration);
   };

   struct Complement : unary_impl<Complement> {
      Complement(FunctionElaboration, Elaboration);
   };

   // ----------------------
   // -- BinaryExpression --
   // ----------------------
   struct BinaryExpression : Expression,
                             structure::ternary<FunctionElaboration,
                                                Elaboration> {
      using Base = structure::ternary<FunctionElaboration, Elaboration>;
      BinaryExpression(FunctionElaboration, Elaboration, Elaboration);
      FunctionElaboration function() const { return first(); }
      Elaboration lhs() const { return second(); }
      Elaboration rhs() const { return third(); }
   };

   template<typename T>
   struct binary_impl : BinaryExpression {
      using impl = binary_impl;
      binary_impl(FunctionElaboration f, Elaboration x, Elaboration y)
            : BinaryExpression(f, x, y)
      { }
      void accept(Visitor&) const;
   };

   struct Plus : binary_impl<Plus> {
      Plus(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Dash : binary_impl<Dash> {
      Dash(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Star : binary_impl<Star> {
      Star(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Slash : binary_impl<Slash> {
      Slash(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Div : binary_impl<Div> {
      Div(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Quo : binary_impl<Quo> {
      Quo(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Rem : binary_impl<Div> {
      Rem(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Mod : binary_impl<Div> {
      Mod(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Langle : binary_impl<Langle> {
      Langle(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Rangle : binary_impl<Rangle> {
      Rangle(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Langleq : binary_impl<Langleq> {
      Langleq(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Rangleq : binary_impl<Rangleq> {
      Rangleq(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Eqeq : binary_impl<Eqeq> {
      Eqeq(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Excleq : binary_impl<Excleq> {
      Excleq(FunctionElaboration, Elaboration, Elaboration);
   };

   struct And : binary_impl<And> {
      And(FunctionElaboration, Elaboration, Elaboration);
   };

   struct Or : binary_impl<Or> {
      Or(FunctionElaboration, Elaboration, Elaboration);
   };

   // -- logical::Operation --
   namespace logical {
      enum Operation {
         conjunction, disjunction, implication, equivalence
      };
   }

   inline std::string
   to_string(logical::Operation op) {
      switch(op) {
         case logical::conjunction: return { "and" };
         case logical::disjunction: return { "or" };
         case logical::implication: return { "=>" };
         case logical::equivalence: return { "<=>" };
      };
      return { };
   }

   // -------------------
   // -- BinaryLogical --
   // -------------------
   struct BinaryLogical : expr_impl<BinaryLogical>,
                          structure::ternary<logical::Operation, Elaboration> {
      BinaryLogical(logical::Operation, Elaboration, Elaboration);
      logical::Operation operation() const { return first(); }
      Elaboration lhs() const { return second(); }
      Elaboration rhs()  const { return third(); }
   };

   // --------------------
   // -- CallExpression --
   // --------------------
   struct CallExpression : expr_impl<CallExpression>,
                           call_expr_base<FunctionElaboration> {
      using Base = call_expr_base<FunctionElaboration>;
      CallExpression(FunctionElaboration, const Arguments&);
      FunctionElaboration function() const { return first(); }
   };

   // ---------------
   // -- Assertion --
   // ---------------
   struct Assertion : expr_impl<Assertion>,
                      structure::binary<Elaboration, FunctionElaboration> {
      Assertion(Elaboration, FunctionElaboration);
      Elaboration expression() const { return first(); }
      FunctionElaboration predicate() const { return second(); }
   };

   // ------------
   // -- Return --
   // ------------
   struct Return : expr_impl<Return>, structure::unary<Elaboration> {
      explicit Return(Elaboration);
      Elaboration expression() const { return operand(); }
   };

   // -----------
   // -- Throw --
   // -----------
   struct Throw : expr_impl<Throw>, structure::unary<Elaboration> {
      explicit Throw(Elaboration);
      Elaboration expression() const { return operand(); }
   };

   // ----------
   // -- Loop --
   // ----------
   // Indefinit loops
   struct Loop : expr_impl<Loop>, structure::unary<Elaboration> {
      explicit Loop(Elaboration);
      Elaboration body() const { return operand(); }
   };

   // -----------
   // -- Leave --
   // -----------
   struct Leave : expr_impl<Leave>, structure::unary<Elaboration> {
      explicit Leave(Elaboration);
      Elaboration expression() const { return operand(); }
   };

   // ------------------
   // -- IfExpression --
   // ------------------
   struct IfExpression : expr_impl<IfExpression>,
                           structure::ternary<Elaboration>  {
      IfExpression(Elaboration, Elaboration, Elaboration);
      Elaboration condition() const { return first(); }
      Elaboration consequence() const { return second(); }
      Elaboration alternative() const { return third(); }
   };

   // --------------------
   // -- BindExpression --
   // --------------------
   // Representation of a symbol definition.  This expression only
   // allocate storage for the symbol.  In particular, it does not run
   // any initialization routine.
   struct BindExpression : expr_impl<BindExpression>,
                           structure::binary<LinkName, Elaboration> {
      BindExpression(const LinkName&, Elaboration);
      const LinkName& link_name() const { return first(); }
      const Type* type() const { return link_name().type(); }
      const Name* name() const { return link_name().name(); }
      Elaboration initializer() const { return second(); }
   };

   // ------------
   // -- Import --
   // ------------
   struct Import : expr_impl<Import>, structure::unary<LoadUnit*> {
      explicit Import(LoadUnit*);
      LoadUnit* load_unit() const { return operand(); }
   };

   // -----------------------
   // -- Elaboration fiber --
   // -----------------------
   struct Fiber : vector<Elaboration> {
      Fiber();
      explicit Fiber(const Elaboration&);
      // Courtesy conversion for contextual use.
      explicit operator bool() const { return not empty(); }
      bool singleton() const { return size() == 1; }
      Fiber& append(const Fiber&);
   };

   // -------------------------
   // -- Expression::Visitor --
   // -------------------------
   struct Expression::Visitor {
      virtual void visit(const Expression&) = 0;

      virtual void visit(const LinkName&);
      virtual void visit(const Value&);
      virtual void visit(const Bool&);
      virtual void visit(const Char&);
      virtual void visit(const Int&);
      virtual void visit(const Uint&);
      virtual void visit(const Double&);
      virtual void visit(const String&);
      virtual void visit(const Record&);
      virtual void visit(const Key&);
      virtual void visit(const Postulate&);
      virtual void visit(const SymbolicValue&);
      virtual void visit(const Alias&);
      virtual void visit(const Instance&);
      virtual void visit(const PatternInstance&);
      virtual void visit(const Quote&);
      virtual void visit(const Field&);
      virtual void visit(const Domain&);
      virtual void visit(const Formula&);

      virtual void visit(const Macro&);
      virtual void visit(const Function&);
      virtual void visit(const NiladicBuiltinFunction&);
      virtual void visit(const UnaryBuiltinFunction&);
      virtual void visit(const BinaryBuiltinFunction&);
      virtual void visit(const TernaryBuiltinFunction&);
      virtual void visit(const DependentFunction&);
      virtual void visit(const DependentNiladicBuiltinFunction&);
      virtual void visit(const DependentUnaryBuiltinFunction&);
      virtual void visit(const DependentBinaryBuiltinFunction&);
      virtual void visit(const DependentTernaryBuiltinFunction&);
      virtual void visit(const Constructor&);
      virtual void visit(const Lambda&);
      virtual void visit(const Signature&);

      virtual void visit(const Type&);
      virtual void visit(const BasicType&);
      virtual void visit(const TagType&);
      virtual void visit(const Concept&);
      virtual void visit(const ReferenceType&);
      virtual void visit(const ArrayType&);
      virtual void visit(const FixedArrayType&);
      virtual void visit(const UintType&);
      virtual void visit(const ArrowType&);
      virtual void visit(const RecordType&);
      virtual void visit(const ReadonlyType&);
      virtual void visit(const RestrictedType&);
      virtual void visit(const TypeExpression&);
      virtual void visit(const ProductType&);
      virtual void visit(const QuantifiedType&);
      virtual void visit(const VariantType&);
      virtual void visit(const GenerativeType&);

      virtual void visit(const LoadUnit&);
      
      virtual void visit(const Formal&);
      virtual void visit(const LetExpression&);
      virtual void visit(const Read&);
      virtual void visit(const Write&);
      virtual void visit(const Offset&);
      virtual void visit(const Component&);
      virtual void visit(const DotSelection&);

      virtual void visit(const Negate&);
      virtual void visit(const Not&);
      virtual void visit(const Complement&);
      virtual void visit(const UnaryExpression&);

      virtual void visit(const Plus&);
      virtual void visit(const Dash&);
      virtual void visit(const Star&);
      virtual void visit(const Slash&);
      virtual void visit(const Div&);
      virtual void visit(const Quo&);
      virtual void visit(const Rem&);
      virtual void visit(const Mod&);
      virtual void visit(const Langle&);
      virtual void visit(const Rangle&);
      virtual void visit(const Langleq&);
      virtual void visit(const Rangleq&);
      virtual void visit(const Eqeq&);
      virtual void visit(const Excleq&);
      virtual void visit(const And&);
      virtual void visit(const Or&);
      virtual void visit(const BinaryExpression&);
      virtual void visit(const BinaryLogical&);

      virtual void visit(const CallExpression&);
      virtual void visit(const Initializer&);
      virtual void visit(const InitializerList&);
      virtual void visit(const Assertion&);
      virtual void visit(const Constraint&);
      virtual void visit(const Return&);
      virtual void visit(const Throw&);
      virtual void visit(const Loop&);
      virtual void visit(const Leave&);
      virtual void visit(const IfExpression&);
      virtual void visit(const Match&);
      virtual void visit(const PatternMatch&);
      virtual void visit(const BindExpression&);
      virtual void visit(const Block&);
      virtual void visit(const Assumption&);
      virtual void visit(const SubstExpr&);

      virtual void visit(const Namespace&);
      virtual void visit(const Import&);
   };

   inline const NiladicBuiltinFunction*
   is_niladic(const Expression* x) {
      return is<NiladicBuiltinFunction>(x);
   }
   
   inline const UnaryBuiltinFunction*
   is_unary(const Expression* x) {
      return is<UnaryBuiltinFunction>(x);
   }
   
   inline const BinaryBuiltinFunction*
   is_binary(const Expression* x) {
      return is<BinaryBuiltinFunction>(x);
   }

   inline const TernaryBuiltinFunction*
   is_ternary(const Expression* x) {
      return is<TernaryBuiltinFunction>(x);
   }

   inline const DependentNiladicBuiltinFunction*
   is_dep_niladic(const Expression* x) {
      return is<DependentNiladicBuiltinFunction>(x);
   }

   inline const DependentUnaryBuiltinFunction*
   is_dep_unary(const Expression* x) {
      return is<DependentUnaryBuiltinFunction>(x);
   }

   inline const DependentBinaryBuiltinFunction*
   is_dep_binary(const Expression* x) {
      return is<DependentBinaryBuiltinFunction>(x);
   }

   inline const DependentTernaryBuiltinFunction*
   is_dep_ternary(const Expression* x) {
      return is<DependentTernaryBuiltinFunction>(x);
   }

   const Type* to_type(const Expression*);
   const Constraint* to_constraint(const Expression*);
   const Function* to_function(const Expression*);
   const DependentFunction* to_dependent_function(const Expression*);

   inline TypeElaboration
   to_type(Elaboration expr) {
      return TypeElaboration(expr.type(), to_type(expr.code()));
   }

   template<typename T>
   void
   Builtin<T>::accept(Visitor& v) const {
      v.visit(*this);
   }

   template<typename T>
   void
   DependentBuiltin<T>::accept(Visitor& v) const {
      v.visit(*this);
   }

   struct ExpressionFactory : TypeFactory {
      ExpressionFactory();
      Symbol intern(const std::string&);
      Symbol intern(const char*);
      Symbol intern(const Token*);
      Data::ArrayHandle intern_array(const Data::Array&);
      const Bool* build_bool(bool, const Type*);
      const Char* build_char(Character, const Type*);
      const Int* build_int(int, const Type*);
      const Uint* build_uint(uintmax_t, const Type*);
      const Double* build_double(double, const Type*);
      const String* build_string(Symbol, const Type*);
      const Array* build_array(const Data::Array&, const Type*);
      const Instance*
      build_instance(const Constructor*, const Sequence<Value>&);
      const PatternInstance*
      build_pattern_instance(const Constructor*, const Formals&);
      const Quote* build_quote(Elaboration);
      Record* build_record();
      const Key* build_key(const SubsetKeyValue&);
      const Identifier* build_identifier(Symbol);
      const Identifier* build_identifier(const Token*);
      const Identifier* fresh_name();
      const Operator* build_operator(Symbol);
      const Operator* build_operator(const Token*);
      const Literal* build_literal(Symbol, const Type*);
      const LinkName* build_name(const Name*, const Type*);
      const Formal* build_formal(int, int, TypeElaboration, const LinkName&);
      const Field* build_field(const LinkName&, const Expression* = nullptr);
      const Formal* build_wildchar(TypeElaboration);
      const Initializer* build_initializer(const AssocArguments&);
      const InitializerList* build_initializer_list(const Arguments&);
      Concept* build_concept();
      const Read* build_read(Elaboration);
      const Write* build_write(Elaboration, Elaboration);
      const Block* build_block(const Arguments);

      const Macro* build_macro(const LinkName&, Elaboration);
      const NiladicBuiltinFunction*
      build_builtin(const Name*, const ArrowType*, Function::Niladic,
                    const Arguments&);
      const UnaryBuiltinFunction*
      build_builtin(const Name*, const ArrowType*, Function::Unary,
                    const Arguments&);
      const BinaryBuiltinFunction*
      build_builtin(const Name*, const ArrowType*, Function::Binary,
                    const Arguments&);
      const TernaryBuiltinFunction*
      build_builtin(const Name*, const ArrowType*, Function::Ternary,
                    const Arguments&);
      const DependentNiladicBuiltinFunction*
      build_builtin(const Name*, const ProductType*,
                    DependentFunction::Niladic);
      const DependentUnaryBuiltinFunction*
      build_builtin(const Name*, const ProductType*,
                    DependentFunction::Unary);
      const DependentBinaryBuiltinFunction*
      build_builtin(const Name*, const ProductType*,
                    DependentFunction::Binary);
      const DependentTernaryBuiltinFunction*
      build_builtin(const Name*, const ProductType*,
                    DependentFunction::Ternary);
      const Constructor* build_constructor(const LinkName&, Elaboration);
      const Postulate* build_postulate(const LinkName&);
      const SymbolicValue* build_symbolic_value(const Formal&);
      const Alias* build_alias(const Signature&, Elaboration);
      const Lambda*
      build_lambda(const LinkName&, const Formals&, Elaboration);
      const Signature* build_signature(const Name*, const Type*);
      const Signature* build_signature(const LinkName&);
      const Signature* build_signature(const LinkName&, std::size_t);
      const Formula*
      build_formula(Quantifier, const Formals&, Elaboration);
      const Assumption* build_assumption(Elaboration);

      const LetExpression* build_let(const vector<Elaboration>, Elaboration);
      const Offset* build_offset(Elaboration, Elaboration);
      const Component* build_component(Elaboration, const LinkName&);
      const DotSelection* build_dot(Elaboration, const LinkName&);

      const Negate* build_negate(FunctionElaboration, Elaboration);
      const Not* build_not(FunctionElaboration, Elaboration);
      const Complement* build_complement(FunctionElaboration, Elaboration);

      const Plus* build_plus(FunctionElaboration, Elaboration, Elaboration);
      const Dash* build_dash(FunctionElaboration, Elaboration, Elaboration);
      const Star* build_star(FunctionElaboration, Elaboration, Elaboration);
      const Slash* build_slash(FunctionElaboration, Elaboration, Elaboration);
      const Div* build_div(FunctionElaboration, Elaboration, Elaboration);
      const Quo* build_quo(FunctionElaboration, Elaboration, Elaboration);
      const Rem* build_rem(FunctionElaboration, Elaboration, Elaboration);
      const Mod* build_mod(FunctionElaboration, Elaboration, Elaboration);
      const Langle* build_langle(FunctionElaboration, Elaboration, Elaboration);
      const Rangle* build_rangle(FunctionElaboration, Elaboration, Elaboration);
      const Langleq* build_langleq(FunctionElaboration, Elaboration, Elaboration);
      const Rangleq* build_rangleq(FunctionElaboration, Elaboration, Elaboration);
      const Eqeq* build_eqeq(FunctionElaboration, Elaboration, Elaboration);
      const Excleq* build_excleq(FunctionElaboration, Elaboration, Elaboration);
      const And* build_and(FunctionElaboration, Elaboration, Elaboration);
      const Or* build_or(FunctionElaboration, Elaboration, Elaboration);

      const BinaryLogical*
      build_logical(logical::Operation, Elaboration, Elaboration);
      const CallExpression* build_call(FunctionElaboration, const Arguments&);
      const Assertion* build_assertion(Elaboration, FunctionElaboration);
      const Constraint* build_constraint(const Constructor*, const Arguments&);
      const Return* build_return(Elaboration);
      const Throw* build_throw(Elaboration);
      const Loop* build_loop(Elaboration);
      const Leave* build_leave(Elaboration);
      const IfExpression* build_if(Elaboration, Elaboration, Elaboration);
      const Match* build_match(Elaboration, const Clauses&);
      const PatternMatch*
      build_pattern_match(Elaboration, const PatternClauses&);
      const BindExpression* build_bind(const LinkName&, Elaboration);
      const BindExpression* build_bind(const Name*, const Type*, Elaboration);
      const SubstExpr* build_subst_expr(const Substitution&, Elaboration);
      Namespace* build_namespace(const Name*, const Scope*);
      const Import* build_import(LoadUnit*);
      LoadUnit* build_load_unit(const Path&);

      struct expression_lt : std::less<Symbol> {
         using std::less<Symbol>::operator();

         bool operator()(logical::Operation x, logical::Operation y) const {
            return x < y;
         }
         
         bool operator()(Quantifier x, Quantifier y) const {
            return x < y;
         }

         bool operator()(const std::string& x, const std::string& y) const {
            return x < y;
         }
         
         template<typename T>
         bool operator()(const T* lhs, const T* rhs) const {
            return std::less<const T*>()(lhs, rhs);
         }

         template<typename T>
         bool operator()(const simple_value<T>& lhs,
                         const simple_value<T>& rhs) const {
            return std::less<T>()(lhs,rhs);
         }

         template<typename T>
         bool operator()(const structure::unary<T>& lhs,
                         const structure::unary<T>& rhs) const {
            return (*this)(lhs.operand(), rhs.operand());
         }

         template<typename S, typename T>
         bool
         operator()(const structure::binary<S, T>& lhs,
                    const structure::binary<S, T>& rhs) const {
            if ((*this)(lhs.first(), rhs.first()))
               return true;
            if (lhs.first() != rhs.first())
               return false;
            return (*this)(lhs.second(), rhs.second());
         }

         template<typename S, typename T, typename U>
         bool
         operator()(const structure::ternary<S, T, U>& lhs,
                    const structure::ternary<S, T, U>& rhs) const {
            if ((*this)(lhs.first(), rhs.first()))
               return true;
            if (lhs.first() != rhs.first())
               return false;
            if ((*this)(lhs.second(), rhs.second()))
               return true;
            if (lhs.second() != rhs.second())
               return false;
            return (*this)(lhs.third(), rhs.third());
         }

         template<typename S, typename T>
         bool
         operator()(const std::pair<S, T>& lhs, const std::pair<S, T>& rhs) const {
            if ((*this)(lhs.first, rhs.first))
               return true;
            if (lhs.first != rhs.first)
               return false;
            return (*this)(lhs.second, rhs.second);
         }

         template<typename T>
         bool
         operator()(const vector<T>& lhs, const vector<T>& rhs) const {
            return lhs < rhs; 
         }

         bool operator()(const Function& lhs, const Function& rhs) const {
            return (*this)(lhs.link_name(), rhs.link_name());
         }

         bool operator()(const DependentFunction& lhs,
                         const DependentFunction& rhs) const
         {
            return (*this)(lhs.link_name(), rhs.link_name());
         }
      };

      template<typename S>
      struct factory : std::set<S, expression_lt> {
         template<typename... T>
         const S* make(const T&... t) {
            return &*this->insert(S(t...)).first;
         }
      };

   private:
      int gensym_count;
      int wildcard_count;
      std::set<std::string> syms;
      std::list<Data::Array> arrs;
      Factory<LoadUnit> modules;
      Factory<Namespace> scopes;
      factory<Bool> bools;
      factory<Char> chars;
      factory<Int> ints;
      factory<Uint> uints;
      factory<Double> doubles;
      factory<String> strings;
      factory<Array> arrays;
      factory<Instance> insts;
      factory<PatternInstance> pinsts;
      factory<Quote> quotes;
      Factory<Record> records;
      Factory<Key> keys;
      factory<Identifier> ids;
      factory<Operator> ops;
      factory<Literal> lits;
      factory<LinkName> lnks;
      Factory<Postulate> pos;
      Factory<SymbolicValue> symvals;
      Factory<Alias> alii;
      factory<Field> flds;
      Factory<Formal> formals;
      factory<NiladicBuiltinFunction> nfuns;
      factory<UnaryBuiltinFunction> ufuns;
      factory<BinaryBuiltinFunction> bfuns;
      factory<TernaryBuiltinFunction> tfuns;
      factory<DependentNiladicBuiltinFunction> depnfuns;
      factory<DependentUnaryBuiltinFunction> depufuns;
      factory<DependentBinaryBuiltinFunction> depbfuns;
      factory<DependentTernaryBuiltinFunction> deptfuns;
      Factory<Macro> macros;
      Factory<Constructor> ctors;
      Factory<Lambda> lambdas;
      factory<Initializer> inits;
      factory<InitializerList> init_lists;
      Factory<LetExpression> lets;
      factory<BindExpression> binds;
      factory<Read> rds;
      factory<Write> wrs;
      factory<Offset> offs;
      factory<Component> cmps;
      factory<DotSelection> dots;
      factory<Negate> negs;
      factory<Not> nots;
      factory<Complement> compls;
      factory<Plus> plss;
      factory<Dash> dshs;
      factory<Star> strs;
      factory<Slash> slhs;
      factory<Div> divs;
      factory<Quo> quos;
      factory<Rem> rems;
      factory<Mod> mods;
      factory<Langle> lgls;
      factory<Rangle> rgls;
      factory<Langleq> lglqs;
      factory<Rangleq> rglqs;
      factory<Eqeq> eqqs;
      factory<Excleq> xqs;
      factory<And> ands;
      factory<Or> ors;
      factory<BinaryLogical> bls;
      factory<Return> rets;
      factory<Throw> throws;
      factory<Loop> loops;
      factory<Leave> leaves;
      factory<IfExpression> ifs;
      Factory<Block> blocks;
      factory<CallExpression> calls;
      factory<Assertion> asserts;
      Factory<Match> matches;
      Factory<PatternMatch> pmatches;
      factory<Constraint> constraints;
      Factory<SubstExpr> substs;
      Factory<Signature> sigs;
      Factory<Formula> formulae;
      factory<Assumption> props;
      Factory<Concept> concepts;
      Factory<Import> imports;
   };

   template<typename T>
   using unary_builder = const T* (ExpressionFactory::*)
      (FunctionElaboration, Elaboration);

   template<typename T>
   using binary_builder = const T* (ExpressionFactory::*)
            (FunctionElaboration, Elaboration, Elaboration);

   inline const Identifier*
   make_identifier(ExpressionFactory* ctx, const std::string& s) {
      return ctx->build_identifier(ctx->intern(s));
   }

   inline const Operator*
   make_operator(ExpressionFactory* ctx, const std::string& s) {
      return ctx->build_operator(ctx->intern(s));
   }

   // -- Return a pointer to a function-like declaration with `name'
   // -- and `type' in the current binding `scope'.
   FunctionElaboration
   lookup_function(ScopeRef, const Name*, const ArrowType*);

   template<typename T>
   void
   expr_impl<T>::accept(Expression::Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename T>
   void
   unary_impl<T>::accept(Expression::Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename T>
   void
   binary_impl<T>::accept(Expression::Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename T>
   void
   value_impl<T>::accept(Expression::Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   template<typename T>
   void
   type_impl<T>::accept(Expression::Visitor& v) const {
      v.visit(static_cast<const T&>(*this));
   }

   // -- match(expr,pat)
   bool structural_equivalence(Elaboration, const Expression*);
   bool match(Elaboration, const Expression*, Substitution&);
   Substitution match_dependent(Elaboration, const Expression*, const Formals&);

   std::size_t expression_size(const Expression*);
   std::size_t top_level_conjunct_count(const Expression*);
}

#endif  // LIZ_EXPRESSION_INCLUDED
