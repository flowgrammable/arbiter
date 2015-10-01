// Copyright (C) 2012-2013, Texas A&M University.
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

#ifndef LIZ_CXX_INCLUDED
#define LIZ_CXX_INCLUDED

// -- This header defines the interface for the C++ backend of the
// -- Liz compiler.  After elaboration of input Liz source program,
// -- this module translate the internal representation to a subset
// -- of ISO C++.  The basic invariant is that any well-formed input
// -- Liz module should map to a well-formed C++ translation.  Any
// -- violation of this basic invariant is an internal error.

#include <map>
#include <liz/storage>
#include "Expression.H"
#include "Compiler.H"

namespace liz {
   namespace cxx {
      struct Expr;
      struct Decl;
      // ----------
      // -- Type --
      // ----------
      // Base class of internal representation of C++ types.
      struct Type {
         struct Visitor;
         virtual void accept(Visitor&) const = 0;
      };

      template<typename>
      struct type_impl : Type {
         void accept(Visitor&) const;
      };

      template<typename T>
      inline const T*
      is(const Type* t) {
         return dynamic_cast<const T*>(t);
      }

      // ---------------
      // -- NamedType --
      // ---------------
      // A named type -- can be a builtin, or a class, or an alias, etc.
      struct NamedType : type_impl<NamedType>,
                         structure::unary<Symbol> {
         explicit NamedType(Symbol);
         Symbol name() const { return operand(); }
      };

      // ---------------
      // -- ConstType --
      // ---------------
      // const-qualified C++ type.
      struct ConstType : type_impl<ConstType>,
                         structure::unary<const Type*> {
         explicit ConstType(const Type*);
         const Type* main_variant() const { return operand(); }
      };

      // -------------
      // -- RefType --
      // -------------
      // Reference to C++ type.
      struct RefType : type_impl<RefType>,
                       structure::unary<const Type*> {
         explicit RefType(const Type*);
         const Type* referee() const { return operand(); }
      };

      // --------------
      // -- UintType --
      // --------------
      // Reference to C++ type.
      struct UintType : type_impl<UintType>,
                        structure::unary<const Expr*> {
         explicit UintType(const Expr*);
         const Expr* precision() const { return operand(); }
      };

      // -------------
      // -- PtrType --
      // -------------
      // Pointer to a C++ type.
      struct PtrType : type_impl<PtrType>,
                       structure::unary<const Type*> {
         explicit PtrType(const Type*);
         const Type* pointee() const { return operand(); }
      };

      // ---------------
      // -- ArrayType --
      // ---------------
      // C++ builtin array type.
      struct ArrayType : type_impl<ArrayType>,
                         structure::binary<const Type*, offset_type> {
         ArrayType(const Type*, offset_type);
         const Type* element_type() const { return first(); }
         offset_type upper_bound() const { return second(); }
      };

      // -------------
      // -- FunType --
      // -------------
      // C++ function type.
      struct FunType : type_impl<FunType>,
                       structure::binary<Sequence<Type>, const Type*> {
         FunType(const Sequence<Type>&, const Type*);
         const Sequence<Type> source() const { return first(); }
         const Type* target() const { return second(); }
         std::size_t arity() const { return source().size(); }
      };

      // ----------------
      // -- ExprAsType --
      // ----------------
      struct ExprAsType : type_impl<ExprAsType>,
                          structure::unary<const Expr*> {
         explicit ExprAsType(const Expr*);
         const Expr* expression() const { return operand(); }
      };

      // --------------
      // -- AnonType --
      // --------------
      struct AnonType : type_impl<AnonType>,
                        structure::binary<const Type*, Sequence<Decl>> {
         AnonType(const Type*, const Sequence<Decl>&);
         const Type* kind() const { return first(); }
         const Sequence<Decl>& members() const { return second(); }
         std::size_t member_count() const { return members().size(); }
      };

      // --------------
      // -- InstType --
      // --------------
      struct InstType : type_impl<InstType>,
                        structure::binary<const Expr*, Sequence<Type>> {
         explicit InstType(const Expr*, const Sequence<Type>&);
         const Expr* constructor() const { return first(); }
         const Sequence<Type>& arguments() const { return second(); }
      };

      // -------------------
      // -- Type::Visitor --
      // -------------------
      struct Type::Visitor {
         virtual void visit(const NamedType&) = 0;
         virtual void visit(const ConstType&) = 0;
         virtual void visit(const RefType&) = 0;
         virtual void visit(const UintType&) = 0;
         virtual void visit(const ExprAsType&) = 0;
         virtual void visit(const PtrType&) = 0;
         virtual void visit(const ArrayType&) = 0;
         virtual void visit(const FunType&) = 0;
         virtual void visit(const AnonType&) = 0;
         virtual void visit(const InstType&) = 0;
      };

      template<typename T>
      void type_impl<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }

      

      // ----------
      // -- Expr --
      // ----------
      struct Expr {
         struct Visitor;
         virtual void accept(Visitor&) const = 0;
      };

      template<typename>
      struct expr_impl : Expr {
         void accept(Visitor&) const;
      };

      // ---------------
      // -- ConstExpr --
      // ---------------
      template<typename T>
      struct Constant : expr_impl<Constant<T>>, structure::unary<T> {
         explicit Constant(T t) : structure::unary<T>(t) { }
         const T& value() const { return this->operand(); }
      };

      // -----------
      // --IdExpr --
      // -----------
      struct IdExpr : expr_impl<IdExpr>, structure::unary<Symbol> {
         explicit IdExpr(Symbol);
         Symbol name() const { return operand(); }
      };

      // --------------
      // -- InstExpr --
      // --------------
      struct InstExpr : expr_impl<InstExpr>,
                        structure::binary<const Expr*, Sequence<Expr>> {
         InstExpr(const Expr*, const Sequence<Expr>&);
         const Expr* primary() const { return first(); }
         const Sequence<Expr>& arguments() const { return second(); }
      };

      // -----------------
      // -- InstOnTypes --
      // -----------------
      struct InstOnTypes : expr_impl<InstOnTypes>,
                           structure::binary<const Expr*, Sequence<Type>> {
         InstOnTypes(const Expr*, const Sequence<Type>&);
         const Expr* primary() const { return first(); }
         const Sequence<Type>& arguments() const { return second(); }
      };

      // -------------
      // -- DotExpr --
      // -------------
      struct DotExpr : expr_impl<DotExpr>,
                       structure::binary<const Expr*, Symbol> {
         DotExpr(const Expr*, Symbol);
         const Expr* object() const { return first(); }
         Symbol member() const { return second(); }
      };

      // ---------------
      // -- ArrowExpr --
      // ---------------
      struct ArrowExpr : expr_impl<ArrowExpr>,
                         structure::binary<const Expr*, Symbol> {
         ArrowExpr(const Expr*, Symbol);
         const Expr* object() const { return first(); }
         Symbol member() const { return second(); }
      };

      // ---------------
      // -- ScopeExpr --
      // ---------------
      struct ScopeExpr : expr_impl<ScopeExpr>,
                         structure::binary<const Expr*, Symbol> {
         ScopeExpr(const Expr*, Symbol);
         const Expr* scope() const { return first(); }
         Symbol member() const { return second(); }
      };

      // -------------------
      // -- SubscriptExpr --
      // -------------------
      struct SubscriptExpr : expr_impl<SubscriptExpr>,
                             structure::binary<const Expr*> {
         SubscriptExpr(const Expr*, const Expr*);
         const Expr* array() const { return first(); }
         const Expr* index() const { return second(); }
      };

      // --------------
      // -- CallExpr --
      // --------------
      struct CallExpr : expr_impl<CallExpr>,
                        structure::binary<const Expr*, Sequence<Expr>> {
         CallExpr(const Expr*, const Sequence<Expr>&);
         const Expr* operation() const { return first(); }
         const Sequence<Expr>& arguments() const { return second(); }
      };
      
      // ----------------
      // -- ObjectExpr --
      // ----------------
      struct ObjectExpr : expr_impl<ObjectExpr>,
                         structure::binary<const cxx::Type*, Sequence<Expr>> {
         ObjectExpr(const cxx::Type*, const Sequence<Expr>&);
         const Type* type() const { return first(); }
         const Sequence<Expr>& arguments() const { return second(); }
      };

      // -----------------------
      // -- PostIncrementExpr --
      // -----------------------
      struct PostIncrementExpr : expr_impl<PostIncrementExpr>,
                                 structure::unary<const Expr*> {
         explicit PostIncrementExpr(const Expr*);
      };

      // -----------------------
      // -- PostDecrementExpr --
      // -----------------------
      struct PostDecrementExpr : expr_impl<PostDecrementExpr>,
                                structure::unary<const Expr*> {
         explicit PostDecrementExpr(const Expr*);
      };

      // ---------------
      // -- DerefExpr --
      // ---------------
      struct DerefExpr : expr_impl<DerefExpr>,
                         structure::unary<const Expr*> {
         explicit DerefExpr(const Expr*);
         const Expr* address() const { return operand(); }
      };

      // -------------
      // -- NegExpr --
      // -------------
      struct NegExpr : expr_impl<NegExpr>, structure::unary<const Expr*> {
         explicit NegExpr(const Expr*);
      };

      // -------------
      // -- NotExpr --
      // -------------
      struct NotExpr : expr_impl<NotExpr>,
                       structure::unary<const Expr*> {
         explicit NotExpr(const Expr*);
      };

      // ---------------
      // -- ComplExpr --
      // ---------------
      struct ComplExpr : expr_impl<ComplExpr>,
                         structure::unary<const Expr*> {
         explicit ComplExpr(const Expr*);
      };

      // ---------------
      // -- Operation --
      // ---------------
      enum class Operation {
         Mult, Div, Rem, Mod, Plus, Minus, Lshift, Rshift,
         Less, Greater, LessEq, GreaterEq, Eq, NotEq, 
         Bitand, Bitxor, Bitor, And, Or
      };

      // ---------------
      // -- BinayExpr --
      // ---------------
      template<Operation op>
      struct BinaryExpr : expr_impl<BinaryExpr<op>>,
                          structure::ternary<const Expr*> {
         BinaryExpr(const Expr* o, const Expr* x, const Expr* y)
            : structure::ternary<const Expr*>(o, x, y) { }
         const Expr* operation() const { return first(); }
         const Expr* lhs() const { return second(); }
         const Expr* rhs() const { return third(); }
      };

      // ---------------------
      // -- ConditionalExpr --
      // ---------------------
      struct ConditionalExpr : expr_impl<ConditionalExpr>,
                               structure::ternary<const Expr*> {
         ConditionalExpr(const Expr*, const Expr*, const Expr*);
         const Expr* condition() const { return first(); }
         const Expr* consequence() const { return second(); }
         const Expr* alternative() const { return third(); }
      };

      // ----------------
      // -- AssignExpr --
      // ----------------
      struct AssignExpr : expr_impl<AssignExpr>,
                          structure::binary<const Expr*> {
         AssignExpr(const Expr*, const Expr*);
         const Expr* lhs() const { return first(); }
         const Expr* rhs() const { return second(); }
      };

      // ----------------
      // -- ReturnExpr --
      // ----------------
      struct ReturnExpr : expr_impl<ReturnExpr>,
                          structure::unary<const Expr*> {
         ReturnExpr(const Expr*);
         const Expr* expr() const { return operand(); }
      };

      // ------------
      // -- Region --
      // ------------
      struct Region : expr_impl<Region>, Sequence<Decl> {
      };

      // -------------------
      // -- Expr::Visitor --
      // -------------------
      struct Expr::Visitor {
         virtual void visit(const Constant<bool>&) = 0;
         virtual void visit(const Constant<Character>&) = 0;
         virtual void visit(const Constant<offset_type>&) = 0;
         virtual void visit(const Constant<double>&) = 0;
         virtual void visit(const Constant<std::string>&) = 0;
         virtual void visit(const Constant<Key>&) = 0;
         virtual void visit(const IdExpr&) = 0;
         virtual void visit(const InstExpr&) = 0;
         virtual void visit(const InstOnTypes&) = 0;
         virtual void visit(const ReturnExpr&) = 0;
         virtual void visit(const Region&) = 0;
         virtual void visit(const DotExpr&) = 0;
         virtual void visit(const ArrowExpr&) = 0;
         virtual void visit(const ScopeExpr&) = 0;
         virtual void visit(const SubscriptExpr&) = 0;
         virtual void visit(const CallExpr&) = 0;
         virtual void visit(const ObjectExpr&) = 0;
         virtual void visit(const PostIncrementExpr&) = 0;
         virtual void visit(const PostDecrementExpr&) = 0;
         virtual void visit(const DerefExpr&) = 0;
         virtual void visit(const NegExpr&) = 0;
         virtual void visit(const NotExpr&) = 0;
         virtual void visit(const ComplExpr&) = 0;
         virtual void visit(const BinaryExpr<Operation::Mult>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Div>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Rem>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Mod>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Plus>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Minus>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Lshift>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Rshift>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Less>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Greater>&) = 0;
         virtual void visit(const BinaryExpr<Operation::LessEq>&) = 0;
         virtual void visit(const BinaryExpr<Operation::GreaterEq>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Eq>&) = 0;
         virtual void visit(const BinaryExpr<Operation::NotEq>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Bitand>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Bitxor>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Bitor>&) = 0;
         virtual void visit(const BinaryExpr<Operation::And>&) = 0;
         virtual void visit(const BinaryExpr<Operation::Or>&) = 0;
         virtual void visit(const ConditionalExpr&) = 0;
         virtual void visit(const AssignExpr&) = 0;
      };

      template<typename T>
      void expr_impl<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }


      // ----------
      // -- Stmt --
      // ----------
      struct Stmt {
         struct Visitor;
         virtual void accept(Visitor&) const = 0;
      };

      // -- Boilerplate implementation of Stmt::accept().
      template<typename>
      struct stmt_impl : Stmt {
         void accept(Visitor&) const;
      };

      // -----------------
      // -- LabeledStmt --
      // -----------------
      struct LabeledStmt : stmt_impl<LabeledStmt>,
                           structure::binary<Symbol, const Stmt*> {
         LabeledStmt(Symbol, const Stmt*);
         Symbol label() const { return first(); }
         const Stmt* statement() const { return second(); }
      };

      // --------------
      // -- CaseStmt --
      // --------------
      struct CaseStmt : stmt_impl<CaseStmt>,
                        structure::binary<const Expr*, const Stmt*> {
         CaseStmt(const Expr*, const Stmt*);
         const Expr* guard() const { return first(); }
         const Stmt* statement() const { return second(); }
      };

      // -----------------
      // -- DefaultStmt --
      // -----------------
      struct DefaultStmt : stmt_impl<DefaultStmt>,
                           structure::unary<const Stmt*> {
         explicit DefaultStmt(const Stmt*);
         const Stmt* statement() const { return operand(); }
      };

      // --------------
      // -- ExprStmt --
      // --------------
      struct ExprStmt : stmt_impl<ExprStmt>,
                        structure::unary<const Expr*> {
         explicit ExprStmt(const Expr*);
         const Expr* expression() const { return operand(); }
      };

      // ------------------
      // -- CompoundStmt --
      // ------------------
      struct CompoundStmt : stmt_impl<CompoundStmt>,
                            structure::unary<Sequence<Stmt>> {
         CompoundStmt(const Sequence<Stmt>&);
         std::size_t size() const { return operand().size(); }
         const Stmt* operator[](int i) const { return operand().at(i); }
      };

      // ------------
      // -- IfStmt --
      // ------------
      struct IfStmt : stmt_impl<IfStmt>,
                      structure::ternary<const Expr*, const Stmt*> {
         IfStmt(const Expr*, const Stmt*, const Stmt*);
         const Expr* condition() const { return first(); }
         const Stmt* consequence() const { return second(); }
         const Stmt* alternative() const { return third(); }
      };

      // ----------------
      // -- SwitchStmt --
      // ----------------
      struct SwitchStmt : stmt_impl<SwitchStmt>,
                          structure::binary<const Expr*, Sequence<Stmt>> {
         SwitchStmt(const Expr*, const Sequence<Stmt>&);
         const Expr* scrutinee() const { return first(); }
         const Sequence<Stmt>& cases() const { return second(); }
      };

      // ---------------
      // -- LoopStmt --
      // ---------------
      struct LoopStmt : stmt_impl<LoopStmt>,
                         structure::unary<const Stmt*> {
         LoopStmt(const Stmt*);
         const Stmt* body() const { return operand(); }
      };

      // ---------------
      // -- BreakStmt --
      // ---------------
      struct BreakStmt : stmt_impl<BreakStmt> {
      };

      // ------------------
      // -- ContinueStmt --
      // ------------------
      struct ContinueStmt : stmt_impl<ContinueStmt> {
      };

      // ----------------
      // -- ReturnStmt --
      // ----------------
      struct ReturnStmt : stmt_impl<ReturnStmt>,
                          structure::unary<const Expr*> {
         explicit ReturnStmt(const Expr*);
         const Expr* expression() const { return operand(); }
      };

      // ---------------
      // -- GotoStmt ---
      // ---------------
      struct GotoStmt : stmt_impl<GotoStmt>,
                        structure::unary<Symbol> {
         explicit GotoStmt(Symbol);
         Symbol label() const { return operand(); }
      };

      // --------------
      // -- DeclStmt --
      // --------------
      struct DeclStmt : stmt_impl<DeclStmt>,
                        structure::unary<const Decl*> {
         explicit DeclStmt(const Decl*);
         const Decl* declaration() const { return operand(); }
      };

      // -------------------
      // -- Stmt::Visitor --
      // -------------------
      struct Stmt::Visitor {
         virtual void visit(const LabeledStmt&) = 0;
         virtual void visit(const CaseStmt&) = 0;
         virtual void visit(const DefaultStmt&) = 0;
         virtual void visit(const ExprStmt&) = 0;
         virtual void visit(const CompoundStmt&) = 0;
         virtual void visit(const IfStmt&) = 0;
         virtual void visit(const SwitchStmt&) = 0;
         virtual void visit(const LoopStmt&) = 0;
         virtual void visit(const BreakStmt&) = 0;
         virtual void visit(const ContinueStmt&) = 0;
         virtual void visit(const ReturnStmt&) = 0;
         virtual void visit(const GotoStmt&) = 0;
         virtual void visit(const DeclStmt&) = 0;
      };

      template<typename T>
      void stmt_impl<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }
      
      // ----------
      // -- Decl --
      // ----------
      struct Decl {
         struct Visitor;
         virtual Symbol name() const = 0;
         virtual const Type* type() const = 0;
         virtual void accept(Visitor&) const = 0;
      };

      template<typename>
      struct decl_impl : Decl {
         void accept(Visitor&) const;
      };

      // ------------
      // -- VarDef --
      // ------------
      struct VarDef : decl_impl<VarDef>,
                      structure::ternary<Symbol, const Type*, const Expr*> {
         VarDef(Symbol, const Type*, const Expr*);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
         const Expr* initializer() const { return third(); }
      };

      // -------------
      // -- VarDecl --
      // -------------
      struct VarDecl : decl_impl<VarDecl>,
                       structure::binary<Symbol, const Type*> {
         VarDecl(Symbol, const Type*);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
      };

      // ---------------
      // -- FieldDecl --
      // ---------------
      struct FieldDecl : decl_impl<FieldDecl>,
                         structure::binary<Symbol, const Type*> {
         FieldDecl(Symbol, const Type*);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
      };

      // ----------
      // -- Parm --
      // ----------
      struct Parm : decl_impl<Parm>,
                    structure::binary<Symbol, const Type*> {
         Parm(Symbol, const Type*);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
      };

      // -----------------
      // -- Abstraction --
      // -----------------
      struct Abstraction : structure::binary<Sequence<Parm>, CompoundStmt> {
         Abstraction(const Sequence<Parm>&, const Sequence<Stmt>&);
         const Sequence<Parm>& parameters() const { return first(); }
         const CompoundStmt& body() const { return second(); }
      };

      // -------------
      // -- FunDecl --
      // -------------
      struct FunDecl : decl_impl<FunDecl>,
                       structure::binary<Symbol, const Type*> {
         FunDecl(Symbol, const Type*);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
      };

      // ------------
      // -- FunDef --
      // ------------
      struct FunDef : decl_impl<FunDef>,
                      structure::ternary<Symbol, const Type*, Abstraction> {
         FunDef(Symbol, const Type*, const Abstraction&);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
         const Sequence<Parm>& parameters() const {
            return third().parameters();
         }
         const CompoundStmt& body() const { return third().body(); }
      };

      // ---------------
      // -- TypeDecl --
      // ---------------
      struct TypeDecl : decl_impl<TypeDecl>,
                        structure::binary<Symbol, const cxx::Type*> {
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
      };

      // --------------
      // -- UsingDef --
      // --------------
      struct UsingDef : decl_impl<UsingDef>,
                        structure::binary<Symbol, const Type*>
      {
         explicit UsingDef(Symbol, const Type* t);
         Symbol name() const { return first(); }
         const Type* type() const { return second(); }
      };

      // --------------
      // -- ScopeDef --
      // --------------
      struct ScopeDef : decl_impl<ScopeDef>, Sequence<Decl> {
         explicit ScopeDef(Symbol);
         Symbol name() const { return n; }
         const Type* type() const { return nullptr; }
      private:
         const Symbol n;
      };

      // ---------------
      // -- StructDef --
      // ---------------
      struct StructDef : decl_impl<StructDef>, Sequence<Decl> {
         explicit StructDef(Symbol);
         Symbol name() const { return n; }
         const cxx::Type* type() const { return nullptr; }
      private:
         Symbol const n;
      };

      // --------------
      // -- UnionDef --
      // --------------
      struct UnionDef : decl_impl<UnionDef>, Sequence<Decl> {
         explicit UnionDef(Symbol);
         Symbol name() const { return n; }
         const cxx::Type* type() const { return nullptr; }
      private:
         Symbol const n;
      };

      // -------------
      // -- EnumDef --
      // -------------
      struct EnumDef : decl_impl<EnumDef>, std::vector<Symbol> {
         explicit EnumDef(Symbol, const std::vector<Symbol>&);
         Symbol name() const { return n; }
         const cxx::Type* type() const { return nullptr; }
      private:
         Symbol n;
      };

      // ----------------
      // -- VariantDef --
      // ----------------
      struct VariantDef : decl_impl<VariantDef>, Sequence<Decl> {
         explicit VariantDef(Symbol, const UnionDef*, const Sequence<Decl>&);
         Symbol name() const { return n; }
         const cxx::Type* type() const { return d->type(); }
         const UnionDef* definition() const { return d; }
      private:
         Symbol n;
         const UnionDef* d;
      };

      // ---------------
      // -- TmpltDecl --
      // ---------------
      struct TmpltDecl : decl_impl<TmpltDecl>,
                         structure::binary<Sequence<Decl>, const Decl*> {
         TmpltDecl(const Sequence<Decl>&, const Decl*);
         const Sequence<Decl>& parameters() const { return first(); }
         const Decl* instance() const { return second(); }
         Symbol name() const { return instance()->name(); }
         const Type* type() const { return nullptr; }
      };

      // --------------
      // -- DeclPack --
      // --------------
      struct DeclPack : decl_impl<DeclPack>,
                        structure::binary<const Decl*, Sequence<Decl>> {
         DeclPack(const Decl*, const Sequence<Decl>&);
         const Decl* primary() const { return first(); }
         const Sequence<Decl>& specializations() const { return second(); }
         Symbol name() const { return first()->name(); }
         const Type* type() const { return first()->type(); }
      };

      // -------------------
      // -- Decl::Visitor --
      // -------------------
      struct Decl::Visitor {
         virtual void visit(const Decl&) = 0;
         virtual void visit(const Parm&);
         virtual void visit(const VarDecl&);
         virtual void visit(const VarDef&);
         virtual void visit(const FieldDecl&);
         virtual void visit(const FunDecl&);
         virtual void visit(const FunDef&);
         virtual void visit(const UsingDef&);
         virtual void visit(const ScopeDef&);
         virtual void visit(const StructDef&);
         virtual void visit(const UnionDef&);
         virtual void visit(const EnumDef&);
         virtual void visit(const VariantDef&);
         virtual void visit(const TmpltDecl&);
         virtual void visit(const DeclPack&);
      };

      template<typename T>
      void decl_impl<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }

      // --------------
      // -- Toplevel --
      // --------------
      struct Toplevel {
         struct Visitor;
         virtual void accept(Visitor&) const = 0;
      };

      template<typename>
      struct top_impl : Toplevel {
         void accept(Visitor&) const;
      };

      // -------------
      // -- TopDecl --
      // -------------
      struct TopDecl : top_impl<TopDecl>, structure::unary<const Decl*> {
         explicit TopDecl(const Decl*);
         const Decl* declaration() const { return operand(); }
      };

      void format(const Decl*, std::ostream&);
      void format_forward_decl(const FunDef*, std::ostream&);
      void format_toplevel(const Toplevel*, std::ostream&);

      // -------------
      // -- Include --
      // -------------
      struct Include : top_impl<Include>, structure::unary<Path> {
         explicit Include(const Path&);
         const Path& header() const { return operand(); }
      };

      // -----------------------
      // -- Toplevel::Visitor --
      // -----------------------
      struct Toplevel::Visitor {
         virtual void visit(const TopDecl&) = 0;
         virtual void visit(const Include&) = 0;
      };

      template<typename T>
      void top_impl<T>::accept(Visitor& v) const {
         v.visit(static_cast<const T&>(*this));
      }

      // ---------------
      // -- Factoties --
      // ---------------
      struct Factories {
         Factories();

         Factory<NamedType> named_type;
         Factory<PtrType> ptr_type;
         Factory<RefType> ref_type;
         Factory<UintType> uint_type;
         Factory<ArrayType> array_type;
         Factory<FunType> fun_type;
         Factory<ConstType> const_type;
         Factory<AnonType> anon_type;
         Factory<InstType> inst_type;
         Factory<ExprAsType> expr_type;

         const Constant<bool> false_cst;
         const Constant<bool> true_cst;
         Factory<Constant<Character>> char_cst;
         Factory<Constant<offset_type>> int_cst;
         Factory<Constant<double>> double_cst;
         Factory<Constant<std::string>> string_cst;
         Factory<Constant<Key>> key_cst;
         Factory<IdExpr> id_expr;
         Factory<InstExpr> inst_expr;
         Factory<InstOnTypes> tinst_expr;
         Factory<ReturnExpr> return_expr;
         Factory<DotExpr> dot_expr;
         Factory<ArrowExpr> arrow_expr;
         Factory<ScopeExpr> scope_expr;
         Factory<SubscriptExpr> sub_expr;
         Factory<CallExpr> call_expr;
         Factory<ObjectExpr> object_expr;
         Factory<PostIncrementExpr> postinc_expr;
         Factory<PostDecrementExpr> postdec_expr;
         Factory<DerefExpr> deref_expr;
         Factory<NegExpr> neg_expr;
         Factory<NotExpr> not_expr;
         Factory<ComplExpr> compl_expr;
         Factory<BinaryExpr<Operation::Mult>> mult_expr;
         Factory<BinaryExpr<Operation::Div>> div_expr;
         Factory<BinaryExpr<Operation::Rem>> rem_expr;
         Factory<BinaryExpr<Operation::Mod>> mod_expr;
         Factory<BinaryExpr<Operation::Plus>> plus_expr;
         Factory<BinaryExpr<Operation::Minus>> minus_expr;
         Factory<BinaryExpr<Operation::Lshift>> lshist_expr;
         Factory<BinaryExpr<Operation::Rshift>> rshist_expr;
         Factory<BinaryExpr<Operation::Less>> lt_expr;
         Factory<BinaryExpr<Operation::Greater>> gt_expr;
         Factory<BinaryExpr<Operation::LessEq>> le_expr;
         Factory<BinaryExpr<Operation::GreaterEq>> ge_expr;
         Factory<BinaryExpr<Operation::Eq>> eq_expr;
         Factory<BinaryExpr<Operation::NotEq>> neq_expr;
         Factory<BinaryExpr<Operation::Bitand>> bitand_expr;
         Factory<BinaryExpr<Operation::Bitxor>> bitxor_expr;
         Factory<BinaryExpr<Operation::Bitor>> bitor_expr;
         Factory<BinaryExpr<Operation::And>> and_expr;
         Factory<BinaryExpr<Operation::Or>> or_expr;
         Factory<ConditionalExpr> cond_expr;
         Factory<AssignExpr> assign_expr;
         Factory<Region> region_expr;

         Factory<ExprStmt> expr_stmt;
         Factory<LabeledStmt> lbl_stmt;
         Factory<CaseStmt> case_stmt;
         Factory<DefaultStmt> dft_stmt;
         Factory<CompoundStmt> cmpd_stmt;
         Factory<IfStmt> if_stmt;
         Factory<SwitchStmt> switch_stmt;
         Factory<LoopStmt> loop_stmt;
         const BreakStmt brk_stmt;
         const ContinueStmt cont_stmt;
         Factory<ReturnStmt> ret_stmt;
         Factory<GotoStmt> goto_stmt;
         Factory<DeclStmt> decl_stmt;

         Factory<VarDecl> var_decl;
         Factory<VarDef> var_def;
         Factory<FieldDecl> fld_decl;
         Factory<Parm> parm_decl;
         Factory<FunDecl> fun_decl;
         Factory<FunDef> fun_def;
         Factory<ScopeDef> scope_def;
         Factory<UsingDef> using_def;
         Factory<StructDef> struct_def;
         Factory<UnionDef> union_def;
         Factory<EnumDef> enum_def;
         Factory<VariantDef> variant_def;
         Factory<TmpltDecl> tmplt_decl;
         Factory<DeclPack> pack_decl;

         Factory<TopDecl> decl_toplevel;
         Factory<Include> include_toplevel;
      };

      // -------------
      // -- Backend --
      // -------------
      struct Backend {
         explicit Backend(Compiler&);
         // Translate a Liz type to a C++ type.
         const cxx::Type* translate(const liz::Type*);
         // Translate a Liz name to a C++ name.
         const cxx::Expr* translate_name(const liz::Name*);
         // Translate a Liz expression to its C++ equivalent.
         const cxx::Expr* translate_expr(Elaboration);
         // Translate a Liz statement to its C++ equivalent.
         const cxx::Stmt* translate_stmt(Elaboration);
         // Translate a Liz declaration to a C++ declaration.
         const cxx::Decl* translate_decl(const liz::Expression*);
         const cxx::Decl* translate_decl(const liz::TagType*);
         // Translate a toplevel item.
         const cxx::Toplevel* translate_top_level(const liz::Expression*);

         const cxx::Type* lookup(const liz::Type*) const;
         Sequence<Parm> translate(const liz::Formals&);
         const cxx::Decl* translate_def(Symbol, const liz::RecordType*);
         const cxx::Decl* translate_def(const liz::GenerativeType*);
         const cxx::Decl* translate_scope_def(const liz::Namespace*);
         const cxx::Decl* translate_variant_def(Symbol, const liz::VariantType*);
         const cxx::Decl* translate_enum_def(Symbol, const liz::VariantType*);
         const cxx::Decl* translate_fundef(const liz::Lambda*, const cxx::Type*);
         const cxx::Decl* translate_template(const liz::Lambda*);
         const cxx::Decl* translate_ctor(const liz::Constructor*);

         using Dict = std::map<const liz::Type*, const cxx::Type*>;

         enum struct register_result { first_occurance, already_registered };
         register_result register_specialization(Elaboration);


         Compiler& comp;
         Dict dict;
         Factories depot;
         std::vector<Elaboration> specs_seen;
         std::vector<const cxx::Decl*> fwd_decl;
      };

      const cxx::Decl* specs_to_decl(Backend&,Elaboration);

      inline bool is_type_decl(const Decl* decl) {
         return dynamic_cast<const StructDef*>(decl)
             or dynamic_cast<const UsingDef*>(decl)
             or dynamic_cast<const UnionDef*>(decl)
             or dynamic_cast<const EnumDef*>(decl)
             or dynamic_cast<const VariantDef*>(decl)
             or dynamic_cast<const TypeDecl*>(decl);
      }
   }
}

#endif  // LIZ_CXX_INCLUDED
