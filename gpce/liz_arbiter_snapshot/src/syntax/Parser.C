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

#include <iterator>
#include <iostream>
#include <string>
#include <sstream>
#include <algorithm>

#include "Parser.H"

namespace liz {
   // -- Parser::IndentationManager --
   Parser::IndentationManager::IndentationManager(Parser* p, input::Column c)
         : parser(p) {
      parser->ctx.indents.push(c);
   }

   Parser::IndentationManager::~IndentationManager() {
      parser->ctx.indents.pop();
   }

   // -- Return true iff the value `t' is in the sequence `seq'.
   template<typename T, int N>
   static bool
   member(const T& t, const T (&seq)[N]) {
      return std::find(seq, seq + N, t) != seq + N;
   }

   template<typename T>
   static const T*
   is(const FixityForm* f) {
      return dynamic_cast<const T*>(f);
   }
   
   // -- ErrorAt --
   ErrorAt::ErrorAt(const std::string& s, const Token& t)
         : BasicError(s),
           tok(t)
   { }

   void
   ErrorAt::disclose_location_on(std::ostream& os) const {
      os << "on line " << line().number()
         << ", column " << column(token()) << ':' << '\n';
      os << line().text() << '\n';
      for (Ordinal i = 0; i < column(token()); ++i)
         os << ' ';
      os << '^' << '\n';
   }

   void error_at(const Token& t, const std::string& s) {
      throw ErrorAt(s, t);
   }

   // --
   static TokenIterator
   begin(const ParsingContext* ctx) {
      return ctx->cursor;
   }

   static TokenIterator
   end(const ParsingContext* ctx) {
      return ctx->last;
   }

   // Return true if we have exhausted the token stream.
   static bool
   at_eoi(const ParsingContext* ctx) {
      return begin(ctx) == end(ctx);
   }

   static const Token*
   current_token(const ParsingContext* ctx) {
      return &*begin(ctx);
   }
   
//    // Current indentation level
//    static input::Column
//    indentation(const ParsingContext* ctx) {
//       return ctx->indents.top();
//    }
   
   static void
   parse_error(Parser* parser, const std::string& msg) {
      if (at_eoi(parser->context()))
         parser->error(msg);
      else
         parser->error_at(msg, *current_token(parser->context()));
   }

   // Return true if the token pointed by `p' is a justification
   // mark at column `c'.
   static bool
   layout_justify_at(TokenIterator p, input::ColumnNumber c) {
      return p->kind == token::justify_tok and column(*p) == c;
   }

   // -- Helper functions to move forward through the input token stream.
   // -- to the parser.  We expected that the input stream may contain
   // -- comment, although it is common practice not to include them in
   // -- the original input token stream.

   static inline bool
   formatting(TokenIterator p) {
      const auto tk = p->kind;
      return tk == token::indent_tok
         or tk == token::justify_tok
         or tk == token::unindent_tok;
   }

   static const Token*
   advance_if_eq(TokenIterator& p, token::Kind tk) {
      if (p->kind == tk)
         return &*p++;
      return nullptr;
   }

   static bool
   match_formatted_wisecrack(TokenIterator& cur) {
      if (advance_if_eq(cur, token::wisecrack_tok))
         return true;
      auto p = cur;
      if (formatting(p) and advance_if_eq(++p, token::wisecrack_tok)) {
         cur = p;
         return true;
      }
      return false;
   }

   static TokenIterator
   skip_blank(TokenIterator& cur, TokenIterator last) {
      while (cur != last and match_formatted_wisecrack(cur))
         ;
      return cur;
   }

   // Checks if the next current token is of type `tt' and return a
   // pointer to the corresponding token object.  Otherwise, return null.
   static const Token*
   match(ParsingContext* ctx, token::Kind tt) {
      if (skip_blank(ctx->cursor, ctx->last) == ctx->last)
         return nullptr;
      else if (auto p = advance_if_eq(ctx->cursor, tt))
         return p;
      return nullptr;
   }

   // -- Same as match(), except that the token is allowed to be at
   // -- the start of a new line.
   static const Token*
   flexible_match(Parser* parser, token::Kind tk) {
      auto ctx = parser->context();
      // FIXME: Check that indentation level is greater than curruent.
      match(ctx, token::indent_tok);
      return match(ctx, tk);
   }

   // Return a pointer to the current token if it is of specified kind.
   static const Token*
   match_if(Parser* parser, bool pred(token::Kind)) {
      auto ctx = parser->context();
      if (skip_blank(ctx->cursor, ctx->last) == ctx->last)
         return nullptr;
      else if (pred(ctx->cursor->kind))
         return &*ctx->cursor++;
      return nullptr;
   }

   // Return a pointer to the nth token in the tokens stream, if there
   // are sufficient tokens; otherwise return null.  Comments tokens
   // are ignored.  The index is 0-based.
   static const Token*
   nth_token(ParsingContext* ctx, int n) {
      auto cur = ctx->cursor;
      while (skip_blank(cur, ctx->last) != ctx->last and n > 0) {
         ++cur;
         --n;
      }
      return n == 0 and cur != ctx->last ? &*cur : nullptr;
   }
   
   // Return true if the nth token in the tokens tream is of type
   // indicated by `tt'.  See also `nth_token'.
   static const Token*
   nth_token_is(ParsingContext* ctx, int n, token::Kind k) {
      const Token* t = nth_token(ctx, n);
      if (t != nullptr and t->kind == k)
         return t;
      return nullptr;
   }
   
   // Return true if the nth token in the tokens tream is of type
   // indicated by the predicate `pred'.  See also `nth_token'.
   template<typename Predicate>
   static bool
   nth_token_is(ParsingContext* ctx, int n, Predicate pred) {
      const Token* t = nth_token(ctx, n);
      return t != nullptr and pred(t->kind);
   }
   
   // return non-null if the next non-comment token is as indicated
   static const Token*
   next_token_is(ParsingContext* ctx, token::Kind tt) {
      return nth_token_is(ctx, 1, tt);
   }

   // Advance the cursor to current token by a given amount and return
   // a pointer to the token at the new position, if any.
   static const Token*
   advance(ParsingContext* ctx, int n = 1) {
      while (skip_blank(ctx->cursor, ctx->last) != ctx->last and n > 0)
         ++ctx->cursor, --n;
      return at_eoi(ctx) ? nullptr : current_token(ctx);
   }

   // -- Accept a newline indentation at a specific column.
   static const Token*
   match_justify_at(ParsingContext* ctx, input::Column c) {
      if (skip_blank(ctx->cursor, ctx->last) != ctx->last
          and layout_justify_at(ctx->cursor, c))
         return &*ctx->cursor++;
      return nullptr;
   }

   // -- Expect next line to start at specified indentation column.
   static void
   require_indentation_at(Parser* parser, input::Column c) {
      auto t = match(parser->context(), token::justify_tok);
      if (t == nullptr)
         parse_error(parser, "newline expected at column " + std::to_string(c));
      else if (column(*t) != c)
         parser->error_at("unexpected indentation level", *t);
   }

   static Ordinal
   layout_line_count(Parser* parser) {
      auto ctx = parser->context();
      auto cur = ctx->cursor;
      if (cur->kind != token::indent_tok)
         return 0;
      input::Column c = column(*cur);
      Cardinal line_count = 1;
      for (++cur; cur != ctx->last; ++cur)
         if (layout_justify_at(cur, c))
            ++line_count;
         else if (cur->kind == token::unindent_tok and column(*cur) < c)
            break;
      return line_count;
   }

   // If the parser's cursor is at a token of the specified class,
   // return a pointer to that token; otherwise null.
   static const Token*
   staring_at(Parser* parser, token::Kind tk) {
      auto ctx = parser->context();
      if (not at_eoi(ctx) and ctx->cursor->kind == tk)
         return &*ctx->cursor;
      return nullptr;
   }

   template<int N>
   static const Token*
   staring_at(Parser* parser, const token::Kind (&ts)[N]) {
      auto ctx = parser->context();
      for (int i = 0; i < N; ++i)
         if (not at_eoi(ctx) and ctx->cursor->kind == ts[i])
            return &*ctx->cursor;
      return nullptr;
   }

   // ------------------------
   // -- Parser combinators --
   // ------------------------

   // Return the matching closer token type of `tt'. Pairs of opening
   // and closing tokens ('brackets') are:
   //         "("   ")"
   //         "["   "]"
   //         "{"   "}"
   //         "<"   ">"
   //         "[|"  "|]"
   //         "{|"  "|}"
   static token::Kind closer(token::Kind tt) {
      switch (tt) {
      case token::open_paren_tok:
         return token::close_paren_tok;
         
      case token::open_brace_tok:
         return token::close_brace_tok;
         
      case token::open_bracket_tok:
         return token::close_bracket_tok;

      case token::less_tok:
         return token::greater_tok;

      case token::open_mbracket_tok:
         return token::close_mbracket_tok;

      case token::open_bar_brace_tok:
         return token::close_bracket_tok;
         
      default:
         throw BasicError("invalid enclosure token type");
      }
   }

   // The type of a function implementing a particular grammar production.
   using Production = const Ast* (*)(Parser*);

   // Checks for a required token, or else.
   static const Token*
   require(Parser* parser, token::Kind tt) {
      if (auto t = match(parser->context(), tt))
         return t;
      parse_error(parser, "expected " + show(tt));
      return nullptr;
   }

   // Attempt to match one of the token type indicated in the
   // array `ts'.  If successful return the corresponding token.
   template<int N>
   static const Token*
   match_one(Parser* parser, const token::Kind (&ts)[N]) {
      for (int i = 0; i < N; ++i)
         if (auto t = match(parser->context(), ts[i]))
            return t;
      return nullptr;
  }

   template<int N>
   static const OperatorAst*
   parse_symbol(Parser* parser, const token::Kind (&ts)[N]) {
      if (const Token* t = match_one(parser, ts))
         return parser->make_operator(*t);
      return nullptr;
   }

   static const OperatorAst*
   parse_symbol(Parser* parser, token::Kind tt) {
      if (auto t = match(parser->context(), tt))
         return parser->make_operator(*t);
      return nullptr;
   }

   // Match a specific `production', or else issue diagnostic `message'.
   template<typename T>
   static T
   must(Parser* parser, T production(Parser*), const std::string& message) {
      const T ast = production(parser);
      if (ast == T())
         parse_error(parser, message);
      return ast;
   }

   // Like the combinator 'must' except that `production' must be
   // bracketed by parenthesis indicated byt `tt'.
   template<typename T>
   static T
   match_enclosed(Parser* parser, T production(Parser*), token::Kind tt,
                  std::string diagnostic) {
      if (not match(parser->context(), tt))
         return T();
      T ast = must(parser, production, diagnostic);
      require(parser, closer(tt));
      return ast;
   }

   // Like the combinator 'match_enclosed' except that the whole
   // syntactic construct is optional.
   template<typename T>
   static T
   match_enclosed_optional(Parser* parser, T production(Parser*),
                           token::Kind tt, std::string diagnostic) {
      if (not match(parser->context(), tt))
         return T();
      if (not match(parser->context(), closer(tt))) {
         T ast = must(parser, production, diagnostic);
         require(parser, closer(tt));
         return ast;
      }
      return T();
   }

   // Match `production' enclosed in round brakcets.
   template<typename T>
   static inline T
   match_parenthesized(Parser* parser, T production(Parser*), std::string m) {
      return match_enclosed(parser, production, token::open_paren_tok, m);
   }

   // Like 'match_parenthesized', except that the construct is optional.
   template<typename T>
   static inline T
   match_parenthesized_optional(Parser* parser, T production(Parser*),
                                std::string diagnostic) {
      return match_enclosed_optional(parser, production,
                                     token::open_paren_tok, diagnostic);
   }

   template<typename T>
   static T
   require_enclosed(Parser* parser, T production(Parser*), token::Kind tt,
                    std::string diagnostic) {
      require(parser, tt);
      T ast = must(parser, production, diagnostic);
      require(parser, closer(tt));
      return ast;
   }

   template<typename T>
   static T
   require_enclosed_optional(Parser* parser, T production(Parser*),
                             token::Kind tt, std::string diagnostic) {
      require(parser, tt);
      if (not match(parser->context(), closer(tt))) {
         T ast = must(parser, production, diagnostic);
         require(parser, closer(tt));
         return ast;
      }
      return T();
   }

   template<typename T>
   static inline T
   require_parenthesized(Parser* parser, T production(Parser*),
                         std::string m) {
      return require_enclosed(parser, production, token::open_paren_tok, m);
   }

   template<typename T>
   static inline T
   require_angled_optional(Parser* parser, T production(Parser*),
                           std::string m) {
      return require_enclosed_optional(parser, production,
                                       token::less_tok, m);
   }

   template<typename T>
   static inline T
   require_bracketed(Parser* parser, T production(Parser*), std::string m) {
      return require_enclosed(parser, production, token::open_bracket_tok, m);
   }
   
   // Sequence parser combinator: parse `production' zero or more times.
   template<typename T>
   static Sequence<T>
   zero_or_more(Parser* parser, const T* production(Parser*)) {
      Sequence<T> seq;
      while (const T* ast = production(parser))
         seq.push_back(ast);
      return seq;
   }

   // Subroutine of indented.
   // Check that an indented single line is properly ended by
   // an unindentation or end of file.
   static void
   check_indented_line_end(Parser* parser, const Token* indent) {
      if (at_eoi(parser->context()))
         return;
      auto unindent = require(parser, token::unindent_tok);
      if (column(*indent) <= column(*unindent))
         parse_error(parser, "misplaced indentation");
   }

   // Parse a single indented line of syntactic form `prod'..
   template<typename T>
   static T
   indented(Parser* parser, T (*prod)(Parser*)) {
      auto start = parser->context()->cursor;
      auto indent = match(parser->context(), token::indent_tok);
      if (indent == nullptr)
         return nullptr;
      else if (auto x = prod(parser)) {
         check_indented_line_end(parser, indent);
         return x;
      }
      parser->context()->cursor = start;
      return nullptr;
   }

   // Same as indented, except that the indentation may be missing.
   template<typename T>
   static T
   possibly_indented(Parser* parser, T (*prod)(Parser*)) {
      if (layout_line_count(parser) == 1)
         return indented(parser, prod);
      return prod(parser);
   }

   // Sanity check the layout ending of a pile starting a column `col'.
   static bool
   at_pile_end(Parser* parser, input::ColumnNumber col) {
      auto ctx = parser->context();
      if (skip_blank(ctx->cursor, ctx->last) == ctx->last)
         return true;
      if (ctx->cursor->kind == token::unindent_tok
          and column(*ctx->cursor) < col) {
         ++ctx->cursor;
         return true;
      }
      return false;
   }

   template<typename T>
   static Sequence<T>
   pile(Parser* parser, const T* rule(Parser*)) {
      auto indent = require(parser, token::indent_tok);
      const auto c = column(*indent);
      Sequence<T> seq;
      while (not at_pile_end(parser, c)) {
         if (not seq.empty())
            require_indentation_at(parser, c);
         if (auto x = rule(parser))
            seq.push_back(x);
         else
            break;
      }
      return seq;
   }

   // Parse a sequeence of comma-separared `production'.
   template<typename T>
   static Sequence<T>
   comma_separated(Parser* parser, const T* production(Parser*)) {
      Sequence<T> seq;
      do
         seq.push_back(must(parser, production, "parse error"));
      while (match(parser->context(), token::comma_tok));
      return seq;
   }

   // Parse a repeated application of operator that is left associative.
   template<int N, typename T>
   static const Ast*
   left_associative(Parser* parser, T rule(Parser*),
                    const token::Kind (&ops)[N]) {
      if (T expr = rule(parser)) {
         while (const OperatorAst* op =  parse_symbol(parser, ops))
            expr = parser->make_ast(op, expr,
                                    must(parser, rule,
                                         "expected and expression"));
         return expr;
      }
      return nullptr;
   }

   template<typename T>
   static const Ast*
   left_associative(Parser* parser, T rule(Parser*), token::Kind tok) {
      const token::Kind ops[] = { tok };
      return left_associative(parser, rule, ops);
   }

   // Parse a repeated application of operator that is right associative.
   template<int N>
   static const Ast*
   right_associative(Parser* parser, Production production,
                     const token::Kind (&ops)[N]) {
      if (const Ast* expr = production(parser)) {
         if (const OperatorAst* op = parse_symbol(parser, ops)) {
            const Ast* rhs = right_associative(parser, production, ops);
            if (rhs == nullptr)
               parser->error_at("missing right operand to ", *op->token());
            expr = parser->make_ast(op, expr, rhs);
         }
         return expr;
      }
      return nullptr;
   }

   static const Ast*
   right_associative(Parser* parser, Production prod, token::Kind tok) {
      const token::Kind ops[] = { tok };
      return right_associative(parser, prod, ops);
   }

   // -------------------------
   // -- Expressions parsers --
   // -------------------------

   // -- Parse a string literal.
   static const Ast*
   parse_string(Parser* parser) {
      if (auto t = match(parser->context(), token::literal_string_tok))
         return parser->make_literal(*t);
      return nullptr;
   }

   // Literal value token classes
   static constexpr token::Kind literal_tokens[] = {
      token::literal_integer_tok,
      token::literal_real_tok, 
      token::literal_boolean_tok,
      token::literal_character_tok,
      token::literal_string_tok,
      token::literal_ipv4_tok,
      token::literal_hex_tok
   };

   // Parse a literal value in the input source.
   // literal ::= boolean | character | integer | real | string
   static const LiteralAst*
   parse_literal(Parser* parser) {
      if (const Token* t = match_one(parser, literal_tokens))
         return parser->make_literal(*t);
      return nullptr;
   }

   // -- Operator names.

   // Suffix operartor token kind.
   static constexpr token::Kind suffix_ops[] = {
      token::exclamation_tok
   };

   // Exponentiation token kind.
   static constexpr token::Kind exponent_ops[] = {
      token::caret_tok
   };

   // Prefix operator token types
   static constexpr token::Kind prefix_ops[] = {
      token::minus_tok,
      token::not_tok,
      token::tilde_tok,
      token::hash_tok,
      token::key_tok
   };

   //  Multiplicative operator token kinds.
   static constexpr token::Kind mult_ops[] = {
      token::star_tok, token::slash_tok,
      token::div_tok, token::quo_tok,
      token::mod_tok, token::rem_tok
   };
   
   // Additive operator token kinds.
   static constexpr token::Kind additive_ops[] = {
      token::plus_tok,
      token::minus_tok
   };

   // Relational operator token kinds.
   static constexpr token::Kind relational_ops[] = {
      token::less_tok, 
      token::less_equal_tok,
      token::greater_tok, 
      token::greater_equal_tok
   };

   // Equality comparison operator token kinds.
   static constexpr token::Kind equality_ops[] = {
      token::double_equal_tok,
      token::not_equal_tok
   };

   // Logicaal operator token kinds.
   static constexpr token::Kind logical_ops[] = {
      token::and_tok,
      token::or_tok,
      token::implies_tok
   };

   // Interval operator token kind.
   static constexpr token::Kind interval_ops[] = {
      token::dot_dot_tok
   };

   // Return true if `k' designates a class of binary operator.
   static bool
   binary_operator(token::Kind k) {
      return member(k, exponent_ops) or member(k, mult_ops)
         or member(k, additive_ops) or member(k, relational_ops)
         or member(k, equality_ops) or member(k, logical_ops)
         or member(k, interval_ops);
   }

   // Return true if `k' designates a class of suffix or binary operator.
   static bool
   suffix_or_binary_operator(token::Kind k) {
      return member(k, suffix_ops) or binary_operator(k);
   }

   // Return true if `k' designates a class of prefix, suffix,
   // or binary operator.
   static bool
   any_operator(token::Kind k) {
      return member(k, prefix_ops) or suffix_or_binary_operator(k);
   }

   // -- Parse the name of an operator
   //   operator-name ::= "(" operator ")"
   static const Ast*
   parse_operator_name(Parser* parser) {
      if (staring_at(parser, token::open_paren_tok)
          and nth_token_is(parser->context(), 1, any_operator)
          and nth_token_is(parser->context(), 2, token::close_paren_tok)) {
         const Token* op = advance(parser->context());
         advance(parser->context(), 2);
         return parser->make_operator(*op);
      }
      return nullptr;
   }

   // -- Parse an identifier.
   static const IdentifierAst*
   parse_identifier(Parser* parser) {
      if (auto t = match(parser->context(), token::identifier_tok))
         return parser->make_identifier(*t);
      return nullptr;
   }

   // -- Must parse an identifier.
   static const IdentifierAst*
   parse_identifier_or_else(Parser* parser) {
      return must(parser, parse_identifier, "expected identifier");
   }

   // -- Parse a name.
   //   name ::= identifier | operator-name
   static const Ast*
   parse_name(Parser* parser) {
      if (const Ast* x = parse_operator_name(parser))
         return x;
      return parse_identifier(parser);
   }

   // Basic type token classes
   static constexpr token::Kind basic_type_tokens[] = {
      token::bool_tok, token::byte_tok,
      token::char_tok, token::int_tok, token::double_tok,
      token::string_tok,
      token::type_tok, token::prop_tok, token::axiom_tok,
      token::concept_tok, token::namespace_tok
   };
   
   // Parse a basic type.
   //    basic_type ::= "bool" | "byte" | "char" | "int" 
   //                 | "double" | "string" | "type" | "auto"
   //                 | "prop" | "namespace" | "concept"
   static const IdentifierAst*
   parse_basic_type(Parser* parser) {
      if (const Token* t = match_one(parser, basic_type_tokens))
         return parser->make_identifier(*t);
      return nullptr;
   }

   static const Ast* parse_nontag_type(Parser*);

   // -- Parse an enclosed comma separated expression sequence
   //   enclosure ::= "(" [ enclosed ] ")"
   //               | "[" [ enclosed ] "]"
   //               | "{" [ enclosed ] "}"
   //               | "[|" [ enclosed ] "|]"
   static constexpr token::Kind enclose_ops[] = {
      token::open_paren_tok,
      token::open_bracket_tok,
      token::open_brace_tok,
      token::open_mbracket_tok
   };

   static const Ast* parse_enclosed(Parser*, const Token*);

   // Finish parsing the maching bracket of an enclosure syntax.
   static const EnclosureAst*
   finish_enclosure(Parser* parser, const Ast* x, const Token* t) {
      const Token* u = require(parser, closer(t->kind));
      return parser->make_enclosure(parser->make_bracket(*t, *u), x);
   }
   
   static const EnclosureAst*
   parse_enclosure(Parser* parser) {
      if (const Token* open = match_one(parser, enclose_ops)) {
         if (auto close = match(parser->context(), closer(open->kind)))
            return parser->make_enclosure
               (parser->make_bracket(*open, *close), nullptr);
         return finish_enclosure(parser, parse_enclosed(parser, open), open);
      }
      return nullptr;
   }

   static const Ast* parse_parenthesis(Parser*);
   static const Ast* parse_arrow(Parser*);
   static const Ast* parse_statement(Parser*);

   // Parse a compound statement
   //   compound ::= pile(statement)
   static const CompoundAst*
   parse_compound(Parser* parser) {
      auto stmts = pile(parser, parse_statement);
      return parser->make_compound(stmts);
   }


   // Parse an elementary expression.
   //   elementary ::= literal | basic_type | name
   static const Ast*
   parse_elementary(Parser* parser) {
      if (at_eoi(parser->context()))
         return nullptr;
      else if (auto ast = parse_literal(parser))
         return ast;
      else if (auto ast = parse_basic_type(parser))
         return ast;
      return parse_name(parser);
   }

   // Parse a primary expression.
   //   primary ::= elementary | parenthesis | enclosure | compound
   static const Ast*
   parse_primary(Parser* parser) {
      if (auto ast = parse_elementary(parser))
         return ast;
      else if (auto ast = parse_parenthesis(parser))
         return ast;
      else if (layout_line_count(parser) > 1)
         return parse_compound(parser);
      return parse_enclosure(parser);
   }

   // -- Parse a field selector.
   //   selector ::= "." elementary
   static const Ast*
   parse_selector(Parser* parser) {
      if (not match(parser->context(), token::dot_tok))
         return nullptr;
      return must(parser, parse_elementary, "expected elementary expression");
   }
   
   static constexpr token::Kind definable_braces[] = {
      token::open_brace_tok, token::open_bracket_tok
   };

   static const ParameterAst* parse_parameter(Parser*);

   // -- Parse the tail of a bracketted form:
   //    bracketted-form-tail ::= [ parameter-list ] ("]" | "}")
   static const ClosedForm*
   parse_closed_form_tail(Parser* parser, const Token& t1) {
      Parameters parms;
      if (not staring_at(parser, closer(t1.kind)))
         parms = comma_separated(parser, parse_parameter);
      const Token* t2 = require(parser, closer(t1.kind));
      return parser->make_closed_form(parser->make_bracket(t1, *t2), parms);
   }

   static const Ast* parse_implication(Parser*);

   // -- Parse the restriction at the end of a call form.
   //
   static const Ast*
   parse_call_form_tail_restriction(Parser* parser) {
      if (match(parser->context(), token::bar_tok))
         return parse_implication(parser);
      return nullptr;
   }

   // -- Parse the comma-separated parameter list and closing parenthesis
   // -- of a call form.
   //    call-form-tail ::= [ parameter { "," parameter } ] ")" [ restriction ]
   static const CallForm*
   parse_call_form_tail(Parser* parser, const Token& id) {
      Parameters parms;
      if (not match(parser->context(), token::close_paren_tok)) {
         parms = comma_separated(parser, parse_parameter);
         require(parser, token::close_paren_tok);
      }
      auto restriction = parse_call_form_tail_restriction(parser);
      return parser->make_call_form(id, parms, restriction);
   }

   // A suffix operator token 'o' follows a parameter 'x'; return the AST
   // for the corresponding suffix form.
   static const SuffixForm*
   finish_suffix_form(Parser* parser, const ParameterAst* x, const Token& o) {
      auto op = parser->make_operator(o);
      return parser->make_suffix_form(op, x);
   }

   // Build a bare parameter AST for an identifier
   static const ParameterAst*
   make_parameter(Parser* parser, const IdentifierAst* id) {
      return parser->make_parameter(id, nullptr);
   }

   // Likewise, but given the identifier token.
   static const ParameterAst*
   make_parameter(Parser* parser, const Token& t) {
      return make_parameter(parser, parser->make_identifier(t));
   }

   // A suffix operator 'op' follwos a bare parameter 'id'; finish the AST
   // for the corresponding suffix form.
   static const SuffixForm*
   finish_suffix_form(Parser* parser, const Token& id, const Token& op) {
      return finish_suffix_form(parser, make_parameter(parser, id), op);
   }

   static const ParameterAst* parse_named_parameter(Parser*);

   // -- Parser an isolated parameter in a definiendum.
   //    isolated-parameter ::= "(" named-parameter ")"
   static const ParameterAst*
   parse_isolated_parameter(Parser* parser) {
      if (not match(parser->context(), token::open_paren_tok))
         return nullptr;
      auto parm = parse_named_parameter(parser);
      require(parser, token::close_paren_tok);
      return parm;
   }

   // -- Parse the tail of a prefix notation in a declaration form
   //     prefix-form-tail ::= prefix-operator [ identifier | isolated-parameterast ]

   static const FixityForm*
   finish_prefix_form(Parser* parser, const ParameterAst* x, const Token& o) {
      auto op = parser->make_operator(o);
      return parser->make_prefix_form(op, x);
   }

   static const FixityForm*
   parse_prefix_form_tail(Parser* parser, const Token& t) {
      if (auto parm = parse_isolated_parameter(parser))
         return finish_prefix_form(parser, parm, t);
      else if (auto id = parse_identifier(parser))
         return finish_prefix_form(parser, make_parameter(parser, id), t);
      return parser->make_operator_form(t);
   }

   // -- Parse the tail of a infix notation, given a parameter and the
   // -- the binary operator token.

   static const InfixForm*
   finish_infix_form(Parser* parser, const ParameterAst* x, const Token& o) {
      auto op = parser->make_operator(o);
      if (auto y = parse_isolated_parameter(parser))
         return parser->make_infix_form(op, x, y);
      auto y = make_parameter(parser, parse_identifier_or_else(parser));
      return parser->make_infix_form(op, x, y);
   }

   static const InfixForm*
   finish_infix_form(Parser* parser, const Token& t, const Token& o) {
      return finish_infix_form(parser, make_parameter(parser, t), o);
   }

   // -- Parse the definiendum in a declarative form.
   //    definiendum ::= literal | identifier [ "(" parameter-list ")" ]
   //                  | prefix-operator [ single-parameter ]
   //                  | single-parameter ( suffix-operator
   //                                       | binary-operator single-parameter)
   //                  | suffix-or-binary-operator
   //                  | "{" [ parameter-list ] "}"
   //                  | "[" [ parameter-list ] "]"
   static const FixityForm*
   parse_definiendum(Parser* parser) {
      if (const Token* t = match_one(parser, literal_tokens))
         return parser->make_literal_form(*t);
      else if (auto t = match_one(parser, prefix_ops))
         return parse_prefix_form_tail(parser, *t);
      else if (auto t = match(parser->context(), token::identifier_tok)) {
         if(match(parser->context(), token::open_paren_tok))
            return parse_call_form_tail(parser, *t);
         else if (auto op = match_one(parser, suffix_ops))
            return finish_suffix_form(parser, *t, *op);
         else if (auto op = match_if(parser, binary_operator))
            return finish_infix_form(parser, *t, *op);
         return parser->make_alphabetic_form(*t);
      }
      else if (auto x = parse_isolated_parameter(parser)) {
         if (auto op = match_one(parser, suffix_ops))
            return finish_suffix_form(parser, x, *op);
         auto op = match_if(parser, binary_operator);
         if (op == nullptr)
            parse_error(parser, "binary of suffix operator expected");
         return finish_infix_form(parser, x, *op);
      }
      else if (auto t = match_if(parser, suffix_or_binary_operator))
         // Note: prefix operators are handled early.
         return parser->make_operator_form(*t);
      else if (const Token* t = match_one(parser, definable_braces))
         return parse_closed_form_tail(parser, *t);
      return nullptr;
   }

   // --
   //   specification ::= definiendum ":" nontag-type [ ":=" arrow ]
   static const SignatureAst*
   parse_signature(Parser* parser) {
      auto form = parse_definiendum(parser);
      if (form == nullptr)
         return nullptr;
      require(parser, token::colon_tok);
      const Ast* type = parse_nontag_type(parser);
      if (match(parser->context(), token::colon_equal_tok)) {
         auto impl = must(parser, parse_arrow,
                          "expected constructor implementation");
         return parser->make_signature(form, type, impl);
      }
      return parser->make_signature(form, type);
   }

   static Sequence<SignatureAst>
   parse_signature_list(Parser* parser) {
      return comma_separated(parser, parse_signature);
   }

   // --
   //   specification-block ::= pile(signature) | "{" { signature } "}"
   static Sequence<SignatureAst>
   parse_signature_block(Parser* parser) {
      if (layout_line_count(parser) > 1)
         return pile(parser, parse_signature);
      require(parser, token::open_brace_tok);
      if (match(parser->context(), token::close_brace_tok))
         return { };
      Sequence<SignatureAst> sigs = parse_signature_list(parser);
      require(parser, token::close_brace_tok);
      return sigs;
   }

   static constexpr token::Kind data_ops[] = {
      token::record_tok, token::variant_tok
   };

   // -- Parse a datatype expression
   //   datatype ::= data-op signature-block
   //   data-op ::= "record" | "variant"
   static const Ast*
   parse_datatype(Parser* parser) {
      if (const Token* t = match_one(parser, data_ops))
         return parser->make_datatype(*t, parse_signature_block(parser));
      return nullptr;
   }

   // --
   //   assertion ::= "assert" "(" arrow ")"
   static const AssertAst*
   parse_assertion(Parser* parser) {
      if (auto t = match(parser->context(), token::assert_tok)) {
         require(parser, token::open_paren_tok);
         auto p = parse_arrow(parser);
         require(parser, token::close_paren_tok);
         return parser->make_assert(*t, p);
      }
      return nullptr;
   }

   // Parse a postfix expression
   //   postfix ::= ( primary | assertion )
   //                  { selector | enclosure } [ "@" nontag-type ]
   //             | datatype
   static const Ast*
   parse_postfix(Parser* parser) {
      const Ast* expr = parse_assertion(parser);
      if (expr == nullptr)
         expr = parse_primary(parser);
      if (expr == nullptr)
         return parse_datatype(parser);

      bool keep_going = true;
      while (keep_going) {
         if (const Ast* x = parse_selector(parser))
            expr = parser->make_dot(expr, x);
         else if (const EnclosureAst* x = parse_enclosure(parser))
            expr = parser->make_apply(expr, x);
         else
            keep_going = false;
      }
      if (match(parser->context(), token::at_tok))
         return parser->make_restrict(expr, parse_nontag_type(parser));
      return expr;
   }

   // --
   //    suffix ::= postfix { "!" }
   static const Ast*
   parse_suffix(Parser* parser) {
      if (const Ast* x = parse_postfix(parser)) {
         while (auto op = parse_symbol(parser, token::exclamation_tok))
            x = parser->make_ast(op, x);
         return x;
      }
      return nullptr;
   }

   // -- Parse an exponentiation expression
   //   exponentiation ::= suffix { "^" suffix }
   static const Ast*
   parse_exponentiation(Parser* parser) {
      return right_associative(parser, parse_suffix, token::caret_tok);
   }
   
   // -- Parse a non-parenthesized application expression
   //    juxtaposition ::= exponentiation
   //                    | exponentiation juxtaposition
   static const Ast*
   parse_juxtaposition(Parser* parser) {
      if (const Ast* x = parse_exponentiation(parser)) {
         if (const Ast* y = parse_juxtaposition(parser))
            return parser->make_juxtapose(x, y);
         return x;
      }
      return nullptr;
   }

   // Parse a const-qualified type.
   // Note: This deviates from the EoP grammar in an attempt to
   // fix a grammar issue with reference to const-qualified types.
   //     qualified ::= [ "const" ] juxtaposition
   static const Ast*
   parse_qualified(Parser* parser) {
      if (const OperatorAst* qual = parse_symbol(parser, token::const_tok)) {
         const Ast* type = must(parser, parse_juxtaposition,
                                "type expression expected");
         return parser->make_ast(qual, type);
      }
      return parse_juxtaposition(parser);
   }

   static const Ast*
   parse_prefix_operand(Parser* parser) {
      return must(parser, parse_qualified, "expected a postfix expression");
   }

   // Parse a prefix expression
   //   prefix ::= [ "-" | "not" | "~" | "#" ] qualified
   //            | qualified [ "&" ]
   static const Ast*
   parse_prefix(Parser* parser) {
      if (const OperatorAst* op = parse_symbol(parser, prefix_ops))
         return parser->make_ast(op, parse_prefix_operand(parser));
      const Ast* x = parse_qualified(parser);
      if (x == nullptr)
         return nullptr;
      if (const OperatorAst* op = parse_symbol(parser, token::ampersand_tok))
         return parser->make_ast(op, x);
      return x;
   }

   // Parse a multiplicative expression
   //   multiplicative ::=  { prefix mult-ops } prefix
   //   mult-ops ::= "*" | "/" | "div" | "quo" | "mod" | "rem"
   static const Ast*
   parse_multiplicative(Parser* parser) {
      return left_associative(parser, parse_prefix, mult_ops);
   }

   // Parse an additive expression.
   //   additive ::= multiplicative { ( "+" | "-" ) multiplicative }
   static const Ast*
   parse_additive(Parser* parser) {
      return left_associative(parser, parse_multiplicative, additive_ops);
   }

   // -- Parse an interval expression
   //   interval ::= additive [ ".." [ additive ] ]
   static const Ast*
   parse_interval(Parser* parser) {
      if (const Ast* x = parse_additive(parser)) {
         if (match(parser->context(), token::dot_dot_tok))
            x = parser->make_interval(x, parse_additive(parser));
         return x;
      }
      return nullptr;
   }
   
   // Parse a relational comparison expression
   //   relational ::= interval { ( "<" | "<=" | ">" | ">=" ) interval }
   static const Ast*
   parse_relational(Parser* parser) {
      return left_associative(parser, parse_interval, relational_ops);
   }

   // Parse an equality comparison expression
   //   equality ::= { relational ("==" | "!=") } relational
   static const Ast*
   parse_equality(Parser* parser) {
      return left_associative(parser, parse_relational, equality_ops);
   }

   // Conjuction operators
   static const token::Kind and_ops[] = {
      token::and_tok, token::meet_tok
   };

   // Parse a conjunctive expression
   //   conjunction ::= equality { ("and" | "/\" equality }
   static const Ast*
   parse_conjunction(Parser* parser) {
      return right_associative(parser, parse_equality, and_ops);
   }

   static const Ast* parse_exit(Parser*);
   
   // Disjunction operators
   static const token::Kind or_ops[] = {
      token::or_tok, token::join_tok
   };

   // -- Parse a disjunction expression
   //   disjunction ::= conjunction { ( "or" | "\/" ) conjunction }
   //                 | conjunction [ "or" exit ]
   static const Ast*
   parse_disjunction(Parser* parser) {
      auto x = parse_conjunction(parser);
      if (x == nullptr)
         return x;
      if (auto op = parse_symbol(parser, or_ops)) {
         auto y = parse_exit(parser);
         if (y == nullptr)
            y = must(parser, parse_disjunction, "unfinished disjunction");
         return parser->make_ast(op, x, y);
      }
      return x;
   }
   
   static const Ast* parse_condition(Parser*);
   
   // --  Finish parsing the restricting condition (if any) of the
   //     core type `t'.
   static const Ast*
   finish_restriction(Parser* parser, const Ast* t) {
      if (match(parser->context(), token::bar_tok))
         t = parser->make_filter(t, parse_condition(parser));
      return t;
   }

   // -- Implication and equivalent operations
   static constexpr token::Kind imp_ops[] = {
      token::implies_tok, token::equiv_tok
   };

   // -- Parse an implication expression
   //    implication ::= { disjunction ("=>" | "<=>") } disjunction
   //                      [ "|" condition ]
   static const Ast*
   parse_implication(Parser* parser) {
      if (auto x = left_associative(parser, parse_disjunction, imp_ops))
         return finish_restriction(parser, x);
      return nullptr;
   }

   // -- Parse an arrow expression
   //    arrow ::= implication { "->" implication }
   static const Ast*
   parse_arrow(Parser* parser) {
      if (auto x = parse_implication(parser)) {
         if (match(parser->context(), token::right_arrow_tok)) {
            auto y = must(parser, parse_arrow, "malformed target");
            x = parser->make_arrow(x, y);
         }
         return x;
      }
      return nullptr;
   }

   static Sequence<DeclarativeAst> parse_locals(Parser*);

   // -- Parse a sequence of local definitions
   //   where-clause ::= "where" locals
   static Sequence<DeclarativeAst>
   parse_where_clause(Parser* parser) {
      if (flexible_match(parser, token::where_tok))
         return parse_locals(parser);
      return { };
   }

   // Parse a general expression
   //   where ::= arrow [ where-clause ]
   static const Ast*
   parse_where(Parser* parser) {
      if (const Ast* x = parse_arrow(parser)) {
         if (auto decls = parse_where_clause(parser))
            return parser->make_where(x, decls);
         return x;
      }
      return nullptr;
   }

   // -- Parse an exit expression
   //   exit ::= ( "throw" | "return" | "leave" ) [ where ]
   static const Ast*
   parse_exit(Parser* parser) {
      if (auto t = match(parser->context(), token::return_tok))
         return parser->make_return(*t, parse_where(parser));
      else if (auto t = match(parser->context(), token::leave_tok))
         return parser->make_leave(*t, parse_where(parser));
      else if (auto t = match(parser->context(), token::throw_tok))
         return parser->make_throw(*t, parse_where(parser));
      return nullptr;
   }

   static constexpr token::Kind quantifier_ops[] = {
      token::forall_tok, token::exists_tok
   };
   // -- Parse a logical quantifier.
   //    quantifier ::= "forall" | "exists"
   static const OperatorAst*
   parse_quantifier(Parser* parser) {
      return parse_symbol(parser, quantifier_ops);
   }

   // --
   //   named-parameter ::= identifier ":" nontag-type
   static const ParameterAst*
   parse_named_parameter(Parser* parser) {
      auto n = parse_identifier_or_else(parser);
      require(parser, token::colon_tok);
      return parser->make_parameter(n, parse_nontag_type(parser));
   }

   static Sequence<ParameterAst>
   parse_named_parameter_list(Parser* parser) {
      return comma_separated(parser, parse_named_parameter);
   }

   static Sequence<ParameterAst>
   parse_parenthesized_named_parameter_list(Parser* parser) {
      return require_enclosed
         (parser, parse_named_parameter_list, token::open_paren_tok,
          "named parameter list expcted");
   }

   struct QuantifierHeader {
      QuantifierHeader() : quant(), parms(), guard() { }
      QuantifierHeader(const OperatorAst* q, const Sequence<ParameterAst>& p,
                       const Ast* r)
            : quant(q), parms(p), guard(r) { }
      const OperatorAst* quantifier() const { return quant; }
      const Sequence<ParameterAst>& parameters() const { return parms; }
      const Ast* constraint() const { return guard; }
      explicit operator bool() const { return quant; }
   private:
      const OperatorAst* quant;
      Sequence<ParameterAst> parms;
      const Ast* guard;
   };

   template <typename T>
      static const T*
      optional_indent(Parser* parser, const T* rule(Parser*)) {
         if (match(parser->context(), token::justify_tok)) {
            auto ast = rule(parser);
            require(parser, token::justify_tok);
            return ast;
         } else if (auto indent = match(parser->context(), token::indent_tok)) {
            auto ast = rule(parser);
            auto unindent = match(parser->context(), token::unindent_tok);
            if (unindent == nullptr)
               parse_error(parser, "missing unindent");
            require(parser, token::justify_tok);
            return ast;
         } else
            return rule(parser);
      }

   static const Ast*
   do_parse_constraint(Parser* parser) {
      auto constraint = parse_implication(parser);
      require(parser, token::comma_tok);
      return constraint;
   }

   static const Ast*
   parse_constraint(Parser* parser) {
      if (match(parser->context(), token::dot_tok))
         return optional_indent(parser, do_parse_constraint);
      else
         require(parser, token::comma_tok);
      return nullptr;
   }

   // -- Parse a quantifier header
   //   quantifier-header ::= quantifier parameter-list [, constraint]
   static QuantifierHeader
   parse_quantifier_header(Parser* parser) {
      if (auto q = parse_quantifier(parser)) {
         auto parms = parse_parenthesized_named_parameter_list(parser);
         auto constraint = parse_constraint(parser);
         match(parser->context(), token::justify_tok);
         return { q, parms, constraint };
      }
      return { };
   }

   static const Ast* parse_type(Parser*);

   // Parse a quantified expression.  Note that the body of a quantified
   // can have tag-types.
   static const Ast*
   do_parse_quantified(Parser* parser) {
      if (const OperatorAst* q = parse_quantifier(parser)) {
         auto parms = parse_parenthesized_named_parameter_list(parser);
         auto constraint = parse_constraint(parser);
         if (layout_line_count(parser) == 0)
            require(parser, token::comma_tok);
         auto body = do_parse_quantified(parser);
         return parser->make_quantified(q, parms, constraint, body);
      }
      return parse_type(parser);
   }

   // -- Parse a quantified expression
   //   quantified ::= quantifier parameter-list "," type | where
   static const Ast*
   parse_quantified(Parser* parser) {
      if (staring_at(parser, quantifier_ops))
         return do_parse_quantified(parser);
      return parse_where(parser);
   }

   // Parse a condition in if/while/switch/such that statements.
   //   condition ::= disjunction
   static const Ast*
   parse_condition(Parser* parser) {
      return must(parser, parse_disjunction, "invalid condition");
   }

   // -- Parse a tag-type
   //   tag-type ::= identifier ":" instantiation
   static const ParameterAst*
   parse_tag_type(Parser* parser) {
      if (staring_at(parser, token::identifier_tok)
          and next_token_is(parser->context(), token::colon_tok))
         return parse_named_parameter(parser);
      return nullptr;
   }

   // -- Parse a nontag-type expression
   //   nontag-type ::= quantified-type | arrow
   static const Ast*
   parse_nontag_type(Parser* parser) {
      if (const Ast* x = parse_quantified(parser))
         return x;
      return must(parser, parse_arrow, "invalid type expression");
   }

   // --
   //   type ::= tag-type | nontag-type
   static const Ast*
   parse_type(Parser* parser) {
      if (auto x = parse_tag_type(parser))
         return x;
      return parse_nontag_type(parser);
   }
   
   // Parse a procedure parameter
   //   parameter ::= type
   static const ParameterAst*
   parse_parameter(Parser* parser) {
      if (auto t = parse_tag_type(parser))
         return t;
      return parser->make_parameter(nullptr, parse_nontag_type(parser));
   }
   
   // -- List of all operator kinds.
   static constexpr token::Kind binary_ops[] = {
      token::or_tok, token::and_tok,                 // logical
      token::double_equal_tok, token::not_equal_tok, // equality
      token::less_tok, token::less_equal_tok,        // relational
      token::greater_tok, token::greater_equal_tok,
      token::plus_tok, token::minus_tok,             // additive
      token::star_tok, token::slash_tok, token::percent_tok, // multiplicative
      token::caret_tok                               // exponentiation
   };

   // -- Parse a binary operator, e.g. as seen in sections.
   static const OperatorAst*
   parse_binary_operator(Parser* parser) {
      return parse_symbol(parser, binary_ops);
   }

   // -- Determine the proper parser for the operand of a right section.
   static Production
   get_right_operand_parser(const OperatorAst* x) {
      switch (x->token()->kind) {
      case token::or_tok:
         return parse_conjunction;
      case token::and_tok:
         return parse_equality;

      case token::double_equal_tok:
      case token::not_equal_tok:
         return parse_relational;

      case token::less_tok:
      case token::less_equal_tok:
      case token::greater_tok:
      case token::greater_equal_tok:
         return parse_additive;

      case token::plus_tok:
      case token::minus_tok:
         return parse_multiplicative;

      case token::star_tok:
      case token::slash_tok:
      case token::percent_tok:
         return parse_prefix;

      case token::caret_tok:
         return parse_exponentiation;

      default:
         return nullptr;
      }
   }

   // -- Parse the operand of a right section.  Note that substraction
   // -- conflicts with unary negation, so is not available for
   // -- constructing right sections.  In order words (-1) always
   // -- means the negation of 1,
   static const Ast*
   parse_right_section_operand(Parser* parser, const OperatorAst* x) {
      if (x->token()->kind == token::minus_tok)
         return parser->make_ast(x, parse_prefix_operand(parser));
      else if (Production prod = get_right_operand_parser(x))
         return parser->make_right_section
            (x, must(parser, prod, "expected operand"));
      parse_error(parser, "invalid section syntax");
      return nullptr;
   }

   // -- Parse syntactic objects in parenthesis
   //   parentheis ::= "(" binary-operator ")"
   //                | right-section
   //                | "(" enclosed ")"
   static const Ast*
   parse_parenthesis(Parser* parser) {
      if (auto o = match(parser->context(), token::open_paren_tok)) {
         if (const OperatorAst* x = parse_binary_operator(parser)) {
            if (match(parser->context(), token::close_paren_tok))
               return parser->make_bisection(x);
            const Ast* y = parse_right_section_operand(parser, x);
            require(parser, token::close_paren_tok);
            return y;
         }
         else if (auto c = match(parser->context(), token::close_paren_tok))
            return parser->make_enclosure
               (parser->make_bracket(*o, *c), nullptr);
         const Ast* x = parse_enclosed(parser, o);
         return finish_enclosure(parser, x, o);
      }
      return nullptr;
   }

   // -- Parse a description
   //    description ::= pile of "description"
   static const DescriptionAst*
   parse_description(Parser* parser) {
      if (auto t = match(parser->context(), token::description_tok)) {
         vector<Token> ts(1, *t);
         while (auto t2 = next_token_is(parser->context(), token::description_tok)) {
            if (column(*t) != column(*t2))
               break;
            ts.push_back(*t2);
            advance(parser->context());
         }
         return parser->make_description(ts);
      }
      return nullptr;
   }

   // -----------------------
   // -- Statement parsers --
   // -----------------------

   static const Ast*
   parse_case_pattern(Parser* parser) {
      if (auto x = parse_literal(parser))
         return x;
      else if (auto name = parse_identifier(parser)) {
         if (match(parser->context(), token::colon_tok))
            return parser->make_parameter(name, parse_type(parser));
         else if (auto x = parse_enclosure(parser))
            return parser->make_apply(name, x);
         else if (current_token(parser->context())->kind == token::implies_tok)
            return name;
      }
      return parse_type(parser);
   }

   // Parse a case statement
   //   case ::= "case" parameter "=>" statement
   static const CaseAst*
   parse_case(Parser* parser) {
      if (match(parser->context(), token::case_tok)) {
         const Ast* key = parse_case_pattern(parser);
         require(parser, token::implies_tok);
         const Ast* stmt = must(parser, parse_statement, "missing statement");
         return parser->make_case(key, stmt);
      }
      return nullptr;
   }

   static const CaseAst*
   parse_case_or_else(Parser* parser) {
      auto x = parse_case(parser);
      if (x == nullptr)
         parse_error(parser, "expected case statement");
      return x;
   }

   // Parse a sequence of case statements.
   //   case-pile ::= pile(case)
   static Sequence<CaseAst>
   parse_case_pile(Parser* parser) {
      switch (layout_line_count(parser)) {
      case 0:
         return { parse_case_or_else(parser) };
      case 1:
         return { indented(parser, parse_case_or_else) };
      default:
         return pile(parser, parse_case);
      }
   }

   // Parse a switch-statement.
   //   match ::= "match" arrow "with" case-pile
   static const Ast*
   parse_match(Parser* parser) {
      if (not match(parser->context(), token::match_tok))
         return nullptr;
      const Ast* expr = must(parser, parse_arrow, "missing scrutinee");
      require(parser, token::with_tok);
      Sequence<CaseAst> body = parse_case_pile(parser);
      return parser->make_match(expr, body);
   }

   // Subroutine of parse_conditional.
   // As a special case, `then' and `else' in conditional expressions
   // are allowed to on a newline by themselves, as long as they are
   // indent as far as the introducing `if' token.
   static const Token*
   require_then(Parser* parser, input::Column c) {
      if (auto t = flexible_match(parser, token::then_tok))
         return t;
      else if (match_justify_at(parser->context(), c))
         return require(parser, token::then_tok);
      parse_error(parser, "malformed if-statement");
      return nullptr;
   }

   // Subroutine of parse_else.  See require_then.
   static const Token*
   match_else(Parser* parser, input::Column c) {
      if (auto t = match(parser->context(), token::else_tok))
         return t;
      auto ctx = parser->context();
      auto cur = ctx->cursor;
      if (cur->kind != token::justify_tok
          or skip_blank(++cur, ctx->last) == ctx->last
          or cur->kind != token::else_tok)
         return nullptr;
      if (column(*cur) != c)
         parse_error(parser, "misindented 'else' branch");
      ctx->cursor = cur;
      return &*ctx->cursor++;
   }

   // Subroutine of parse_conditional.
   // Determine acceptable indentation for `else' in a conditional
   // expression based on the source loci of `if' and `then' tokens.
   // See require_then.
   static input::ColumnNumber
   acceptable_else_indent(const Token* itok, const Token* ttok) {
      if (line_number(*ttok) == line_number(*itok))
         return column(*itok);
      return column(*ttok);
   }

   // Parse the possible else-branch of a conditional expression.
   static const Ast*
   parse_else(Parser* parser, input::ColumnNumber c) {
      if (not match_else(parser, c))
         return nullptr;
      return must(parser, parse_statement,
                  "missing statement after 'else'");
   }
      
   // Parse an if-statement.
   //   conditional ::= "if" condition "then" statement
   //                   [ "else" statement ]
   static const Ast*
   parse_conditional(Parser* parser) {
      auto itok = match(parser->context(), token::if_tok);
      if (itok == nullptr)
         return nullptr;
      const Ast* e = parse_condition(parser);
      auto ttok = require_then(parser, column(*itok));
      const Ast* s = must(parser, parse_statement, "missing statement");
      const Ast* f = parse_else(parser, acceptable_else_indent(itok, ttok));
      return parser->make_if(e, s, f);
   }

   // -- Parse a for iterator
   //   for ::= "for" identifier "in" arrow
   static const ForIterator*
   parse_for_iterator(Parser* parser) {
      if (not match(parser->context(), token::for_tok))
         return nullptr;
      const IdentifierAst* v = parse_identifier_or_else(parser);
      require(parser, token::in_tok);
      const Ast* x = must(parser, parse_arrow, "missing sequence");
      return parser->make_for(v, x);
   }

   // -- Parser a loop iterator
   //   iterator ::= ( "while" | "until" | "|" ) condition
   //              | for
   static const Iterator*
   parse_iterator(Parser* parser) {
      if (match(parser->context(), token::until_tok))
         return parser->make_until(parse_condition(parser));
      else if (match(parser->context(), token::while_tok))
         return parser->make_while(parse_condition(parser));
      else if (match(parser->context(), token::bar_tok))
         return parser->make_proviso(parse_condition(parser));
      return parse_for_iterator(parser);
   }

   // -- Parse loop statement
   //   loop ::= { iterator } "repeat" statement
   static const Ast*
   parse_loop(Parser* parser) {
      const Sequence<Iterator> iters = zero_or_more(parser, parse_iterator);
      if (not iters.empty())
         require(parser, token::repeat_tok);
      else if (not match(parser->context(), token::repeat_tok))
         return nullptr;
      const Ast* body = must(parser, parse_statement, "missing loop body");
      return parser->make_repeat(iters, body);
   }
   
   // Parse a control statement.
   //   control-statement ::= exit | conditional | switch | loop | break
   static const Ast*
   parse_control_statement(Parser* parser) {
      if (const Ast* x = parse_exit(parser))
         return x;
      else if (const Ast* x = parse_conditional(parser))
         return x;
      else if (const Ast* x = parse_match(parser))
         return x;
      return parse_loop(parser);
   }
   
   // Return true if there is enough parameters are indicated by `arity'.
   static inline bool
   has_adequate_parameter_count(const Parameters& parms, int arity) {
      if (arity < 0)
         return true;
      return parms.size() == std::size_t(arity);
   }

   // -- Parse the tail of assignment statement.
   //   assignment-tail ::= ":=" where
   static const Ast*
   parse_assignment_tail(Parser* parser) {
      if (parse_symbol(parser, token::colon_equal_tok)) {
         auto rhs = parse_where(parser);
         if (rhs == nullptr)
            parse_error(parser, "missing right hand side in assignment");
         return rhs;
      }
      return nullptr;
   }

   // Parse either simple-statement
   //   simple-statement ::= quantified [ assignment-tail | where-clause ]
   static const Ast*
   parse_simple_statement(Parser* parser) {
      if (const Ast* expr = parse_quantified(parser)) {
         if (auto rhs = parse_assignment_tail(parser)) 
            expr = parser->make_assignment(expr, rhs);
         else if (auto decls = parse_where_clause(parser))
            expr = parser->make_where(expr, decls);
         return expr;
      }
      return nullptr;
   }

   // assumption ::= "assume" where
   static const Ast*
   parse_assumption(Parser* parser) {
      if (const OperatorAst* op = parse_symbol(parser, token::assume_tok)) {
         const Ast* prop = must(parser, parse_where, "assumption expected");
         return parser->make_ast(op, prop);
      }
      return nullptr;
   }

   // -- Attempt to complete the current syntactic object into
   //    a collect expression if it is followed by iterators.
   static const CollectAst*
   parse_collect_if_can(Parser* parser, const Ast* x) {
      Sequence<Iterator> iters = zero_or_more(parser, parse_iterator);
      if (not iters.empty())
         return parser->make_collect(iters, x);
      return nullptr;
   }

   //  enclosed ::= quantified iterators
   //             | quantified [ "," quantified-sequence ]
   static const Ast*
   parse_enclosed(Parser* parser, const Token* t) {
      const Ast* x = must(parser, parse_quantified, "missing expression");
      if (auto y = parse_collect_if_can(parser, x))
         return y;
      if (staring_at(parser, closer(t->kind)))
         return x;
      require(parser, token::comma_tok);
      AstSequence seq (1, x);
      seq.append(comma_separated(parser, parse_quantified));
      return parser->make_sequence(seq);
   }

   static const Ast*
   do_parse_rule_body(Parser* parser) {
      auto body = possibly_indented(parser, parse_quantified);
      if (body == nullptr)
         parse_error(parser, "malformed rule right-hand-side");
      return body;
   }

   // -- Parse a rule
   //   rule ::= "rule" name ":" nontag-type "=" quantified
   static const Ast*
   parse_rule(Parser* parser) {
      if (not match(parser->context(), token::rule_tok))
         return nullptr;
      auto head = parse_definiendum(parser);
      require(parser, token::colon_tok);
      const Ast* type = parse_nontag_type(parser);
      require(parser, token::equal_tok);
      const Ast* init = do_parse_rule_body(parser);
      return parser->make_rule(head, type, init);
   }

   // -- Parse a postulate statement
   //   simple_postulate ::= form ":" nontag-type
   static const Ast*
   parse_simple_postulate(Parser* parser) {
      auto head = parse_definiendum(parser);
      require(parser, token::colon_tok);
      auto type = must(parser, parse_nontag_type, "missing type");
      return parser->make_postulate(head, type);
   }

   // -- Parse an existential postulate
   //   ex_postulate ::= "exists" "(" identifier ":" identifier ")" ","
   //                    expression
   static const Ast*
   parse_existential_postulate(Parser* parser) {
      if (not match(parser->context(), token::exists_tok))
         return nullptr;
      require(parser, token::open_paren_tok);
      auto var = parse_identifier_or_else(parser);
      require(parser, token::colon_tok);
      auto type = parse_identifier_or_else(parser);
      require(parser, token::close_paren_tok);
      require(parser, token::comma_tok);
      auto constraint = parse_implication(parser);
      return parser->make_expostulate(var, type, constraint);
   }

   // -- Parse a postulate statement
   //   postulate ::= "postulate" form ":" nontag-type
   static const Ast*
   parse_postulate(Parser* parser) {
      if (match(parser->context(), token::postulate_tok)) {
         if (auto x = parse_existential_postulate(parser))
            return x;
         return parse_simple_postulate(parser);
      } else
         return nullptr;
   }

   // -- Parse an existential
   //   existential ::= "where" identifier "=" identifier
   static const Ast*
   parse_alias(Parser* parser) {
      if (match(parser->context(), token::where_tok)) {
         auto alias = parse_identifier_or_else(parser);
         require(parser, token::equal_tok);
         auto value = parse_arrow(parser);
         return parser->make_alias(alias, value);
      }
      return nullptr;
   }

   // Specification operator token class.
   static constexpr token::Kind spec_ops[] = {
      token::inductive_tok, token::coinductive_tok
   };

   // -- Parse a specification
   //   specification ::= spec-op signature-block
   //   spec-op ::=  "inductive" | "coinductive"
   static const Ast*
   parse_specification(Parser* parser) {
      if (const Token* t = match_one(parser, spec_ops)) {
         auto sigs = parse_signature_block(parser);
         return parser->make_datatype(*t, sigs);
      }
      return nullptr;
   }

   // -- Parse the right hand side of a definition.
   //   initializer ::= specification | statement
   static const Ast*
   parse_initializer(Parser* parser) {
      if (auto x = parse_specification(parser))
         return x;
      auto x = parse_statement(parser);
      if (x == nullptr)
         parse_error(parser, "missing initializer");
      return x;
   }

   // -- Parse a local declaration
   //      local-decl ::= head ":" type [ "=" initializer ]
   static const DeclarativeAst*
   parse_local_decl(Parser* parser) {
      auto head = parse_definiendum(parser);
      if (head == nullptr)
         return nullptr;
      require(parser, token::colon_tok);
      const Ast* type = parse_nontag_type(parser);
      if (match(parser->context(), token::equal_tok)) {
         auto rhs = parse_initializer(parser);
         return parser->make_definition(head, type, rhs);
      }
      return parser->make_signature(head, type);
   }

   static const DeclarativeAst*
   parse_local_decl_or_else(Parser* parser) {
      return must(parser, parse_local_decl, "expected local declaration");
   }

   // -- Parse declarations local to a where expression
   //    locals ::= local-decl | pile(local-decl)
   static Sequence<DeclarativeAst>
   parse_locals(Parser* parser) {
      switch (layout_line_count(parser)) {
      case 0:
         return { parse_local_decl_or_else(parser) };
      case 1:
         return { indented(parser, parse_local_decl_or_else) };
      default:
         return pile(parser, parse_local_decl);
      }
   }

   template<typename T>
   using def_builder =
      const T* (AstFactory::*)(const FixityForm*, const Ast*, const Ast*);

   // Parse definitional introductory forms.  The category of AST
   // to build is indicated by `builder'.
   template<typename T>
   static const T*
   do_parse_intro(Parser* parser, def_builder<T> builder) {
      auto head = parse_definiendum(parser);
      require(parser, token::colon_tok);
      const Ast* type = parse_nontag_type(parser);
      require(parser, token::equal_tok);
      auto init = parse_initializer(parser);
      return (parser->*builder)(head, type, init);
   }

   static const Ast*
   make_quantified(Parser* parser, const QuantifierHeader& q, const Ast* bound)
   {
      return parser->make_quantified(
         q.quantifier(),
         q.parameters(),
         q.constraint(),
         bound);
   }

   template<typename T>
   static const Ast*
   parse_intro(Parser* parser, token::Kind tk, def_builder<T> builder) {
      if (not match(parser->context(), tk))
         return nullptr;
      if (auto q = parse_quantifier_header(parser))
         return make_quantified(parser, q, do_parse_intro(parser, builder));
      return do_parse_intro(parser, builder);
   }

   // -- Parse a definition.
   //    definition ::= [ "define" | "prolong" ] name ":" nontag-type
   //                                 "=" initializer
   static const Ast*
   parse_definition(Parser* parser) {
      if (auto x =
          parse_intro(parser, token::define_tok, &AstFactory::make_definition))
         return x;
      return parse_intro(parser, token::prolong_tok, &AstFactory::make_prolong);
   }

   // Parse a possibly labelled statement.
   //   statement ::= control-statement | definition
   //                 | simple-statement | assignment
   //                 | assumption | postulate | rule | compound
   static const Ast*
   parse_statement(Parser* parser) {
      if (const Ast* stmt = parse_control_statement(parser))
         return stmt;
      else if (const Ast* def = parse_definition(parser))
         return def;
      else if (const Ast* prop = parse_assumption(parser))
         return prop;
      else if (const Ast* rule = parse_rule(parser))
         return rule;
      else if (auto x = parse_postulate(parser))
         return x;
      else if (auto x = parse_alias(parser))
         return x;
      else if (auto x = parse_simple_statement(parser))
         return x;
      else if (layout_line_count(parser) == 1)
         return indented(parser, parse_statement);
      return nullptr;
   }

   // -- Parse an item in a path specification
   //    path-item ::= identifier | basic-type
   static const IdentifierAst*
   parse_path_item(Parser* parser) {
      if (const IdentifierAst* x = parse_basic_type(parser))
         return x;
      return parse_identifier_or_else(parser);
   }

   // -- Parse the path of a module.
   //   path ::= identifier [ "." path ]
   static const Ast*
   parse_path(Parser* parser) {
      if (const Ast* x = parse_string(parser))
         return x;
      const IdentifierAst* x = parse_path_item(parser);
      if (match(parser->context(), token::dot_tok))
         return parser->make_path(x, parse_path(parser));
      return x;
   }

   // -- Parse an import statement
   //   import ::= "import" path
   static const Ast*
   parse_import(Parser* parser) {
      if (match(parser->context(), token::import_tok)) {
         auto path = must(parser, parse_path, "module path expected");
         return parser->make_import(path);
      }
      return nullptr;
   }

   // -- Parse a toplevel statement.
   //   toplevel ::= description | import | statement
   static const Ast*
   parse_toplevel(Parser* parser, const Token* indent) {
      if (at_eoi(parser->context()))
         return nullptr;
      else if (indent != nullptr)
         require_indentation_at(parser, column(*indent));
      if (auto x = parse_import(parser))
         return x;
      else if (auto x = parse_description(parser))
         return x;
      return parse_statement(parser);
   }

   //  -- Parse a pile of toplevel statement
   //    toplevel-pile ::= pile(toplevel)
   static void
   parse_toplevel_pile(Parser* parser, bool debugging, AstSequence& stmts) {
      auto indent = match(parser->context(), token::indent_tok);
      bool first_item = true;
      while (auto x = parse_toplevel(parser, first_item ? nullptr : indent)) {
         if (debugging)
            prefix_form(parser->debug_stream(), x) << std::endl;
         stmts.push_back(x);
         first_item = false;
      }
   }

   // -------------------------
   // -- Definition parsers ---
   // -------------------------

   // -- Parse toplevel declarations
   //     module ::= toplevel-pile
   static const AstSequence&
   parse_module(Parser* parser, const Flags& flags, AstSequence& seq) {
      const bool debugging = has(flags.verbosity, debug::parsing);
      if (debugging)
         parser->debug_stream() << "---> parsing forms <----" << std::endl;
      parse_toplevel_pile(parser, debugging, seq);
      if (not at_eoi(parser->context()))
         parse_error(parser, "stray token");
      return seq;
   }
   
   // -- Parser --
   
   Parser::Parser() : ctx() { ctx.indents.push(0); }

   void
   Parser::error(const Diagnostic& msg) {
      throw ParseErrorMessage(msg);
   }

   void
   Parser::error_at(const std::string& s, const Token& t) {
      liz::error_at(t, "parse error: " + s);
   }

   // Main entry point into the parser module.  Returns a sequence of
   // AST objects corresponding to the input token stream `tokens'.
   const AstSequence&
   Parser::parse(Fragment& input, const Flags& flags) {
      if (has(flags.verbosity, debug::lexing)) {
         debug_stream() << "---> token stream <---\n";
         std::copy(input.tokens.begin(), input.tokens.end(),
                   std::ostream_iterator<Token>(debug_stream(), " "));
         debug_stream() << std::endl;
      }
      
      ctx.cursor = input.tokens.begin();
      ctx.last = input.tokens.end();
      return parse_module(this, flags, input.asts);
   }

   AstSequence
   Parser::parse(const TokenStream& ts, const Flags& flags) {
      if (has(flags.verbosity, debug::lexing)) {
         debug_stream() << "---> token stream <---\n";
         std::copy(ts.begin(), ts.end(),
                   std::ostream_iterator<Token>(debug_stream(), " "));
         debug_stream() << std::endl;
      }

      ctx.cursor = ts.begin();		
      ctx.last = ts.end();
      AstSequence asts;
      return std::move(parse_module(this, flags, asts));
   }

   const SourceFileAst*
   Reader::read_file(const std::string& filename, const Flags& flags) {
      SourceFileAst* file = make_source_file(filename);
      file->tokens << file->source;
      parse(*file, flags);      
      return file;
   }
}
