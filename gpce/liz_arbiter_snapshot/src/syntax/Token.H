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

#ifndef LIZ_TOKEN_INCLUDED
#define LIZ_TOKEN_INCLUDED

#include <cstddef>
#include <iosfwd>
#include <list>
#include <string>
#include <stack>

#include <liz/utility>
#include <liz/Input>

namespace liz {
   namespace token {
      // Underlying representation of basic literal characters.
      using u8 = unsigned char;

      // Underlying representation type for token kinds.
      using kind_value = uint32_t;
      
      // Numeric basis for numeric representation of token kinds.
      constexpr kind_value u8_bits = 8;

      // Value of the unigraphic token composed of unique basic character `c'.
      constexpr kind_value
      value(u8 c) {
         return c;
      }
   
      // Value of digraphic token composed of 2 characters `c1' and `c2'
      constexpr kind_value
      value(u8 c1, u8 c2) {
         return (c1 << u8_bits) | value(c2);
      }

      // Value of trigraphic token composed of 3 characters `c1', `c2', and `c3'.
      constexpr kind_value
      value(u8 c1, u8 c2, u8 c3) {
         return (c1 << (2 * u8_bits)) | value(c2, c3);
      }

      // -- Literal strings of given length.
      template<int N>
      using text_chunk = const char (&)[N+1];

      constexpr kind_value
      value(text_chunk<0>) {
         return u8();
      }

      constexpr kind_value
      value(text_chunk<1> s) {
         return value(s[0]);
      }

      constexpr kind_value
      value(text_chunk<2> s) {
         return value(s[0], s[1]);
      }

      constexpr kind_value value(text_chunk<3> s) {
         return value(s[0], s[1], s[2]);
      }

      // Token objects are classified by kinds.
      enum Kind : kind_value {
         unknown_tok          = value(""),         // unknown token
         hash_tok             = value("#"),
         caret_tok            = value("^"),
         exclamation_tok      = value("!"),
         plus_tok             = value("+"),
         minus_tok            = value("-"),
         equal_tok            = value("="),
         less_tok             = value("<"),
         greater_tok          = value(">"),
         ampersand_tok        = value("&"),
         bar_tok              = value("|"),
         star_tok             = value("*"),
         slash_tok            = value("/"),
         percent_tok          = value("%"),
         tilde_tok            = value("~"),
         open_paren_tok       = value("("),
         close_paren_tok      = value(")"),
         open_bracket_tok     = value("["),
         close_bracket_tok    = value("]"),
         open_brace_tok       = value("{"),
         close_brace_tok      = value("}"),
         comma_tok            = value(","),
         dot_tok              = value("."),
         semicolon_tok        = value(";"),
         colon_tok            = value(":"),
         at_tok               = value("@"),
         underscore_tok       = value("_"),
         last_unigraph_tok    = 0xff,
         description_tok      = value("++"),
         wisecrack_tok        = value("--"),
         right_arrow_tok      = value("->"),
         left_arrow_tok       = value("<-"),
         less_equal_tok       = value("<="),
         greater_equal_tok    = value(">="),
         double_equal_tok     = value("=="),
         implies_tok          = value("=>"),
         not_equal_tok        = value("!="),
         double_colon_tok     = value("::"),
         colon_equal_tok      = value(":="),
         hash_exclamation_tok = value("#!"),
         dot_dot_tok          = value(".."),
         call_tok             = value("()"), // pseudo, call operator
         index_tok            = value("[]"), // pseudo, index operator
         construct_tok        = value("{}"), // pseudo, construction operator
         open_mbracket_tok    = value("[|"),
         close_mbracket_tok   = value("|]"),
         open_bar_brace_tok   = value("{|"),
         close_bar_brace_tok  = value("|}"),
         meet_tok             = value("/\\"),
         join_tok             = value("\\/"),
         last_digraph_tok     = 0xffff,
         equiv_tok            = value("<=>"),
         last_trigraph_tok    = 0xffffff,
         delimiter_tok,                   // delimiter characters
         literal_boolean_tok,             // Boolean literal values
         literal_character_tok,           // Character literal values
         literal_integer_tok,             // Integral literal values
         literal_ipv4_tok,                // IPv4 literal values
         literal_hex_tok,                 // Hexidecimal literal values
         literal_string_tok,              // String literal values
         literal_real_tok,                // Real literal values
         identifier_tok,                  // identifier
         div_tok,                         // "div"
         quo_tok,                         // "quo"
         mod_tok,                         // "mod"
         rem_tok,                         // "rem"
         is_tok,                          // "is"
         axiom_tok,                       // "axiom"
         prop_tok,                        // "prop"
         const_tok,                       // "const"
         if_tok,                          // "if"
         then_tok,                        // "then"
         else_tok,                        // "else"
         in_tok,                          // "in"
         match_tok,                       // "match"
         with_tok,                        // "with"
         case_tok,                        // "case"
         for_tok,                         // "for"
         while_tok,                       // "while"
         until_tok,                       // "until"
         repeat_tok,                      // "repeat"
         do_tok,                          // "do"
         return_tok,                      // "return"
         throw_tok,                       // "throw"
         leave_tok,                       // "leave"
         break_tok,                       // "break"
         variant_tok,                     // "variant"
         record_tok,                      // "record"
         key_tok,                         // "key"
         requires_tok,                    // "requires"
         bool_tok,                        // "bool"
         byte_tok,                        // "byte"
         char_tok,                        // "char"
         int_tok,                         // "int"
         double_tok,                      // "double"
         string_tok,                      // "string"
         type_tok,                        // "type"
         not_tok,                         // "not"
         and_tok,                         // "and"
         or_tok,                          // "or"
         forall_tok,                      // "forall"
         exists_tok,                      // "exists"
         concept_tok,                     // "concept"
         namespace_tok,                   // "namespace"
         assert_tok,                      // "assert"
         assume_tok,                      // "assume"
         define_tok,                      // "define"
         where_tok,                       // "where"
         rule_tok,                        // "rule"
         import_tok,                      // "import"
         inductive_tok,                   // "inductive"
         coinductive_tok,                 // "coinductive"
         prolong_tok,                     // "prolong"
         postulate_tok,                   // "postulate"
         realize_tok,                     // "realize"
         indent_tok,                      // increase indentation level
         justify_tok,                     // match previous indentation
         unindent_tok,                    // decrease indentation level
         last_tok
      };

      std::ostream& operator<<(std::ostream&, Kind);
   }

   // Type of offset value types; which is basically the same as
   // type of difference between two native pointer types.
   using offset_type = ptrdiff_t;

   // -----------
   // -- Token --
   // -----------

   struct Token {
      token::Kind kind;         // type of this token
      input::Span span;         // span of this token's lexeme
      input::Line line;         // input source line provenance.
   };

   // -- Return the column number of a token
   inline input::Span::Column
   column(const Token& t) {
      return t.span.column();
   }

   // The line number of the first character of this token's lexeme.
   inline input::Line::Number
   line_number(const Token& t) {
      return t.line.number();
   }

   // -- Return the length of a token's lexeme.
   inline Cardinal
   length(const Token& t) {
      return t.span.size();
   }

   // -- Return the starting iterator of a token's lexeme
   inline input::LineIterator
   begin(const Token& t) {
      return t.line.begin() + column(t);
   }
   
   // -- Return the one-past-the-end iterator of a token's lexeme
   inline input::LineIterator
   end(const Token& t) {
      return begin(t) + length(t);
   }
   
   // Return the lexeme of a token
   inline std::string
   lexeme(const Token& t) {
      return { begin(t), end(t) };
   }
   
   // Print a token on the output stream.
   std::ostream& operator<<(std::ostream&, const Token&);

   // -- Indentation stack --
   using IndentationStack = std::stack<input::Column>;

   // -----------------
   // -- TokenStream --
   // -----------------
   // The lexer turns logical input lines into a sequence of tokens
   // represented by objects of this type.
   struct TokenStream : private std::list<Token> {
      struct Range : std::pair<iterator, iterator> {
         Range(iterator b, iterator e)
               : std::pair<iterator, iterator>(b, e)
         { }
         iterator begin() const { return first; }
         iterator end() const { return second; }
      };
      using super = std::list<Token>;
      using super::iterator;
      using super::const_iterator;
      using super::value_type;
      using super::begin;
      using super::end;
      using super::front;
      using super::back;
      using super::clear;

      TokenStream();
      TokenStream(TokenStream&&) = default;
      Range operator[](input::Line);
      iterator insert(const Token&);
      iterator append(TokenStream&&);

   private:
      TokenStream(const TokenStream&) = delete;
      TokenStream& operator=(const TokenStream&) = delete;
      iterator inst_pt;         // insertion point
   };

   using TokenIterator = TokenStream::const_iterator;

   // Decompose an input into tokens and append them to the stream
   TokenStream& operator<<(TokenStream&, input::Line);
   TokenStream& operator<<(TokenStream&, const input::Source&);

   // Transform a input source into a sequence of tokens.
   TokenStream tokenize(input::Line);

   // -- SyntaxError --
   struct SyntaxError : BasicError {
      SyntaxError(const input::Location&, const std::string&);
      input::LineNumber line_number() const { return loc.line; }
      input::ColumnNumber column_number() const { return loc.column; }
   private:
      input::Location loc;
      void disclose_location_on(std::ostream&) const;
   };
}

#endif	// LIZ_TOKEN_INCLUDED
