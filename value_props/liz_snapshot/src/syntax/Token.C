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

#include <cctype>
#include <iostream>
#include <sstream>
#include <liz/Token>

namespace liz {
   using input::ColumnNumber;

   SyntaxError::SyntaxError(const input::Location& l, const std::string& m)
         : BasicError(m), loc(l)
   { }

   void SyntaxError::disclose_location_on(std::ostream& os) const {
      os << "Syntax error at line " << line_number()
         << ", column " << column_number()
         << ": ";
   }

   static void
   syntax_error(const input::Location& loc, const std::string& msg) {
      throw SyntaxError(loc, msg);
   }


   static TokenStream&
   operator<<(TokenStream& ts, const Token& t) {
      ts.insert(t);
      return ts;
   }

   using input::LineIterator;
   using input::Line;
   using input::Source;

   static Token
   make_token(Line line, token::Kind kind,
              LineIterator first, LineIterator last) {
      const Ordinal column = first - line.begin();
      const Cardinal length = last - first;
      return { kind, { column, length }, line };
   }

   // We store all alphabetic keywords in a table to allow
   // for a simple map from lexeme to token type.
   struct Keyword {
      const char* text;
      const token::Kind kind;
   };
   
   const Keyword keyword_table[] = {
      { "_", token::underscore_tok },
      { "true", token::literal_boolean_tok },
      { "false", token::literal_boolean_tok },
      { "mod", token::mod_tok },
      { "rem", token::rem_tok },
      { "div", token::div_tok },
      { "quo", token::quo_tok },
      { "if", token::if_tok },
      { "in", token::in_tok },
      { "then", token::then_tok },
      { "else", token::else_tok },
      { "match", token::match_tok },
      { "with", token::with_tok },
      { "case", token::case_tok },
      { "for", token::for_tok },
      { "while", token::while_tok },
      { "until", token::until_tok },
      { "repeat", token::repeat_tok },
      { "do", token::do_tok },
      { "break", token::break_tok },
      { "and", token::and_tok },
      { "or", token::or_tok },
      { "not", token::not_tok },
      { "return", token::return_tok },
      { "throw", token::throw_tok },
      { "leave", token::leave_tok },
      { "where", token::where_tok },
      { "alias", token::alias_tok },
      { "is", token::is_tok },
      { "bool", token::bool_tok },
      { "byte", token::byte_tok },
      { "char", token::char_tok },
      { "int", token::int_tok },
      { "double", token::double_tok },
      { "string", token::string_tok },
      { "const", token::const_tok },
      { "variant", token::variant_tok },
      { "record", token::record_tok },
      { "key", token::key_tok },
      { "type", token::type_tok },
      { "requires", token::requires_tok },
      { "forall", token::forall_tok },
      { "exists", token::exists_tok },
      { "axiom", token::axiom_tok },
      { "prop", token::prop_tok },
      { "concept", token::concept_tok },
      { "namespace", token::namespace_tok },
      { "assume", token::assume_tok },
      { "assert", token::assert_tok },
      { "define", token::define_tok },
      { "rule", token::rule_tok },
      { "import", token::import_tok },
      { "inductive", token::inductive_tok },
      { "coinductive", token::coinductive_tok },
      { "prolong", token::prolong_tok },
      { "postulate", token::postulate_tok },
      { "realize", token::realize_tok },
      { "indentation mark", token::indent_tok },
      { "justification mark", token::justify_tok },
      { "unindentation mark", token::unindent_tok }
   };

   namespace token {
      // Constructor functions
      constexpr Kind kind(u8 c) { return Kind(c); }
      constexpr Kind kind(u8 c1, u8 c2) { return Kind(value(c1, c2)); }

      std::ostream&
      operator<<(std::ostream& os, Kind k) {
         if (k < last_unigraph_tok)
            return os << char(k);
         else if (k < last_digraph_tok)
            return os << char((k & 0xff00) >> u8_bits) << char(k & 0xff);
         else if (k < last_trigraph_tok)
            return os << char((k & 0xff0000) >> (2 * u8_bits))
                      << char((k & 0xff00) >> u8_bits)
                      << char(k & 0xff);
         for (auto& key : keyword_table)
            if (key.kind == k)
               return os << key.text;
         return os << "<unknown-token: " << kind_value(k) << ">";
      }
   }
   
  std::ostream&
  operator<<(std::ostream& os, const Token& t) {
    const char* prefix = nullptr;
    switch (t.kind) {
    case token::indent_tok:
       return os << "INDENT" << '(' << column(t) << ')';
    case token::unindent_tok:
       return os << "UNINDENT" << '(' << column(t) << ')';
    case token::justify_tok:
       return os << "JUSTIFY" << '(' << column(t) << ')';

    case token::literal_string_tok:
      prefix = "STRING";
      break;

    case token::literal_character_tok:
      prefix = "CHAR";
      break;

    case token::literal_integer_tok:
      prefix = "INTEGER";
      break;

    case token::literal_hex_tok:
       prefix = "HEXIDECIMAL";
       break;

    case token::literal_boolean_tok:
      prefix = "BOOLEAN";
      break;

    case token::identifier_tok:
      prefix = "IDENTIFIER";
      break;

    case token::description_tok:
       prefix = "DESCRIPTION";
       break;

    case token::wisecrack_tok:
       prefix = "WISECRACK";
       break;

    case token::literal_ipv4_tok:
       prefix = "IPV4";
       break;

    default:
      break;
    }
    
    if (prefix != nullptr) {
      os << prefix << '(';
    }
    else
      os << '"';

    std::copy(begin(t), end(t), std::ostream_iterator<char>(os));
    
    if (prefix != nullptr)
      os << ')';
    else
      os << '"';
    
    return os;
  }

   // Skip leading blank characters, return true iff found at least one
   // nonblank character (first points to that character).
   static bool
   nonblank(LineIterator& first, LineIterator last) {
      while (first != last && *first == ' ')
         ++first;
      return first != last;
   }
  
   // Return true if the character `c' is a letter according to our
   // interpretation of the Elements.
   static inline bool
   is_letter(char c) {
      return std::isalpha(c) or c == '_';
   }

   static bool
   follows(const char* c, LineIterator f, Line line) {
      auto last = line.end();
      std::string str(c);
      for (auto i = str.begin(); i != str.end(); ++i, ++f) {
         if (not (f < last))
            return false;
         if (*i != *f)
            return false;
      }
      return true;
   }

   static void
   ensure_string(const char* c, LineIterator& f, Line line) {
      auto start = f;
      auto last = line.end();
      for (; *c != '\0' and f < last; ++c, ++f) {
         if (*c != *f) {
            std::string str("expected `");
            str += c;
            str += "`";
            syntax_error({line.number(),ColumnNumber(f++ - start)},str.c_str());
         }
      }
   }

   static inline void
   ensure_char(char c, LineIterator& current, Line line) {
      auto last = line.end();
      auto start = current;
      if (current < last and *current == c) {
         ++current;
      } else {
         std::string str("expected character `");
         str += c;
         str += '`';
         syntax_error({ line.number(), ColumnNumber(current++ - start) },
                      str.c_str());
      }
   }

   static void
   ensure_digits(LineIterator& current, Line line) {
      auto last = line.end();
      auto start = current;
      if (not (current < last and std::isdigit(*current)))
         syntax_error({ line.number(), ColumnNumber(current++ - start) },
                      "expected digits");
      while (current < last and std::isdigit(*current))
         ++current;
   }

   static void
   ensure_hex_digits(LineIterator& current, Line line) {
      auto last = line.end();
      auto start = current;
      if (not (current < last and std::isxdigit(*current)))
         syntax_error({ line.number(), ColumnNumber(current++ - start) },
                      "expected hexidecimal digits");
      while (current < last and std::isxdigit(*current))
         ++current;
   }

   static LineIterator
   lex_ipv4(TokenStream& ts, Line line, LineIterator& current) {
      LineIterator start = current;
      ensure_string("ipv4(", current, line);
      ensure_digits(current, line);
      ensure_char('.', current, line);
      ensure_digits(current, line);
      ensure_char('.', current, line);
      ensure_digits(current, line);
      ensure_char('.', current, line);
      ensure_digits(current, line);
      ensure_char(')', current, line);
      ts << make_token(line, token::literal_ipv4_tok, start, current);
      return current;
   }
   
   // Return true is the character `c' can be part of an identifier.
   // Note that a character that can be part of an identifier may not
   // be the first letter of that identifier (e.g. digits).
   static inline bool
   is_part_of_identifier(char c) {
      return is_letter(c) or std::isdigit(c);
   }

   static LineIterator
   lex_hexidecimal(TokenStream& ts, Line line, LineIterator current) {
      auto start = current;
      ensure_string("0x", current, line);
      ensure_hex_digits(current, line);
      ts << make_token(line, token::literal_hex_tok, start, current);
      return current;
   }

   // Classify the number lexeme starting with `current', and running
   // possibly through 'last'.  Return the actual one-past-the-end iterator.
   static LineIterator
   lex_number(TokenStream& ts, Line line, LineIterator current) {
      if (follows("0x", current, line))
         return lex_hexidecimal(ts, line, current);
      LineIterator start = current;
      while ((current < line.end()) and std::isdigit(*current))
         ++current;
      // Don't mistake an interval for a malformed floating point number.
      if (current + 2 < line.end()
          and token::kind(current[0], current[1]) == token::dot_dot_tok)
         ts << make_token(line, token::literal_integer_tok, start, current);
      // Checking for real numbers starting 
      else if (current + 1  < line.end() and *current == '.'
          and std::isdigit(current[1])) {
         ++current;
         while (current < line.end() and std::isdigit(*current))
            ++current;
         ts << make_token(line, token::literal_real_tok, start, current);
      }
      else
         ts << make_token(line, token::literal_integer_tok, start, current);
      
      return current;
   }

   // Subroutine of `is_keyword'.
   // Compare a token's lexeme for equality against a C-style string.
   static bool
   lexeme_equals(const Token& t, const char* s) {
      for (auto c : t) {
         if (*s == 0 or c != *s)
            return false;
         ++s;
      }
      return *s == 0;
   }

   // If the lexeme of `t' designates a keyword, return a pointer to the
   // corresponding Keyword structure.  Otherwise, return null.
   // FIXME: if the keyword table gets a bit large, then its content
   // should be stored to allow a binary search, which would
   // be more efficient than the current linear search.
   static const Keyword*
   is_keyword(const Token& t) {
      for (auto& entry : keyword_table)
         if (lexeme_equals(t, entry.text))
            return &entry;
      return nullptr;
   }

   // -- Each string in this table list all possible combinations
   // -- of digraphic tokens.  A digraph combination is obtained 
   // -- by the first character and any other remaining in the string.
   const char* digraph_info[] = {
      "==>",                    // "=", "==", "=>"
      "->",                     // "-", "->"
      "<=-",                    // "<", "<=", "<-"
      ">=",                     // ">", ">="
      "!=",                     // "!", "!="
      "|]}",                    // "|", "|]", "|}"
      "::=",                    // ":", "::", ":="
      "#!",                     // "#", "#!"
      "[|",                     // "[", "[|"
      "..",                     // ".", ".."
      "/\\",                    // "/", "/\"
   };

   static const char*
   get_digraph_info(char c) {
      for (int i = 0; i < length(digraph_info); ++i) {
         const char* info = digraph_info[i];
         if (*info == c)
            return info;
      }
      return nullptr;
   }

   // Return true if the character `c' is a delimiter.
   static bool
   is_delimiter(char c) {
      switch (c) {
      case '(': case ')': case '{': case '}': case ']':
      case '.': case ',': case ';': case '"': case '\'':
      case '%': case '~': case '@': case '/': case '\\':
         return true;

      default:
         return std::isspace(c) or get_digraph_info(c) != nullptr;
      }
   }
   
   // static char
   // transform_escape_char(char c) {
   //   switch (c) {
   //     case 'n': return '\n';
   //     case '\\': return '\\';
   //     case '"': return '"';
   //     default: return c;
   //   }
   // }
   
   // static std::string
   // fixup_escape_chars(std::string s) {
   //   for (auto i = s.begin(); i != s.end(); ++i) {
   //     if (*i == '\\') {
   //       i = s.erase(i);
   //       if (i != s.end()) *i = transform_escape_char(*i);
   //     }
   //   }
   //   return s;
   // }
   
   // Classify a lexeme that starts from `current' and possibly run through
   // `last'.  Add the resulting token to the current token stream.
   // At this point, the only possibilities are: identifier, keyword,
   // delimiter, or junk.
   static LineIterator
   lex_symbol(TokenStream& ts, Line line, LineIterator current) {
      if (follows("ipv4(", current, line))
         return lex_ipv4(ts, line, current);
      LineIterator start = current;
      // Maybe an identifier or a keyword.
      if (is_letter(*current)) { 
         while (current < line.end() and is_part_of_identifier(*current))
            ++current;
         // Presumbly this is an identifier.
         Token t = make_token(line, token::identifier_tok,start, current);
         // Fix that assumption when it is, in fact, a keyword.
         if (const Keyword* kwd = is_keyword(t))
            t.kind = kwd->kind;

         ts << t;
      }
      // No, it is junk.
      else {
         while (current < line.end() and not is_delimiter(*current))
            ++current;
         ts << make_token(line, token::unknown_tok, start, current);
      }
      return current;
   }
   
   // Classify the lexeme (predicted to be an operator string) starting
   // from  `current' and possibly running through the entire `line'.
   // Return the actual one-past-the-end iterator of the recognize lexeme.
   static LineIterator
   lex_operator(TokenStream& ts, Line line, LineIterator current) {
      LineIterator start = current;
      token::Kind tt = token::unknown_tok;
      // Try digraphic tokens first.
      if (const char* chars = get_digraph_info(*current)) {
         if (current + 1 < line.end())
            for (const char* p = chars + 1; *p != 0; ++p)
               if (*p == current[1]) {
                  tt = token::kind(current[0], current[1]);
                  current += 2;
                  break;
               }
      }
      // If that fails, then it is just a single character operator.
      if (tt == token::unknown_tok) {
         tt = token::kind(*current);
         ++current;
      }
      ts << make_token(line, tt, start, current);
      return current;
   }

   // Make a description token starting at given column.
   static Token
   description(Line line, LineIterator start) {
      return make_token(line, token::description_tok, start, line.end());
   }

   // Compute the boundary of the next token after seeing '+'.
   static LineIterator
   lex_plus_et_al(TokenStream& ts, Line line, LineIterator start) {
      if (start + 1 < line.end() and start[1] == '+') {
         ts << description(line, start);
         return line.end();
      }
      return lex_operator(ts, line, start);
   }

   // Make a comment token starting a given location.
   static Token
   wisecrack(Line line, LineIterator start) {
      return make_token(line, token::wisecrack_tok, start, line.end());
   }

   // Compute the boundary of the next token after seeing '+'.
   static LineIterator
   lex_minus_et_al(TokenStream& ts, Line line, LineIterator start) {
      if (start + 1 < line.end() and start[1] == '-') {
         ts << wisecrack(line, start);
         return line.end();
      }
      return lex_operator(ts, line, start);
   }

   // Recognize a lexeme starting with '<'.  
   static LineIterator
   lex_less_et_al(TokenStream& ts, Line line, LineIterator start) {
      if (start + 2 < line.end() and start[1] == '=' and start[2] == '>') {
         auto next = start + 3;
         ts << make_token(line, token::equiv_tok, start, next);
         return next;
      }
      return lex_operator(ts, line, start);
   }

   // Decompose a line (character sequence) into a token sequence.
   static TokenStream&
   ingest_tokens_from(TokenStream& ts, Line line) {
      LineIterator current = line.begin();
      LineIterator last = line.end();
      while (nonblank(current, last)) {
         LineIterator start = current;
         switch (*current) {	 
         case '*': case '%': case '=': case '&':
         case '|': case '#': case '!': case '~': case '>':
         case '(': case ')': case '[': case ']': case '{': case '}':
         case ':': case ';': case ',': case '^': case '.': case '@':
            current = lex_operator(ts, line, current);
            break;

         case '<':
            current = lex_less_et_al(ts, line, current);
            break;
            
         case '"': {
            // We collect all the characters making up the literal
            // string, including the leading and trailing double
            // quotes.  However, the actual internal representation
            // does not include them, e.g. we take everything between
            // one-past-start and one-before-current.
            do {
	       if (*current == '\\' and current < last - 1) ++current;
               ++current; 
	    } while (current < last and *current != '"');
	    ts <<  make_token(line, token::literal_string_tok, start + 1, current);
            // If we went to the closing quote, move one more time.
            if (current < last)
               ++current;
            break;
	 }
         case '\'' : {
            do {
	       if (*current == '\\' and current < last - 1) ++current;
               ++current; 
	    } while (current < last and *current != '\'');
	    ts <<  make_token(line, token::literal_character_tok, start + 1, current);
            if (current < last)
               ++current;
            break;
	 }
	 
         case '/':
            if (current + 1 < last and current[1] == '\\') {
               ts << make_token(line, token::meet_tok, start, start + 2); 
               current += 2;
            }
            else
               ts << make_token(line, token::slash_tok, start, ++current);
            break;

         case '+':
            current = lex_plus_et_al(ts, line, current);
            break;

         case '-':
            current = lex_minus_et_al(ts, line, current);
            break;

         case '\\':
            if (current + 1 < last and current[1] == '/') {
               ts << make_token(line, token::join_tok, start, start + 2);
               current += 2;
            }
            else
               ts << make_token(line, token::unknown_tok, start, ++current);
            
         case '0': case '1': case '2': case '3': case '4':
         case '5': case '6': case '7': case '8': case '9':
            current = lex_number(ts, line, current);
            break;

         case '\t':
            syntax_error({ line.number(), ColumnNumber(current++ - start) },
                         "horizontal tab character");
            break;
            
         case '\v':
            syntax_error({ line.number(), ColumnNumber(current++ - start) },
                         "vertical tab character");
            break;
            
         default:
            current = lex_symbol(ts, line, current);
            break;
         }
      }
      return ts;
   }

   // Decompose a line (character sequence) into a token sequence.
   TokenStream&
   operator<<(TokenStream& ts, Line line) {
      return ingest_tokens_from(ts, line);
   }

   inline bool
   more_indented(Line x, Line y) {
      return x.indentation() > y.indentation();
   }

   static Token
   delimiter(Line line, token::Kind k) {
      return { k, { line.indentation(), 0 }, line };
   }

   TokenStream&
   operator<<(TokenStream& ts, const Source& src) {
      IndentationStack indents;
      for (auto p = src.begin(); p != src.end(); ++p) {
         auto line = src.line(p);
         if (line.blank())
            continue;
         const auto n = line.indentation();
         if (line.length() > n + 1 and line[n] == '-' and line[n+1] == '-')
            ;                   // keep wisecrack lines, but ignore format
         else if (indents.empty() or n > indents.top()) {
            indents.push(n);
            ts << delimiter(line, token::indent_tok);
         }
         else {
            while (n < indents.top()) {
               ts << delimiter(line, token::unindent_tok);
               indents.pop();
            }
            if (n == indents.top())
               ts << delimiter(line, token::justify_tok);
         }
         ts << line;
      }
      return ts;
   }

   TokenStream
   tokenize(Line line) {
      TokenStream ts;
      ingest_tokens_from(ts, line);
      return ts;
   }

   // -- TokenStream --
   TokenStream::TokenStream() : inst_pt(end()) { }

   TokenStream::iterator
   TokenStream::insert(const Token& t) {
      auto p = super::insert(inst_pt, t);
      return inst_pt = ++p;
   }

   TokenStream::iterator
   TokenStream::append(TokenStream&& ts) {
      for (auto& t : ts)
         insert(t);
      return inst_pt;
   }

   TokenStream::Range
   TokenStream::operator[](input::Line l) {
      for (auto p = begin(); p != end(); ++p)
         if (p->line == l) {
            for(auto q = p; q != end(); ++q)
               if (q->line != l)
                  return { p, q };
            return { p, end() };
         }
      return { end(), end() };
   }
}
