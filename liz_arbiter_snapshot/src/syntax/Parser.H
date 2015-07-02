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

#ifndef LIZ_PARSER_INCLUDED
#define LIZ_PARSER_INCLUDED

#include <liz/Ast>
#include "BasicDriver.H"

namespace liz {
   // -------------
   // -- ErrorAt --
   // -------------
   struct ErrorAt : BasicError {
      ErrorAt(const std::string&, const Token&);
      const Token& token() const { return tok; }
      const input::Line& line() const { return token().line; }
   protected:
      void disclose_location_on(std::ostream&) const;
   private:
      const Token tok;
   };

   void error_at(const Token&, const std::string&);

   // -----------------------
   // -- ParseErrorMessage --
   // -----------------------
   struct ParseErrorMessage : BasicError {
      explicit ParseErrorMessage(const std::string& m) : BasicError(m) { }
   };

   // -- Parsing context.
   struct ParsingContext {
      IndentationStack indents; // Indentation levels
      TokenIterator cursor;     // Next available token.
      TokenIterator last;       // End of token stream.
   };

   // ------------
   // -- Parser --
   // ------------
   struct Parser : AstFactory, BasicDriver {
      struct IndentationManager;

      // Build a parser in a default state
      Parser();
      
      // The entry point into the parser module.
      const AstSequence& parse(Fragment&, const Flags&);
      AstSequence parse(const TokenStream&, const Flags&);

      // Current parsing context.
      ParsingContext* context() { return &ctx; }

      // Issue sever diagnostics.
      void error(const Diagnostic&);
      virtual void error_at(const Diagnostic&, const Token&);

   private:
      ParsingContext ctx;
   };

   // -- Parser::IndentationManager --
   struct Parser::IndentationManager {
      IndentationManager(Parser*, input::Column);
      ~IndentationManager();
   private:
      Parser* const parser;
   };
   
   // ------------
   // -- Reader --
   // ------------
   struct Reader : Parser {
      const SourceFileAst* read_file(const Path&, const Flags&);
      const Ast* read_name_from(const char*&);
      const Ast* read_expression_from(const char*&);
   };
}

#endif	// LIZ_PARSER_INCLUDED
