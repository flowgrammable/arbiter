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

#include <liz/utility>

namespace liz {
   // -- Symbol --
   std::ostream&
   operator<<(std::ostream& os, Symbol s) {
      return os << s.string();
   }
   
   // -- Diagnostic reporting and error recovery
   BasicError::BasicError(const std::string& m) : msg(m) { }

   void BasicError::disclose_location_on(std::ostream&) const {
   }

   void
   BasicError::format_message(std::ostream& os) const {
      os << msg;
   }

   void
   BasicError::issue_on(std::ostream& os) const {
      disclose_location_on(os);
      format_message(os);
   }

   InternalError::InternalError(const std::string& s)
         : BasicError(s)
   { }

   void
   internal_error(const std::string& msg) {
      throw InternalError("internal error: " + msg);
   }
   
   SystemError::SystemError(const std::string& s)
         : BasicError(s)
   { }

   void system_error(const std::string& msg) {
      throw SystemError("system error: " + msg);
   }

   void cannot_open(const std::string& file) {
      throw FileError(file);
   }

   FileError::FileError(const std::string& s)
         : BasicError("Cannot open file " + quote(s))
   { }

   void filesystem_error(const std::string& file) {
      throw FileError(file);
   }

   EvaluationError::EvaluationError(const std::string& s)
         : BasicError(s)
   { }

   void evaluation_error(const std::string& msg) {
      throw EvaluationError("evaluation error: " + msg);
   }

   RedefinitionError::RedefinitionError(const std::string& s)
         : EvaluationError(s)
   { }

   void redefinition_error(const std::string& msg) {
      throw RedefinitionError("evaluation error: " + msg);
   }

   std::string
   quote(const std::string& s) {
      return "`" + s + "'";
   }
}
