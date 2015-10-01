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


#ifndef LIZ_EVALUATOR_INCLUDED
#define LIZ_EVALUATOR_INCLUDED


#include <stack>
#include <set>
#include <iostream>
#include "Parser.H"
#include "Elaborator.H"
#include "Translator.H"

namespace liz {
   // -- FreeVariableError --
   struct FreeVariableError : BasicError {
      FreeVariableError(const std::string&, Symbol);
   private:
      Symbol sym;
      void format_message(std::ostream&) const;
   };

   // ---------------
   // -- CallFrame --
   // ---------------
   struct CallFrame : Store {
      explicit CallFrame(FunctionElaboration);
      FunctionElaboration function() const { return fun; }
      Store* push_store();
      void pop_store();
      const Data::Value* address(Symbol) const;
   private:
      const FunctionElaboration fun;
      std::list<Store> st;
   };

   // ------------------
   // -- ControlStack --
   // ------------------
   struct ControlStack : Stack<void, CallFrame> {
      void push(FunctionElaboration);
      void pop();
   };

   // ----------------
   // -- StoreStack --
   // ----------------
   struct StoreStack : Stack<Variable, StoreRef> {
      void push(Store*);
      void pop();
   };

   // ---------------
   // -- Evaluator --
   // ---------------
   // Object of this type represents C++ evaluators.
   struct Evaluator : Elaborator {
      Evaluator(Reader&);
      ~Evaluator();

      // Toplevel read-eval-print-loop
      void toplevel_loop(const Flags&);

      bool process_file(const Path&, const Flags&);

      using Elaborator::eval;
      Object eval(Elaboration);

      // Evaluate a toplevel statement
      void evaluate_toplevel(const Ast*);
      // Evaluate a sequence of toplevel statements
      void evaluate_toplevel(const AstSequence&);

      CallFrame* push_call_frame(FunctionElaboration);
      CallFrame* current_call_frame();
      void pop_call_frame();

      StoreStack* get_store() { return &store; }

   private:
      // define a builtin type or concept in the global environement.
      void define_global(const BasicType*, const Type*);
      void define_global(const Concept*, const Type*);
      
      // Bind a symbol to a value in a given environment.
      Object* define(Store*, const std::string&, Object);

      StoreStack store;         // global store stack
      ControlStack ctrl;        // Control stack
   };
}

#endif	//  LIZ_EVALUATOR_INCLUDED
