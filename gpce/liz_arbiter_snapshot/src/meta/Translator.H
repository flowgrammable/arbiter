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

#ifndef LIZ_TRANSLATOR_INCLUDED
#define LIZ_TRANSLATOR_INCLUDED

#include <string>
#include <stack>
#include <liz/utility>
#include "Environment.H"
#include "Expression.H"

namespace liz {
   // ----------------
   // -- Translator --
   // ----------------
   // General interface to a Liz translator instance.
   struct Translator {
      // -- Manage flags stack used to invoke a translator.
      struct FlagsManager {
         FlagsManager(Translator&, const Flags&);
         ~FlagsManager();
      private:
         Translator& ctx;
      };

      Translator();
      virtual ~Translator();

      // Process an input file given by its path
      virtual bool process_file(const Path&, const Flags&) = 0;

      // Occasionally, type elaboration entails evaluation of certain forms.
      // We could devise a simple compile-time evaluator that could
      // be independent of the runtime evaluation engine.  However, that
      // looks like a point-less labour duplication.  Consequentely
      // we make that engine an implicit parameter to the elaborator
      // by require this pure virtual function.
      virtual Object eval(Elaboration) = 0;

      // Elaborate a program fragment in a context where a specific
      // type of expression is expected.  In particular a null value
      // for the second argument corresponds to an inference mode.
      // The third parameter designates the home scope where of any
      // declaration resultin from the elaboration should reside.
      virtual Elaboration elaborate(const Ast*, const Type*) = 0;

      // Returns the current flags in effect.
      const Flags& current_flags() const;
      bool enabled(debug::Verbosity) const;

      // Return the path to the standard library directory.
      Path stdlib_path() const;

      // If the path contains a library path, return the absolute path.
      Path expand_library_path(const Path&) const;

      // Add another library path.
      // FIXME: Implement.
      void add_library_path(const std::string&, Path);

      // Returns true if path has been seen before.
      bool have_seen_path(const Path&) const;
      void mark_path_as_seen(const Path&);

   protected:
      // Flags stack management
      void push_flags(const Flags&);
      void pop_flags();

   private:
      std::stack<Flags> flags;
      std::map<std::string,Path> library_paths;
      std::set<Path> paths_seen;
   };

}

#endif  // LIZ_TRANSLATOR_INCLUDED
