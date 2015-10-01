// Copyright (C) 2013, Texas A&M University
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

#include "Translator.H"

namespace liz {
   // -- Translator::FlagsManager --
   Translator::FlagsManager::FlagsManager(Translator& t, const Flags& f)
         : ctx(t) {
      ctx.push_flags(f);
   }

   Translator::FlagsManager::~FlagsManager() {
      ctx.pop_flags();
   }
   
   // -- Translator --
   Translator::Translator() {
      push_flags(Flags());
      library_paths["Liz"] = stdlib_path();
   }

   Translator::~Translator() {
      pop_flags();
   }

   void
   Translator::push_flags(const Flags& f) {
      flags.push(f);
   }

   void
   Translator::pop_flags() {
      flags.pop();
   }

   const Flags&
   Translator::current_flags() const {
      return flags.top();
   }

   bool
   Translator::enabled(debug::Verbosity v) const {
      return debug::has(current_flags().verbosity, v);
   }

   Path
   Translator::stdlib_path() const {
      if (not current_flags().stdlib.empty())
         return current_flags().stdlib;
      return LIZ_SYSTEM_DIRECTORY + Path("/stdlib");
   }

   Path
   Translator::expand_library_path(const Path& path) const {
      auto pos = path.find('/');
      if (pos == 0 or pos == std::string::npos)
         return path;
      std::string lib_name = path.substr(0,pos);
      auto i = library_paths.find(lib_name);
      if (i == library_paths.end())
         return path;
      return i->second + path.substr(pos);
   }

   bool
   Translator::have_seen_path(const Path& path) const {
      return paths_seen.find(path) != paths_seen.end();
   }

   void
   Translator::mark_path_as_seen(const Path& path) {
      paths_seen.insert(path);
   }
}
