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

#include <cstdlib>
#include <iterator>
#include <algorithm>
#include <fstream>

#include "Compiler.H"
#include "backend/cxx.H"

namespace liz {

   // Return a output pathname for the intermediate C++ file.
   // Note that it does not contain the file extension.
   static Path
   get_cxx_output_path(const Flags& flags, const Path& inpath) {
      if (not flags.output_file.empty())
         return flags.output_file;
      auto p = std::find(inpath.rbegin(), inpath.rend(), '.');
      return p == inpath.rend()
         ? inpath + '.'
         : Path(inpath.begin(), p.base());
   }

   static bool compile_cxx_file(const Path& path) {
      std::string cmd = std::string(tools::host::cxx_compiler)
         + " -c " + tools::host::cxx_flags + " "
         + path;
      return std::system(cmd.c_str()) == 0;
   }
      
   
   // -- Compiler --
   Compiler::Compiler(Reader& r)
         : Evaluator(r)
   { }

   Compiler::~Compiler()
   { }

   static void
   process_import(cxx::Backend& be, std::ofstream& os, const Import& imp) {
      // Print a header
      os << "// ---------------------------------------------------------\n"
         << "// -- from: `" << imp.load_unit()->path() << "`\n"
         << "// ---------------------------------------------------------\n\n";
      for (auto elab: imp.load_unit()->statements()) {
         auto tl = be.translate_top_level(elab.code());
         format_toplevel(tl, os);
         os << "\n\n";
      }
      os << "// -- end: `" << imp.load_unit()->path() << "`\n\n";
   }

   static void
   add_header(std::ofstream& os) {
      os << "#include \"liz_intrinsics.hpp\"\n\n\n\n\n" << std::endl;
   }

   struct FormatterContext {
      FormatterContext()
            : tl_types(), tl_terms(), fun_defs()
         { }

      cxx::Backend* be;
      std::vector<const cxx::Decl*> tl_types;
      std::vector<const cxx::Decl*> tl_terms;
      std::vector<const cxx::FunDef*> fun_defs;
   };

   static inline void
   push_decl(FormatterContext& ctx, const cxx::Decl* decl) {
      if (is_type_decl(decl))
         ctx.tl_types.push_back(decl);
      else
         ctx.tl_terms.push_back(decl);
   }

   static void
   push_top_level(FormatterContext& ctx, const cxx::Toplevel* tl) {
      if (tl == nullptr)
         return;
      // This will always be a top level decl.
      if (auto topdecl = dynamic_cast<const cxx::TopDecl*>(tl)) {
         auto decl = topdecl->declaration();
         if (auto decl_pack = dynamic_cast<const cxx::DeclPack*>(decl)) {
            for (auto spec: decl_pack->specializations()) {
               if (auto fundef = dynamic_cast<const cxx::FunDef*>(spec))
                  ctx.fun_defs.push_back(fundef);
               else
                  push_decl(ctx, spec);
            }
            push_decl(ctx, decl_pack->primary());
         } else {
            push_decl(ctx, decl);
         }
      } else
         internal_error("formatter: found a non-declaration top level statement");
   }

   static void
   format_cxx(FormatterContext& ctx, std::ostream& os) {
      os << "// -- Types\n";
      for (auto decl: ctx.tl_types) {
         if (decl != nullptr) {
            format(decl, os);
            os << "\n\n";
         }
      }
      os << "// -- Forward decls\n";
      for (auto fun_def: ctx.fun_defs) {
         format_forward_decl(fun_def, os);
         os << '\n';
      }
      os << '\n';
      os << "// -- Terms\n";
      for (auto decl: ctx.tl_terms) {
         if (decl != nullptr) {
            format(decl, os);
            os << "\n\n";
         }
      }
      for (auto decl: ctx.fun_defs) {
         if (decl != nullptr) {
            format(decl, os);
            os << "\n\n";
         }
      }
   }

   bool
   Compiler::process_file(const Path& path, const Flags& flags) {
      FlagsManager new_flags(*this, flags);
      cxx::Backend backend(*this);
      Path output_path = get_cxx_output_path(flags, path) + "cpp";
      std::ofstream output(output_path.c_str());
      add_header(output);
      const SourceFileAst* src = reader()->read_file(path, flags);
      FormatterContext ctx;
      for (auto ast : src->asts) {
         const Expression* expr = elaborate(ast).code();
         if (auto imp = is<Import>(expr))
            process_import(backend, output, *imp);
         else if (expr != nullptr) {
            if (auto tl = backend.translate_top_level(expr))
               push_top_level(ctx, tl);
         }
      }
      for (auto x: get_specializations()) {
         auto decl = specs_to_decl(backend, x);
         if (auto fundef = dynamic_cast<const cxx::FunDef*>(decl))
            ctx.fun_defs.push_back(fundef);
         else
            push_decl(ctx, decl);
      }
      format_cxx(ctx, output);
      if (not compile_cxx_file(output_path))
         internal_error("intermediate C++ translation failed to compile");
      return true;
   }
}
