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

   const char* file_header =
"#include <vector>\n"
"#include <tuple>\n"
"#include <utility>\n"
"#include <sstream>\n"
"#include <iomanip>\n"
"\n"
"#include <cpprest/http_client.h>\n"
"#include <cpprest/http_listener.h>\n"
"\n"
"using http_client = web::http::client::http_client;\n"
"using http_listener = web::http::experimental::listener::http_listener;\n"
"using http_request = web::http::http_request;\n"
"\n"
"// -- Uint types\n"
"template <int N> struct Uint {\n"
"   // -- members\n"
"   static constexpr int bits = N;\n"
"   unsigned long long val;\n"
"   // -- interface\n"
"   Uint(unsigned long long x) : val(x) { }\n"
"   bool operator== (const Uint& rhs) { return val == rhs.val; }\n"
"   bool operator!= (const Uint& rhs) { return val != rhs.val; }\n"
"   operator unsigned long long() { return val; }\n"
"};\n"
"\n"
"template <int N>\n"
"   web::json::value\n"
"   to_json_value(Uint<N> x) {\n"
"      web::json::value obj;\n"
"      obj[U(\"_bytes\")] = web::json::value::number(N/8);\n"
"      obj[U(\"_value\")] = web::json::value::number((double)x.val);\n"
"      return obj;\n"
"   }\n"
"\n"
"using Key = std::vector<std::pair<int,int>>;\n"
"\n"
"template <typename T> using Array = std::vector<T>;\n"
"\n"
"template <typename T> Array<T> mk_array(int n) {\n"
"   std::size_t i = i < 0 ? 0 : n;\n"
"   return Array<T>(i, T());\n"
"}\n"
"\n"
"template <typename T> void set(Array<T>& a, int n, T x) {\n"
"   std::size_t i = i < 0 ? 0 : n;\n"
"   a[i] = x;\n"
"}\n"
"\n"
"constexpr std::tuple<> empty;\n"
"\n\n"
"struct TinyClient {\n"
"   std::shared_ptr<http_client> client;\n"
"   utility::string_t switch_name;\n"
"};\n"
"\n"
"template <typename X>\n"
"   Array<char> to_bytes(const X& x) {\n"
"      const std::size_t n = sizeof(X);\n"
"      Array<char> arr(n);\n"
"      std::copy(static_cast<const char*>(static_cast<const void*>(&x)),\n"
"                static_cast<const char*>(static_cast<const void*>(&x)) + n,\n"
"                arr.begin());\n"
"      return arr;\n"
"   }\n"
"\n"
"template <size_t... n>\n"
"   struct ct_integers_list {\n"
"       template <size_t m>\n"
"       struct push_back\n"
"       {\n"
"           typedef ct_integers_list<n..., m> type;\n"
"       };\n"
"   };\n"
"\n"
"template <size_t max>\n"
"   struct ct_iota_1\n"
"   {\n"
"       typedef typename ct_iota_1<max-1>::type::template push_back<max>::type type;\n"
"   };\n"
"\n"
"template <>\n"
"   struct ct_iota_1<0>\n"
"   {\n"
"       typedef ct_integers_list<> type;\n"
"   };\n"
"\n"
"template <size_t... indices, typename Tuple>\n"
"   auto tuple_subset(const Tuple& tpl, ct_integers_list<indices...>)\n"
"       -> decltype(std::make_tuple(std::get<indices>(tpl)...))\n"
"   {\n"
"       return std::make_tuple(std::get<indices>(tpl)...);\n"
"   }\n"
"\n"
"template <typename Head, typename... Tail>\n"
"   std::tuple<Tail...> tuple_tail(const std::tuple<Head, Tail...>& tpl)\n"
"   {\n"
"       return tuple_subset(tpl, typename ct_iota_1<sizeof...(Tail)>::type());\n"
"   }\n"
"\n"
"void\n"
"push_to_match(std::tuple<>, std::vector<std::vector<char>>&) { }\n"
"\n"
"template <typename T>\n"
"   void\n"
"   push_to_match(std::tuple<T> t, std::vector<std::vector<char>>& ms) {\n"
"      ms.push_back(to_bytes(std::get<0>(t)));\n"
"   }\n"
"\n"
"template <typename T, typename... Args>\n"
"   void\n"
"   push_to_match(std::tuple<T, Args...> t,\n"
"                 std::vector<std::vector<char>>& ms)\n"
"   {\n"
"      ms.push_back(to_bytes(std::get<0>(t)));\n"
"      push_to_match(tuple_tail(t), ms);\n"
"   }\n"
"\n"
"template<typename... Args>\n"
"   std::vector<std::vector<char>>\n"
"   to_match(const std::tuple<Args...>& t) {\n"
"      std::vector<std::vector<char>> ms;\n"
"      ms.reserve(std::tuple_size<std::tuple<Args...>>::value);\n"
"      push_to_match(t, ms);\n"
"      return ms;\n"
"   }\n"
"\n"
"struct TinyFlowMod {\n"
"   std::vector<std::vector<char>> match_set;\n"
"   Uint<8> command;\n"
"   Uint<16> priority;\n"
"   Uint<16> hard_timeout;\n"
"   Uint<32> buffer_id;\n"
"   Array<Uint<32>> apply;\n"
"   Uint<8> goto_id;\n"
"};\n"
"\n"
"using Config = std::tuple<Uint<16>,Uint<16>>;\n"
"template <typename X>\n"
"TinyFlowMod tiny_make_flow_mod(const X& x, const Config& c, const std::tuple<Uint<8>>& goto_id) {\n"
"   return { to_match(x), 0, std::get<0>(c), std::get<1>(c), 0, { }, std::get<0>(goto_id) };\n"
"}\n"
"\n"
"template <typename X, typename Act>\n"
"TinyFlowMod tiny_make_flow_mod(const X& x, const Config& c, const std::tuple<Array<Act>>& acts)\n"
"{\n"
"   Array<Uint<32>> outs;\n"
"   outs.reserve(std::get<0>(acts).size());\n"
"   for (auto act: std::get<0>(acts))\n"
"      outs.push_back(act.output);\n"
"   return { to_match(x), 0, std::get<0>(c), std::get<1>(c), 0, outs, 0 };\n"
"}\n"
"\n"
"template <typename X, typename Act>\n"
"TinyFlowMod tiny_make_flow_mod(const X& x, const Config& c, const std::tuple<Array<Act>, Uint<8>>& insts)\n"
"{\n"
"   Array<Uint<32>> outs;\n"
"   outs.reserve(std::get<0>(insts).size());\n"
"   for (auto act: std::get<0>(insts))\n"
"      outs.push_back(act.output);\n"
"   return { to_match(x), 0, std::get<0>(c), std::get<1>(c), 0, outs, std::get<1>(insts) };\n"
"}\n"
"\n"
"// Extend.\n"
"utility::string_t protocol_name(int) { return \"internal\"; }\n"
"utility::string_t field_name(int) { return \"phy_port\"; }\n"
"\n"
"web::json::value\n"
"default_command() {\n"
"   web::json::value obj;\n"
"   obj[U(\"_value\")] = web::json::value::number(0);\n"
"   obj[U(\"_bytes\")] = web::json::value::number(1);\n"
"   return obj;\n"
"}\n"
"\n"
"web::json::value\n"
"default_buffer_id() {\n"
"   web::json::value obj;\n"
"   obj[U(\"_value\")] = web::json::value::number((double)(0xffffffff));\n"
"   obj[U(\"_bytes\")] = web::json::value::number(4);\n"
"   return obj;\n"
"}\n"
"\n"
"inline double as_double(char c) { return c; }\n"
"\n"
"web::json::value\n"
"to_array_obj(const std::vector<char>& v) {\n"
"   const std::size_t n = v.size();\n"
"   web::json::value val_arr = web::json::value::array();\n"
"   if (not v.empty()) {\n"
"      val_arr[0] = web::json::value::number(as_double(v.front()));\n"
"      for (double i = 1; i != n; ++i)\n"
"         val_arr[i] = web::json::value::number(as_double(v[i]));\n"
"   }\n"
"   return val_arr;\n"
"}\n"
"\n"
"web::json::value\n"
"to_uint_obj(const std::vector<char>& v) {\n"
"   const std::size_t n = v.size();\n"
"   web::json::value obj;\n"
"   obj[U(\"_value\")] = to_array_obj(v);\n"
"   obj[U(\"_bytes\")] = web::json::value::number((double)n);\n"
"   return obj;\n"
"}\n"
"\n"
"Key get_key(int);\n"
"\n"
"web::json::value\n"
"match_to_json(int tbl_idx, const TinyFlowMod& f) {\n"
"   if (f.match_set.empty())\n"
"      return web::json::value::array();\n"
"   auto key = get_key(tbl_idx);\n"
"   const std::size_t n = key.size();\n"
"   web::json::value arr = web::json::value::array();\n"
"   for (std::size_t i = 0; i != n; ++i) {\n"
"      web::json::value match;\n"
"      match[U(\"protocol\")] =\n"
"         web::json::value::string(protocol_name(key[i].first));\n"
"      match[U(\"field\")] = web::json::value::string(field_name(key[i].second));\n"
"      match[U(\"value\")] = to_uint_obj(f.match_set[i]);\n"
"      arr[i] = match;\n"
"   }\n"
"   return arr;\n"
"}\n"
"\n"
"web::json::value\n"
"apply_instructions_to_json(Array<Uint<32>> apps) {\n"
"   web::json::value arr = web::json::value::array();\n"
"   for (std::size_t i = 0; i != apps.size(); ++i) {\n"
"      web::json::value app_inst;\n"
"      app_inst[U(\"protocol\")] = web::json::value::string(U(\"internal\"));\n"
"      app_inst[U(\"field\")] = web::json::value::string(U(\"phy_port\"));\n"
"      app_inst[U(\"op\")] = web::json::value::string(U(\"set\"));\n"
"      app_inst[U(\"value\")] = to_json_value(apps[i]);\n"
"      arr[i] = app_inst;\n"
"   }\n"
"   return arr;\n"
"}\n"
"\n"
"web::json::value\n"
"instructions_to_json(const TinyFlowMod& f) {\n"
"   web::json::value inst_obj;\n"
"   inst_obj[U(\"apply\")] = apply_instructions_to_json(f.apply);\n"
"   if (f.goto_id.val != 0)\n"
"      inst_obj[U(\"goto_id\")] = to_json_value(f.goto_id);\n"
"   return inst_obj;\n"
"}\n"
"\n"
"web::json::value\n"
"to_json_value(int tbl_idx, const TinyFlowMod& f) {\n"
"   web::json::value obj;\n"
"   obj[U(\"match\")] = match_to_json(tbl_idx, f);\n"
"   obj[U(\"command\")] = default_command();\n"
"   obj[U(\"table_id\")] = to_json_value(Uint<8>(tbl_idx));\n"
"   obj[U(\"hard_timeout\")] = to_json_value(f.hard_timeout);\n"
"   obj[U(\"priority\")] = to_json_value(f.priority);\n"
"   obj[U(\"instructions\")] = instructions_to_json(f);\n"
"   obj[U(\"buffer_id\")] = to_json_value(f.buffer_id);\n"
"   return obj;\n"
"}\n"
"\n"
"void tiny_add_flow(TinyClient tiny_client, int tbl_idx, const TinyFlowMod& f) {\n"
"   utility::stringstream_t sstr;\n"
"   sstr\n"
"      << U(\"/tinynbi/switch/\") << tiny_client.switch_name << U(\"/tables/\")\n"
"      << tbl_idx << U(\"/flows\");\n"
"   web::json::value f_json = to_json_value(tbl_idx, f);\n"
"   try {\n"
"      tiny_client.client->request(web::http::methods::POST, sstr.str(), f_json)\n"
"         .then([](web::http::http_response){})\n"
"         .wait();\n"
"   } catch (const std::exception &e) {\n"
"      // FIXME: Need to log this!\n"
"      std::cerr << \"Error exception: \" << e.what() << '\\n';\n"
"   }\n"
"}\n"
"\n"
"utility::string_t\n"
"get_server_name(std::shared_ptr<http_client> client) {\n"
"   utility::string_t name;\n"
"   client->request(web::http::methods::GET, U(\"/tinynbi/switch\"))\n"
"      .then([](web::http::http_response response) {\n"
"         if (response.status_code() == web::http::status_codes::OK)\n"
"            return response.extract_json();\n"
"         else\n"
"            return pplx::task_from_result(web::json::value()); })\n"
"      .then([&name](pplx::task<web::json::value> prev_tsk) {\n"
"         try {\n"
"            const web::json::value& v = prev_tsk.get();\n"
"            auto x = v.at(0); \n"
"            if (x.is_string())\n"
"               name = x.as_string();\n"
"         } catch (...) { } })\n"
"      .wait();\n"
"   return name;\n"
"}\n"
"\n"
"TinyClient tiny_acquire_client() {\n"
"   auto client = std::make_shared<http_client>(U(\"http://0.0.0.0:8080/\"));\n"
"   auto name = get_server_name(client);\n"
"   return { client, name };\n"
"}\n"
"\n"
"web::json::value\n"
"extract_json_or_panic(http_request req) {\n"
"   web::json::value obj;\n"
"   req.extract_json()\n"
"      .then([&obj](pplx::task<web::json::value> tsk) {\n"
"         try { obj = tsk.get(); } catch(...) { abort(); }\n"
"      }).wait();\n"
"   return obj;\n"
"}\n"
"\n"
"struct Event;\n"
"\n"
"Event parse_event(const utility::string_t&, const web::json::value&);\n"
"\n"
"template <typename DataPath, typename Respond_t>\n"
"   void tiny_responder(DataPath& dp, Respond_t respond_func) {\n"
"      http_listener listener(U(\"http://0.0.0.0:9000\"));\n"
"      bool done = false;\n"
"      listener.support(web::http::methods::POST, [&done, &dp, respond_func](http_request req) {\n"
"         auto cmd = req.request_uri().path();\n"
"         if (cmd == \"/quit\") {\n"
"            req.reply(web::http::status_codes::OK, U(\"terminating application\"))\n"
"               .wait();\n"
"            done = true;\n"
"         }\n"
"         else if (cmd == \"/echo\") {\n"
"            req.reply(web::http::status_codes::OK, U(\"bonjour\"))\n"
"               .wait();\n"
"         } else {\n"
"            req.reply(web::http::status_codes::OK);\n"
"            auto obj = extract_json_or_panic(req);\n"
"            auto evt = parse_event(cmd, obj);\n"
"            respond_func(dp, evt);\n"
"         }\n"
"      });\n"
"      try {\n"
"         listener.open().wait();\n"
"         while (not done) { }\n"
"      } catch (...) {\n"
"      }\n"
"      listener.close();\n"
"   }\n"
;


   static void
   add_header(std::ofstream& os) {
      os << file_header << "\n\n\n\n\n" << std::endl;
   }

   // FIXME: Must remove.
   static void
   add_footer(std::ofstream& os) {
      os << "Key get_key(int tbl_idx) {\n"
         << "   if (tbl_idx == 0)\n"
         << "      return k0;\n"
         << "   else\n"
         << "      return k1;\n"
         << "}\n\n";
      os << "Uint<48>\n"
         << "get_src(const web::json::value& obj) {\n"
         << "   auto arr = obj.as_array();\n"
         << "   std::vector<int> v;\n"
         << "   for (std::size_t i = 6; i != 12; ++i)\n"
         << "      v.push_back(arr.at(i).as_integer());\n"
         << "   unsigned long long val = 0;\n"
         << "   int b = 0;\n"
         << "   for (auto i = v.rbegin(); i != v.rend(); ++i, b += 8)\n"
         << "      val += (unsigned long long)(*i) << b;\n"
         << "   return Uint<48>(val);\n"
         << "}\n"
         << "\n"
         << "Context\n"
         << "tiny_handle_packet_in(const web::json::value& obj) {\n"
         << "   Uint<32> in_port { (unsigned long long)(obj.at(U(\"in_port\")).at(U(\"_value\")).as_integer()) };\n"
         << "   Uint<48> eth_src { get_src(obj.at(\"packet\").at(\"data\")) };\n"
         << "   return { { in_port, 0, 0, 0 }, { eth_src, 0, 0 } };\n"
         << "}\n"
         << "\n"
         << "Event parse_event(const utility::string_t& cmd, const web::json::value& obj)\n"
         << "{\n"
         << "   if (cmd == \"/exception/PacketIn\")\n"
         << "      return PacketIn(tiny_handle_packet_in(obj));\n"
         << "   else {\n"
         << "      return Event();\n"
         << "   }\n"
         << "}";
   }

   bool
   Compiler::process_file(const Path& path, const Flags& flags) {
      FlagsManager new_flags(*this, flags);
      cxx::Backend backend(*this);
      Path output_path = get_cxx_output_path(flags, path) + "cpp";
      std::ofstream output(output_path.c_str());
      add_header(output);
      const SourceFileAst* src = reader()->read_file(path, flags);
      for (auto ast : src->asts) {
         const Expression* expr = elaborate(ast).code();
         if (auto imp = is<Import>(expr))
            process_import(backend, output, *imp);
         else if (expr != nullptr) {
            format_toplevel(backend.translate_top_level(expr), output);
            output << "\n\n";
         }
      }
      add_footer(output);
      if (not compile_cxx_file(output_path))
         internal_error("intermediate C++ translation failed to compile");
      return true;
   }
}
