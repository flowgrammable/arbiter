// Todo:
//   Change Uint to an underlying vector of unsigned char

#include <utility>
#include <sstream>

#include <cpprest/http_client.h>
#include <cpprest/http_listener.h>

using http_client = web::http::client::http_client;
using http_listener = web::http::experimental::listener::http_listener;
using http_request = web::http::http_request;

// -- Uint types
template <int N> struct Uint {
   // -- members
   static constexpr int bits = N;
   unsigned long long val;
   // -- interface
   Uint(unsigned long long x) : val(x) { }
   bool operator== (const Uint& rhs) { return val == rhs.val; }
   bool operator!= (const Uint& rhs) { return val != rhs.val; }
   operator unsigned long long() { return val; }
};

template <int N>
   web::json::value
   to_json_value(Uint<N> x) {
      web::json::value obj;
      obj[U("_bytes")] = web::json::value::number(N/8);
      obj[U("_value")] = web::json::value::number((double)x.val);
      return obj;
   }

web::json::value
default_command() {
   web::json::value obj;
   obj[U("_value")] = web::json::value::number(0);
   obj[U("_bytes")] = web::json::value::number(1);
   return obj;
}

utility::string_t
get_server_name(http_client& client) {
   utility::string_t name;
   client.request(web::http::methods::GET, U("/tinynbi/switch"))
      .then([](web::http::http_response response) {
         if (response.status_code() == web::http::status_codes::OK)
            return response.extract_json();
         else
            return pplx::task_from_result(web::json::value()); })
      .then([&name](pplx::task<web::json::value> prev_tsk) {
         try {
            const web::json::value& v = prev_tsk.get();
            auto x = v.at(0); 
            if (x.is_string())
               name = x.as_string();
         } catch (...) { } })
      .wait();
   return name;
}

web::json::value
bogus_flow() {
   web::json::value obj;
   obj[U("match")] = web::json::value::array();
   obj[U("command")] = default_command();
   obj[U("table_id")] = to_json_value(Uint<8>(0));
   obj[U("hard_timeout")] = to_json_value(Uint<16>(0));
   obj[U("priority")] = to_json_value(Uint<16>(0));
   obj[U("instructions")] = web::json::value::array();
   obj[U("buffer_id")] = to_json_value(Uint<32>(0));
   return obj;
}

struct flow_gen_t {
   http_client client;
   utility::string_t name;
   web::json::value bogus;

   flow_gen_t()
     : client(U("http://0.0.0.0:8080/")),
       name(get_server_name(client)),
       bogus(bogus_flow()) { }

   void send_flow() {
      utility::stringstream_t sstr;
      sstr << U("/tinynbi/switch/") << name << U("/tables/0/flows");
      client.request(web::http::methods::POST, sstr.str(), bogus)
         .then([](web::http::http_response){})
         .wait();
   }
};

int main() {
   // Build a flowmod generator
   flow_gen_t flow_gen;
   // Setup a listener.
   http_listener listener(U("http://0.0.0.0:9000"));
   bool done = false;
   listener.support(web::http::methods::POST, [&done, &flow_gen](http_request req) {
      req.reply(web::http::status_codes::OK).wait();
      auto cmd = req.request_uri().path();
      if (cmd == "/exception/PacketIn")
         flow_gen.send_flow();
   });
   listener.open().wait();
   while (not done) { }
   listener.close();
}

