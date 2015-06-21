// Todo:
//   Change Uint to an underlying vector of unsigned char

#include <vector>
#include <tuple>
#include <utility>
#include <sstream>
#include <iomanip>

#include <cpprest/http_client.h>
#include <cpprest/http_listener.h>

using http_client = web::http::client::http_client;
using http_listener = web::http::experimental::listener::http_listener;
using http_request = web::http::http_request;

// utility::string_t
// to_hex(unsigned long long val, std::size_t bits) {
//    utility::stringstream_t ss;
//    ss << std::setfill('0') << std::setw(bits/4) << std::hex << val;
//    return ss.str();
// }

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


// using Key = std::vector<std::pair<int,int>>;

// template <typename T> using Array = std::vector<T>;

// template <typename T> Array<T> mk_array(int n) {
//    std::size_t i = i < 0 ? 0 : n;
//    return { i, T() };
// }

// template <typename T> void set(Array<T>& a, int n, T x) {
//    std::size_t i = i < 0 ? 0 : n;
//    a[i] = x;
// }

// constexpr std::tuple<> empty;
// // ---------------------------------------------------------
// // -- from: `Core.liz`
// // ---------------------------------------------------------

// struct Internal {
//    Uint<32> in_port;
//    Uint<16> in_phy_port;
//    Uint<16> tunnel_id;
//    Uint<16> metadata;
// };

// struct Ethernet {
//    Uint<48> src;
//    Uint<48> dst;
//    Uint<16> typ;
// };

// struct Packet {
//    Ethernet eth;
// };

// struct Context {
//    Internal intl;
//    Packet pkt;
// };

// using MatchSet = int;

//   // ADT: Action
// // Changes: output is now a 32 bit value according to OpenFlow v1.3
// struct Action {
//    Action () : liz_tag(0), dummy(0) { }
//    int liz_tag;
//    union {
//       int dummy;
//       Uint<32> output;
//    };
// };

//   // constructor: output
// ::Action output(Uint<32> arg0) {
//    ::Action x;
//    x.liz_tag = 0;
//    x.output = arg0;
//    return x;
// }

// Uint<32> out_ALL = 0xfffffffb;

// Uint<32> out_CONTROLLER = 0xfffffffd;

// struct FlowRemovedData {
//    Uint<16> priority;
//    Uint<8> reason;
//    Uint<8> table_id;
// };

//   // ADT: Event
// struct Event {
//    Event () : liz_tag(0), dummy(0) { }
//    int liz_tag;
//    union {
//       int dummy;
//       Context PacketIn;
//       FlowRemovedData FlowRemoved;
//    };
// };

//   // constructor: PacketIn
// ::Event PacketIn(Context arg0) {
//    ::Event x;
//    x.liz_tag = 0;
//    x.PacketIn = arg0;
//    return x;
// }

//   // constructor: FlowRemoved
// ::Event FlowRemoved(FlowRemovedData arg0) {
//    ::Event x;
//    x.liz_tag = 1;
//    x.FlowRemoved = arg0;
//    return x;
// }

// Uint<48>
// get_src(const web::json::value& obj) {
//    auto arr = obj.as_array();
//    std::vector<int> v;
//    for (std::size_t i = 6; i != 12; ++i)
//       v.push_back(arr.at(i).as_integer());
//    unsigned long long val = 0;
//    int b = 0;
//    for (auto i = v.rbegin(); i != v.rend(); ++i, b += 8)
//       val += (unsigned long long)(*i) << b;
//    return Uint<48>(val);
// }

// Context
// tiny_handle_packet_in(const web::json::value& obj) {
//    Uint<32> in_port { (unsigned long long)(obj.at(U("in_port")).at(U("_value")).as_integer()) };
//    Uint<48> eth_src { get_src(obj.at("packet").at("data")) };
//    return { { in_port, 0, 0, 0 }, { eth_src, 0, 0 } };
// }

// // Not yet supported.
// FlowRemovedData
// tiny_handle_flow_removed(const web::json::value&) {
//    return { 0, 0, 0 };
// }

// ::Event parse_event(const utility::string_t& cmd, const web::json::value& obj) {
//    if (cmd == "/exception/PacketIn")
//       return PacketIn(tiny_handle_packet_in(obj));
//    else if (cmd == "/exception/FlowRemoved")
//       return FlowRemoved(tiny_handle_flow_removed(obj));
//    else {
//       abort();
//       return Event();
//    }
// }








// bool poll_kill_signal() {
//    return true;
// }

// Event poll_event() {
//    return ::FlowRemoved(FlowRemovedData{ 0, 0, 0 });
// }

// void destroy() {
//    return;
// }



// int pro_int = 0;

// int pro_eth = 1;

// int pro_vlan = 2;

// int pro_mpls = 3;

// int pro_arp = 4;

// int pro_ipv4 = 5;

// int pro_icmpv4 = 6;

// int pro_ipv6 = 7;

// int pro_icmpv6 = 8;

// int pro_tcp = 9;

// int pro_sctp = 10;

// int pro_udp = 11;

// int fld_in_port = 0;

// int fld_in_phy_port = 1;

// int fld_tunnel_id = 2;

// int fld_metadata = 3;

// int fld_src = 4;

// int fld_dst = 5;

// int fld_typ = 6;

// int fld_pcp = 7;

// int fld_vid = 8;

// int fld_label = 9;

// int fld_tc = 10;

// int fld_bos = 11;

// int fld_opcode = 12;

// int fld_sha = 13;

// int fld_spa = 14;

// int fld_tha = 15;

// int fld_tpa = 16;

// int fld_dscp = 17;

// int fld_ecn = 18;

// int fld_proto = 19;

// int fld_code = 20;

// int fld_flabel = 21;

// int fld_ttl = 22;

// // -- end: `Core.liz`

// // ---------------------------------------------------------
// // -- from: `Interface.liz`
// // ---------------------------------------------------------



































// // -- end: `Interface.liz`

// // ---------------------------------------------------------
// // -- from: `EthernetBridge.liz`
// // ---------------------------------------------------------







// Key k0 = std::vector<std::pair<int,int>>{ std::pair<int,int>(0,0), std::pair<int,int>(1,4)};

// Key k1 = std::vector<std::pair<int,int>>{ std::pair<int,int>(1,4)};

// Key k2 = std::vector<std::pair<int,int>>{ };

// Key k3 = std::vector<std::pair<int,int>>{ };

// Uint<32> get_in_port(Context ctx) {
//    Internal i = ctx.intl;
//    return i.in_port;
// }

// Uint<48> get_eth_src(Context ctx) {
//    Packet pkt = ctx.pkt;
//    Ethernet eth = pkt.eth;
//    return eth.src;
// }

// Uint<48> get_eth_dst(Context ctx) {
//    Packet pkt = ctx.pkt;
//    Ethernet eth = pkt.eth;
//    return eth.dst;
// }

// Array<Action> singleton(Action x) {
//    Array<Action> arr = mk_array<Action>(1);
//    set(arr, 0, x);
//    return arr;
// }

// std::tuple<Array<Action>, Uint<8>> to_controller = std::make_tuple<Array<Action>, Uint<8>>(singleton(::output(::out_CONTROLLER)), 1);

// std::tuple<Array<Action>> flood = std::make_tuple<Array<Action>>(singleton(::output(::out_ALL)));

// // Change: All generative typing will now be struct based!
// // And change these to strings
// struct Table__k0014_80_16__ { utility::string_t id; };
// struct Table__k14_80_16__ { utility::string_t id; };
// struct Table__k_80_16__ { utility::string_t id; };

// // Change: must get classifiers of the tables!



// struct DataPlane__k0014_k14_k_k__ {
//    std::shared_ptr<http_client> client;
//    utility::string_t switch_name;
//    Table__k0014_80_16__ tbl0;
//    Table__k14_80_16__ tbl1;
//    Table__k_80_16__ tbl2;
//    Table__k_80_16__ tbl3;
// };

// // Change: need to synthesize these
// Key get_key (Table__k0014_80_16__) { return k0; }
// Key get_key (Table__k14_80_16__) { return k1; }
// Key get_key (Table__k_80_16__) { return k2; }

// Key get_key(DataPlane__k0014_k14_k_k__ d, int i) {
//    switch (i) {
//       case 0: return get_key(d.tbl0);
//       case 1: return get_key(d.tbl1);
//       case 2: return get_key(d.tbl2);
//       case 3: return get_key(d.tbl3);
//       default: return { };
//    }
// }

// struct ApplyInst {
//    std::string protocol;
//    std::string field;
//    std::string op;
//    std::size_t value_byte_width;
//    Uint<32> value;
// };

// struct Instructions {
//    Array<ApplyInst> apply;
//    Uint<8> goto_id;
// };

// struct FlowMod {
//    std::vector<std::vector<unsigned char>> match;
//    Uint<8> table_id;
//    Uint<8> command;
//    Uint<16> idle_timeout;
//    Uint<16> priority;
//    Uint<32> buffer_id;
//    Instructions insts;
// };

// inline double as_double(unsigned char c) { return c; }

// web::json::value
// to_array_obj(const std::vector<unsigned char>& v) {
//    const std::size_t n = v.size();
//    web::json::value val_arr = web::json::value::array();
//    if (not v.empty()) {
//       val_arr[0] = web::json::value::number(as_double(v.front()));
//       for (double i = 1; i != n; ++i)
//          val_arr[i] = web::json::value::number(as_double(v[i]));
//    }
//    return val_arr;
// }

// web::json::value
// to_uint_obj(const std::vector<unsigned char>& v) {
//    const std::size_t n = v.size();
//    web::json::value obj;
//    obj[U("_value")] = to_array_obj(v);
//    obj[U("_bytes")] = web::json::value::number((double)n);
//    return obj;
// }

// // FIXME: oi...
// utility::string_t protocol_name(int) { return "internal"; }
// utility::string_t field_name(int) { return "phy_port"; }

// web::json::value
// match_to_json(DataPlane__k0014_k14_k_k__ dp, const FlowMod& f, int tbl_idx) {
//    // Look at the wildcard case
//    if (f.match.empty())
//       return web::json::value::array();
//    auto key = get_key(dp, tbl_idx);
//    const std::size_t n = key.size();
//    web::json::value arr = web::json::value::array();
//    for (std::size_t i = 0; i != n; ++i) {
//       web::json::value match;
//       match[U("protocol")] =
//          web::json::value::string(protocol_name(key[i].first));
//       match[U("field")] = web::json::value::string(field_name(key[i].second));
//       match[U("value")] = to_uint_obj(f.match[i]);
//       arr[i] = match;
//    }
//    return arr;
// }

// web::json::value
// apply_instructions_to_json(Array<ApplyInst> apps) {
//    web::json::value arr = web::json::value::array();
//    for (std::size_t i = 0; i != apps.size(); ++i) {
//       web::json::value app_inst;
//       app_inst[U("protocol")] = web::json::value::string(apps[i].protocol);
//       app_inst[U("field")] = web::json::value::string(apps[i].field);
//       app_inst[U("op")] = web::json::value::string(apps[i].op);
//       app_inst[U("value")] = to_json_value(apps[i].value);
//       arr[i] = app_inst;
//    }
//    return arr;
// }

// web::json::value
// instructions_to_json(Instructions insts) {
//    web::json::value inst_obj;
//    inst_obj[U("apply")] = apply_instructions_to_json(insts.apply);
//    if (insts.goto_id.val != 0)
//       inst_obj[U("goto_")] = to_json_value(insts.goto_id);
//    return inst_obj;
// }

web::json::value
default_command() {
   web::json::value obj;
   obj[U("_value")] = web::json::value::number(0);
   obj[U("_bytes")] = web::json::value::number(1);
   return obj;
}

// web::json::value
// default_buffer_id() {
//    web::json::value obj;
//    obj[U("_value")] = web::json::value::number((double)(0xffffffff));
//    obj[U("_bytes")] = web::json::value::number(4);
//    return obj;
// }

// web::json::value
// to_json_value(DataPlane__k0014_k14_k_k__ dp, const FlowMod& f, int tbl_idx) {
//    web::json::value obj;
//    obj[U("match")] = match_to_json(dp, f, tbl_idx);
//    obj[U("command")] = default_command();
//    obj[U("table_id")] = to_json_value(f.table_id);
//    obj[U("hard_timeout")] = to_json_value(f.idle_timeout);
//    obj[U("priority")] = to_json_value(f.priority);
//    obj[U("instructions")] = instructions_to_json(f.insts);
//    obj[U("buffer_id")] = to_json_value(f.buffer_id);
//    return obj;
// }



// void
// tiny_add_flow_mod(DataPlane__k0014_k14_k_k__ dp, int tbl_idx, FlowMod flow) {
//    utility::stringstream_t sstr;
//    sstr
//       << U("/tinynbi/switch/") << dp.switch_name << U("/tables/") << tbl_idx
//       << U("/flows");
//    web::json::value f_json = to_json_value(dp, flow, tbl_idx);
//    try {
//       dp.client->request(web::http::methods::POST, sstr.str(), f_json)
//          .then([](web::http::http_response){})
//          .wait();
//    } catch (const std::exception &e) {
//       // FIXME: Need to log this!
//       std::cerr << "Error exception: " << e.what() << '\n';
//    }
// }

// // Add to Liz
// ApplyInst
// to_Action (Action a) {
//    switch (a.liz_tag) {
//       case 0: return {
//          "internal",
//          "phy_port",
//          "set",
//          4, 
//          a.output };
//    }
//    return { "", "", "", 0, Uint<32>(0) };
// }

// Array<ApplyInst>
// to_Actions(Array<Action> acts) {
//    Array<ApplyInst> arr;
//    for (auto act: acts)
//       arr.push_back(to_Action(act));
//    return arr;
// }

// // Change: missing instruction...
// // First of c is idle_timeout
// void flow_mod_miss0(DataPlane__k0014_k14_k_k__& d, std::tuple<Array<Action>, Uint<8>> action, std::tuple<Uint<16>,Uint<16>> c)
// {
//    // Change: Liz needs to do this.
//    FlowMod f { { }, 0, 0, std::get<0>(c), std::get<1>(c), 0,
//                { to_Actions(std::get<0>(action)), std::get<1>(action) } };
//    tiny_add_flow_mod(d, 0, f);
//    return;
// }

// void flow_mod_miss1(DataPlane__k0014_k14_k_k__& dp, std::tuple<Array<Action>> insts, std::tuple<Uint<16>,Uint<16>> c)
// {
//    FlowMod f { { }, 1, 0, std::get<0>(c), std::get<1>(c), 0, { to_Actions(std::get<0>(insts)), 0 } };
//    tiny_add_flow_mod(dp, 1, f);
//    return;
// }

// void load(DataPlane__k0014_k14_k_k__ d) {
//    flow_mod_miss0(d, ::to_controller, std::make_tuple<Uint<16>,Uint<16>>(Uint<16>(0xffff),Uint<16>(0)));
//    flow_mod_miss1(d, ::flood, std::make_tuple<Uint<16>,Uint<16>>(Uint<16>(0xffff),Uint<16>(0)));
// }

// void unload(DataPlane__k0014_k14_k_k__) {
//    return;
// }

// // Change: When Uint is changed to std::char, this will go away. Me thinks.
// template <int N>
//    std::vector<unsigned char>
//    Uint_to_vec(Uint<N> n) {
//       auto val = n.val;
//       const std::size_t byte_count = N/8;
//       std::vector<unsigned char> v;
//       v.reserve(byte_count);
//       for (std::size_t i = 0; i != byte_count; ++i) {
//          v.insert(v.begin(), val%256);
//          val /= 256;
//       }
//       return v;
//    }

// bool
// packet_safe_DataPlane__k0014_k14_k_k___0_(std::tuple<Uint<32>, Uint<48>> ms, std::tuple<Uint<8>> i) {
//    std::vector<int> delta = { 0, 1 };
//    // Key safety
//    if (std::find(delta.begin(), delta.end(), 0) == delta.end())
//       return false;
//    if (std::find(delta.begin(), delta.end(), 1) == delta.end())
//       return false;
//    // Empty learn!
//     // FIXME: No instruction safety!!
//    // Goto safety
//    if (std::get<0>(i) == Uint<8>(0))
//       return true;
//    if (std::get<0>(i) <= Uint<8>(0))
//       return false;
//    switch (std::get<0>(i)) {
//       case 0:
//          if (std::find(delta.begin(), delta.end(), 0) == delta.end())
//             return false;
//          break;
//       case 1:
//          if (std::find(delta.begin(), delta.end(), 1) == delta.end())
//             return false;
//          break;
//       case 2:
//          break;
//       case 3:
//          break;
//    }
//    return true;
// }

// bool
// packet_safe_DataPlane__k0014_k14_k_k___1_(std::tuple<Uint<48>> ms, std::tuple<Array<Action>> i) {
//    std::vector<int> delta = { 0 , 1 };
//    delta.push_back(1);
//    // Key safety
//    if (std::find(delta.begin(), delta.end(), 1) == delta.end())
//       return false;
//    // Empty learn!
//     // FIXME: No instruction safety!!
//    // Goto safety
//    // No goto!
//    return true;
// }

// // First of properties is idle_timeout
// void flow_mod0(DataPlane__k0014_k14_k_k__& dp, std::tuple<Uint<32>, Uint<48>> ms, std::tuple<Uint<8>> i, std::tuple<Uint<16>,Uint<16>> properties) {
//    std::vector<std::vector<unsigned char>> m {
//       Uint_to_vec(std::get<0>(ms)),
//       Uint_to_vec(std::get<1>(ms)) };
//    FlowMod f { m, 0, 0, std::get<0>(properties), std::get<1>(properties), 0, { { }, std::get<0>(i) } };
//    tiny_add_flow_mod(dp, 0, f);
//    return;
// }

// void flow_mod1(DataPlane__k0014_k14_k_k__& dp, std::tuple<Uint<48>> ms, std::tuple<Array<Action>> insts, std::tuple<Uint<16>,Uint<16>> properties) {
//    std::vector<std::vector<unsigned char>> m { Uint_to_vec(std::get<0>(ms)) };
//    FlowMod f { m, 1, 0, std::get<0>(properties), std::get<1>(properties), 0, { to_Actions(std::get<0>(insts)) ,0 } };
//    tiny_add_flow_mod(dp, 1, f);
//    return;
// }

// void lrn_miss(DataPlane__k0014_k14_k_k__ d, Context ctx) {
//    if (packet_safe_DataPlane__k0014_k14_k_k___0_(std::make_tuple<Uint<32>, Uint<48>>(::get_in_port(ctx), ::get_eth_src(ctx)), std::make_tuple<Uint<8>>(1)))
//       flow_mod0(d, std::make_tuple<Uint<32>, Uint<48>>(::get_in_port(ctx), ::get_eth_src(ctx)), std::make_tuple<Uint<8>>(1), std::make_tuple<Uint<16>,Uint<16>>(60,100));
//    if (packet_safe_DataPlane__k0014_k14_k_k___1_(std::make_tuple<Uint<48>>(::get_eth_src(ctx)), std::make_tuple<Array<Action>>(singleton(::output(::get_in_port(ctx))))))
//       flow_mod1(d, std::make_tuple<Uint<48>>(::get_eth_src(ctx)), std::make_tuple<Array<Action>>(singleton(::output(::get_in_port(ctx)))), std::make_tuple<Uint<16>,Uint<16>>(60,100));
// }

// void flow_removed(DataPlane__k0014_k14_k_k__&, FlowRemovedData) {
//    return;
// }

// bool respond(DataPlane__k0014_k14_k_k__& d, Event ev) {
//    switch (ev.liz_tag) {
//       case 0:
//          {
//             Context& ctx = ev.PacketIn;
//             ::lrn_miss(d, ctx);
//             break;
//          }
//       case 1:
//          {
//             FlowRemovedData& flow = ev.FlowRemoved;
//             ::flow_removed(d, flow);
//             break;
//          }
      
//    }
//    return true;
// }

// // -- end: `EthernetBridge.liz`


// // CHANGE: Added



// DataPlane__k0014_k14_k_k__ initialize_data_plane() {
//    auto client = std::make_shared<http_client>(U("http://0.0.0.0:8080/"));
//    auto name = get_server_name(client);
//    return { client, name, { U("0") }, { U("1") }, { U("2") }, { U("3") } };
// }

// bool initialization_success(DataPlane__k0014_k14_k_k__& dp) {
//    return not dp.switch_name.empty();
// }

// // changed: added
// web::json::value
// extract_json_or_panic(http_request req) {
//    web::json::value obj;
//    req.extract_json()
//       .then([&obj](pplx::task<web::json::value> tsk) {
//          try { obj = tsk.get(); } catch(...) { abort(); }
//       }).wait();
//    return obj;
// }

// Changed: added
// using respond_func_t = bool(DataPlane__k0014_k14_k_k__& d, Event);
// void responder(DataPlane__k0014_k14_k_k__& d, respond_func_t respond_func) {
//    http_listener listener(U("http://0.0.0.0:9000"));
//    bool done = false;
//    listener.support(web::http::methods::POST, [&done, &d, respond_func](http_request req) {
//       std::cerr << "Got a post: "
//       auto cmd = req.request_uri().path();
//       if (cmd == "/quit") {
//          req.reply(web::http::status_codes::OK, U("terminating application"))
//             .wait();
//          done = true;
//       }
//       else if (cmd == "/echo") {
//          req.reply(web::http::status_codes::OK, U("bonjour"))
//             .wait();
//       } else {
//          req.reply(web::http::status_codes::OK);
//          auto obj = extract_json_or_panic(req);
//          auto evt = parse_event(cmd, obj);
//          respond_func(d, evt);
//       }
//    });
//    try {
//       listener.open().wait();
//       while (not done) { }
//    } catch (...) {
//       std::cerr << "listener error\n";
//    }
//    listener.close();
// }

// int exec_arbiter() {
//    DataPlane__k0014_k14_k_k__ dp = initialize_data_plane();
//    if (not initialization_success(dp))
//     return 1;
//    load(dp);
//    responder(dp, respond);
//    unload(dp);
//    ::destroy();
//    return 0;
// }

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
      client.request(web::http::methods::POST, sstr.str(), bogus);
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

