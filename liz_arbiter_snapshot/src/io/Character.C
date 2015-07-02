#include <ostream>
#include <iomanip>
#include <liz/Character>

namespace liz {
   std::ostream&
   operator<<(std::ostream& os, Character c) {
      auto v = CodePoint(c);
      if (v < 256)
         os << char(v);
      else
         os << "U+" << std::hex << v;
      return os;
   }
}
