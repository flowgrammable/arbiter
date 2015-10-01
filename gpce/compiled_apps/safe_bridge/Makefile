

all: header safe_bridge

header:
	@echo ""
	@echo "====================================="
	@echo "== Building an arbiter application =="
	@echo "====================================="
	@echo ""

safe_bridge: safe_bridge.o
	${CXX} safe_bridge.o -L/usr/local/lib -lcpprest -pthread -lgmp -o safe_bridge

safe_bridge.o: safe_bridge.cpp
	${CXX} -std=c++11 -Wall -pedantic -g -gstabs -Wextra -c safe_bridge.cpp
