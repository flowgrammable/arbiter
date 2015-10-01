

all: header less_safe_bridge

header:
	@echo ""
	@echo "====================================="
	@echo "== Building an arbiter application =="
	@echo "====================================="
	@echo ""

less_safe_bridge: less_safe_bridge.o
	${CXX} less_safe_bridge.o -L/usr/local/lib -lcpprest -pthread -lgmp -o less_safe_bridge

less_safe_bridge.o: less_safe_bridge.cpp
	${CXX} -std=c++11 -Wall -pedantic -g -gstabs -Wextra -c less_safe_bridge.cpp
