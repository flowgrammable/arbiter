

all: header echo

header:
	@echo ""
	@echo "====================================="
	@echo "== Building an arbiter application =="
	@echo "====================================="
	@echo ""

echo: echo.o
	${CXX} echo.o -L/usr/local/lib -lcpprest -pthread -lgmp -o echo

echo.o: echo.cpp
	${CXX} -std=c++11 -Wall -pedantic -g -gstabs -Wextra -c echo.cpp
