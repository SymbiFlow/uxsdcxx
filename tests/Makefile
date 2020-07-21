TESTS := hello
CFLAGS := -O0 -g -I pugixml/src/ --std=c++11

all: $(TESTS)

# so that we don't run tests which have already passed
%: %.xsd %_driver.cpp $(shell find ../uxsdcxx/)
	uxsdcxx $@.xsd
	g++ $(CFLAGS) pugixml/src/pugixml.cpp $@_driver.cpp -o $@.test
	./$@.test $@.xml
	diff $@.xml.generated $@.xml.generated.2
	echo "ok" > $@

clean:
	rm *.generated* *_uxsdcxx.cpp *_uxsdcxx.h *.test $(TESTS)
