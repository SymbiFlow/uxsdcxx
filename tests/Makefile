TESTS := hello catalog
CFLAGS := -O0 -g -I pugixml/src/ --std=c++11

all: $(TESTS) coverage_badge

# so that we don't run tests which have already passed
%: %.xsd %_driver.cpp $(shell find ../uxsdcxx/)
	coverage run --source ../uxsdcxx/ $(shell which uxsdcxx) $@.xsd
	g++ $(CFLAGS) pugixml/src/pugixml.cpp $@_driver.cpp -o $@.test
	./$@.test $@.xml
	diff $@.xml.generated $@.xml.generated.2
	echo "ok" > $@

coverage_badge: $(TESTS)
	coverage-badge -f -o ../coverage.svg

clean:
	rm *.generated* *_uxsdcxx.h *_uxsdcxx_interface.h *.test $(TESTS)
