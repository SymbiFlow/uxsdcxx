TESTS=hello catalog mixin orange
all: hello catalog mixin orange

# so that we don't run tests which have already passed
%: %.xsd %_driver.cpp $(shell find ../uxsdcxx/) ../uxsdcxx.py ../uxsdcap.py
	python3 ../uxsdcxx.py $@.xsd
	g++ -O0 -g -I pugixml/src/ pugixml/src/pugixml.cpp $@_uxsdcxx.cpp $@_driver.cpp -o $@.test
	./$@.test $@.xml
	diff $@.xml.generated $@.xml.generated.2
	echo "ok" > $@

clean:
	rm *.generated* *_uxsdcxx.cpp *_uxsdcxx.h *.test $(TESTS)
