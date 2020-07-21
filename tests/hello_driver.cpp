#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <assert.h>
#include "hello_uxsdcxx.h"
#include "hello_uxsdcxx_interface.h"

class HelloSerializer: public uxsd::HelloBase<uxsd::DefaultHelloContextTypes> {
public:
	std::string greeting;
	std::vector<std::string> names;

	HelloSerializer(){}
	void start_load(const std::function<void(const char*)> *report_error){}
	void finish_load(){}
	void start_write(){}
	void finish_write(){}
	void error_encountered(const char * file, int line, const char *message){
		printf("%s:%d %s", file, line, message);
		throw std::runtime_error("Error while reading file.");
	}
	inline void set_hello_greeting(const char * greeting, void *& /*ctx*/){
		this->greeting = greeting;
	}
	inline const char * get_hello_greeting(void *& /*ctx*/){
		return this->greeting.c_str();
	}
	inline void preallocate_hello_name(void *& /*ctx*/, size_t size){}
	inline void add_hello_name(const char * name, void *& /*ctx*/){
		this->names.push_back(name);
	}
	inline size_t num_hello_name(void *& /*ctx*/){
		return this->names.size();
	}
	inline const char * get_hello_name(size_t n, void *& /*ctx*/){
		return this->names[n].c_str();
	}
};

int main(int argc, char **argv){
	HelloSerializer hello0;
	void *context;
	std::ifstream is;
	is.open(argv[1]);
	uxsd::load_hello_xml(hello0, context, argv[1], is);
	is.close();

	/* Write out. */
	std::string of_name = std::string(argv[1]) + ".generated";
	std::ofstream os;
	os.open(of_name);
	uxsd::write_hello_xml(hello0, context, os);
	os.close();

	/* Read back the generated file. */
	HelloSerializer hello1;
	is.open(of_name);
	uxsd::load_hello_xml(hello1, context, of_name.c_str(), is);
	is.close();

	/* Write out again. */
	std::string of_name2 = std::string(argv[1]) + ".generated.2";
	os.open(of_name2);
	uxsd::write_hello_xml(hello1, context, os);
	os.close();

	assert(hello0.greeting == hello1.greeting);
	for(size_t i=0; i<hello0.names.size(); i++){
		assert(hello0.names[i] == hello1.names[i]);
	}
}
