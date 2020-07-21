#include <fstream>
#include <iostream>
#include <string>
#include <vector>
#include <assert.h>
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
	inline void add_hello_name(const char * name, void *& /*ctx*/){
		this->names.push_back(name);
	}
	inline size_t num_hello_name(void *& /*ctx*/){
		return this->names.size();
	}
	inline const char * get_hello_name(int n, void *& /*ctx*/){
		return this->names[n].c_str();
	}
};

int main(int argc, char **argv){
	HelloSerializer hello;
	void *context;
	std::ifstream f("hello.xml");
	uxsd::load_hello_xml(hello, context, "hello.xml", f);

	/* $greet $people. */
	std::cout << hello.greeting;
	for(auto& name : hello.names){
		std::cout << " " << name;
	}
	std::cout << "!" << std::endl;
}
