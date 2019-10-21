#include <fstream>
#include <string>
#include <limits>
#include <assert.h>
#include "hello_uxsdcxx.h"

int main(int argc, char **argv){
	uxsd::hello hello0;
	std::ifstream is;
	is.open(argv[1]);
	assert(hello0.load(is));
	is.close();

	std::string of_name = std::string(argv[1]) + ".generated";
	std::ofstream os;
	os.open(of_name);
	os.precision(std::numeric_limits<float>::max_digits10);
	hello0.write(os);
	os.close();

	/* Read back the generated file. */
	uxsd::hello hello1;
	is.open(of_name);
	assert(hello1.load(is));
	is.close();

	/* Compare the two memory representations. */
	assert(strcmp(hello0.greeting, hello1.greeting) == 0);
	assert(hello0.names.size() == hello1.names.size());
	for(int i=0; i<hello0.names.size(); i++){
		auto& name0 = hello0.names[i];
		auto& name1 = hello1.names[i];
		assert(strcmp(name0, name1) == 0);
	}

	std::string of_name_2 = of_name + ".2";
	os.open(of_name_2);
	os.precision(std::numeric_limits<float>::max_digits10);
	hello1.write(os);
	os.close();
}
