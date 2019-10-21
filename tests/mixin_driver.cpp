#include <fstream>
#include <string>
#include <limits>
#include <assert.h>
#include "mixin_uxsdcxx.h"

int main(int argc, char **argv){
	uxsd::root root0;
	std::ifstream is;
	is.open(argv[1]);
	assert(root0.load(is));
	is.close();

	std::string of_name = std::string(argv[1]) + ".generated";
	std::ofstream os;
	os.open(of_name);
	os.precision(std::numeric_limits<float>::max_digits10);
	root0.write(os);
	os.close();

	/* Read back the generated file. */
	uxsd::root root1;
	is.open(of_name);
	assert(root1.load(is));
	is.close();

	/* Compare the two memory representations. */
	assert(root0.a == root1.a);
	assert(root0.b == root1.b);

	std::string of_name_2 = of_name + ".2";
	os.open(of_name_2);
	os.precision(std::numeric_limits<float>::max_digits10);
	root1.write(os);
	os.close();
}
