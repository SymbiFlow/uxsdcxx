#include <fstream>
#include <string>
#include <limits>
#include <assert.h>
#include "orange_uxsdcxx.h"

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
	assert(root0.records.size() == root1.records.size());
	for(int i=0; i<root0.records.size(); i++){
		auto& record0 = root0.records[i];
		auto& record1 = root1.records[i];
		assert(record0.apple == record1.apple);
		assert(record0.orange == record1.orange);
		assert(record0.int_ == record1.int_);
		assert(record0.double_ == record1.double_);
		assert(record0.has_string == record1.has_string);
		if(record0.has_string)
			assert(strcmp(record0.string, record1.string) == 0);
		assert(record0.has_choice1 == record1.has_choice1);
		if(record0.has_choice1)
			assert(strcmp(record0.choice1, record1.choice1) == 0);
		assert(record0.has_choice2 == record1.has_choice2);
		if(record0.has_choice2)
			assert(strcmp(record0.choice2, record1.choice2) == 0);
		assert(record0.has_choice3 == record1.has_choice3);
		if(record0.has_choice3)
			assert(strcmp(record0.choice3, record1.choice3) == 0);
		assert(record0.has_choice4 == record1.has_choice4);
		if(record0.has_choice4)
			assert(strcmp(record0.choice4, record1.choice4) == 0);
		assert(record0.enum_ == record1.enum_);
	}

	std::string of_name_2 = of_name + ".2";
	os.open(of_name_2);
	os.precision(std::numeric_limits<float>::max_digits10);
	root1.write(os);
	os.close();
}
