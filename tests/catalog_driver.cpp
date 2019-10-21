#include <fstream>
#include <string>
#include <limits>
#include <assert.h>
#include "catalog_uxsdcxx.h"

int main(int argc, char **argv){
	uxsd::catalog catalog0;
	std::ifstream is;
	is.open(argv[1]);
	assert(catalog0.load(is));
	is.close();

	std::string of_name = std::string(argv[1]) + ".generated";
	std::ofstream os;
	os.open(of_name);
	os.precision(std::numeric_limits<float>::max_digits10);
	catalog0.write(os);
	os.close();

	/* Read back the generated file. */
	uxsd::catalog catalog1;
	is.open(of_name);
	assert(catalog1.load(is));
	is.close();

	/* Compare the two memory representations. */
	assert(catalog0.books.size() == catalog1.books.size());
	for(int i=0; i<catalog0.books.size(); i++){
		auto &book0 = catalog0.books[i];
		auto &book1 = catalog1.books[i];
		assert(book0.available == book1.available);
		assert(strcmp(book0.id, book1.id) == 0);
		assert(book0.isbn == book1.isbn);
		assert(book0.genre == book1.genre);
		if(book0.title.lang != NULL && book1.title.lang != NULL){
			assert(strcmp(book0.title.lang, book1.title.lang) == 0);
		}else{
			assert(book0.title.lang == NULL);
			assert(book1.title.lang == NULL);
		}
		assert(strcmp(book0.title.value, book1.title.value) == 0);
		assert(book0.authors.size() == book1.authors.size());
		for(int j=0; j<book0.authors.size(); j++){
			auto &auth0 = book0.authors[j];
			auto &auth1 = book1.authors[j];
			if(auth0.recommends != NULL && auth1.recommends != NULL){
				assert(strcmp(auth0.recommends, auth1.recommends) == 0);
			}else{
				assert(auth0.recommends == NULL);
				assert(auth1.recommends == NULL);
			}
			assert(strcmp(auth0.name, auth1.name) == 0);
			assert(strcmp(auth0.born, auth1.born) == 0);
			assert(auth0.has_died == auth1.has_died);
			if(auth0.has_died){
				assert(strcmp(auth0.died, auth1.died) == 0);
			}
		}
	}

	std::string of_name_2 = of_name + ".2";
	os.open(of_name_2);
	os.precision(std::numeric_limits<float>::max_digits10);
	catalog1.write(os);
	os.close();
}
