#include <fstream>
#include <iostream>
#include <string>
#include <limits>
#include <assert.h>
#include "catalog_uxsdcxx.h"
#include "catalog_uxsdcxx_interface.h"

/* Context types for serializer. */
struct CatalogContextTypes {
	using CatalogReadContext = void *;
	using BookReadContext = size_t;
	using AuthorReadContext = size_t;
	using TitleReadContext = size_t;
	using CatalogWriteContext = void *;
	using BookWriteContext = size_t;
	using AuthorWriteContext = size_t;
	using TitleWriteContext = size_t;
};

class CatalogSerializer: public uxsd::CatalogBase<CatalogContextTypes>
{
public:
	struct Author {
		std::string name;
		std::string recommends;
		std::string born;
		std::string died;
	};
	struct Book {
		bool available;
		std::string id;
		unsigned int isbn;
		std::string title;
		std::string title_lang;
		uxsd::enum_genre genre;
		std::vector<Author> authors;
	};
	std::vector<Book> books;
	size_t current_book;

	~CatalogSerializer(){}
	void start_load(const std::function<void(const char*)> *report_error){}
	void finish_load(){}
	void start_write(){}
	void finish_write(){}
	void error_encountered(const char * file, int line, const char *message){
		std::cerr << file << ":" << line << ": " << message << std::endl;
		throw std::runtime_error("Error while reading file.");
	}

	/** Generated for complex type "catalog":
	 * <xs:complexType name="catalog">
	 *   <xs:sequence>
	 *     <xs:element name="book" type="book" maxOccurs="unbounded" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void preallocate_catalog_book(void *& /*ctx*/, size_t size){}
	/* Add a book to this catalog.
	 * Return a size_t offset as BookWriteContext. */
	inline size_t add_catalog_book(void *& /*ctx*/, bool available){
		Book book;
		book.available = available;
		books.push_back(book);
		return books.size() - 1;
	}
	inline void finish_catalog_book(size_t & /*ctx*/){}

	/* How many books are there? */
	inline size_t num_catalog_book(void *& /*ctx*/){
		return books.size();
	}
	/* What is a handle to book number n?
	 * In our case, it's just n.
	 * If performance is an issue you can return Book *. */
	inline size_t get_catalog_book(size_t n, void *& /*ctx*/){
		return n;
	}

	/** Generated for complex type "book":
	 * <xs:complexType name="book">
	 *   <xs:sequence>
	 *     <xs:element name="isbn" type="isbn" />
	 *     <xs:element name="title" type="title" />
	 *     <xs:element name="genre" type="genre" />
	 *     <xs:element name="author" type="author" maxOccurs="unbounded" />
	 *   </xs:sequence>
	 *   <xs:attribute name="available" type="xsd:boolean" use="required" />
	 *   <xs:attribute name="id" type="xsd:ID" use="required" />
	 * </xs:complexType>
	*/

	/* `available` is a non-string required attribute,
	 * so it is set in the `add_catalog_book` function. */
	inline bool get_book_available(size_t &n){
		return books[n].available;
	}
	inline const char * get_book_id(size_t &n){
		return books[n].id.c_str();
	}
	inline void set_book_id(const char * id, size_t &n){
		books[n].id = id;
	}

	inline void set_book_isbn(unsigned int isbn, size_t &n){
		books[n].isbn = isbn;
	}
	inline unsigned int get_book_isbn(size_t &n){
		return books[n].isbn;
	}

	inline size_t init_book_title(size_t &n){
		return n;
	}
	inline void finish_book_title(size_t &/*ctx*/){}
	inline size_t get_book_title(size_t &n){
		return n;
	}

	inline void set_book_genre(uxsd::enum_genre genre, size_t &n){
		books[n].genre = genre;
	}
	inline uxsd::enum_genre get_book_genre(size_t &n){
		return books[n].genre;
	}

	/* Keep track of the current book for the functions
	 * filling in data about the current book's author. */
	inline void preallocate_book_author(size_t &n, size_t size){}
	inline size_t add_book_author(size_t &n){
		Author author;
		current_book = n;
		books[n].authors.push_back(author);
		return books[n].authors.size() - 1;
	}
	inline void finish_book_author(size_t &n){}
	inline size_t num_book_author(size_t &n){
		return books[n].authors.size();
	}
	inline size_t get_book_author(size_t n, size_t &book_n){
		current_book = book_n;
		return n;
	}

	/** Generated for complex type "author":
	 * <xs:complexType name="author">
	 *   <xs:complexContent>
	 *     <xs:extension base="person">
	 *       <xs:attribute name="recommends" type="xsd:IDREF" />
	 *     </xs:extension>
	 *   </xs:complexContent>
	 * </xs:complexType>
	*/
	inline const char * get_author_recommends(size_t &n){
		return books[current_book].authors[n].recommends.c_str();
	}
	inline void set_author_recommends(const char * recommends, size_t &n){
		books[current_book].authors[n].recommends = recommends;
	}
	inline void set_author_name(const char * name, size_t &n){
		books[current_book].authors[n].name = name;
	}
	inline const char * get_author_name(size_t &n){
		return books[current_book].authors[n].name.c_str();
	}
	inline void set_author_born(const char * born, size_t &n){
		books[current_book].authors[n].born = born;
	}
	inline const char * get_author_born(size_t &n){
		return books[current_book].authors[n].born.c_str();
	}
	inline void set_author_died(const char * died, size_t &n){
		books[current_book].authors[n].died = died;
	}
	inline const char * get_author_died(size_t &n){
		return books[current_book].authors[n].died.c_str();
	}

	/** Generated for complex type "title":
	 * <xs:complexType name="title">
	 *   <xs:simpleContent>
	 *     <xs:extension base="xsd:string">
	 *       <xs:attribute name="lang" type="xsd:string" />
	 *     </xs:extension>
	 *   </xs:simpleContent>
	 * </xs:complexType>
	*/
	inline const char * get_title_lang(size_t &n){
		return books[n].title_lang.c_str();
	}
	inline void set_title_lang(const char * lang, size_t &n){
		books[n].title_lang = lang;
	}
	inline const char * get_title_value(size_t &n){
		return books[n].title.c_str();
	}
	inline void set_title_value(const char * value, size_t &n){
		books[n].title = value;
	}
};

int main(int argc, char **argv){
	CatalogSerializer catalog0;
	void *context;
	std::ifstream is;
	is.open(argv[1]);
	try {
		uxsd::load_catalog_xml(catalog0, context, argv[1], is);
	} catch (std::runtime_error &e) {
		std::cout << e.what() << std::endl;
		return 1;
	}
	is.close();

	/* Write out. */
	std::string of_name = std::string(argv[1]) + ".generated";
	std::ofstream os;
	os.open(of_name);
	uxsd::write_catalog_xml(catalog0, context, os);
	os.close();

	/* Read back the generated file. */
	CatalogSerializer catalog1;
	is.open(of_name);
	try {
		uxsd::load_catalog_xml(catalog1, context, of_name.c_str(), is);
	} catch (std::runtime_error &e) {
		std::cout << e.what() << std::endl;
		return 1;
	}
	is.close();

	/* Write out again. */
	std::string of_name2 = std::string(argv[1]) + ".generated.2";
	os.open(of_name2);
	uxsd::write_catalog_xml(catalog1, context, os);
	os.close();

	/* Compare memory representations. */
	assert(catalog0.books.size() == catalog1.books.size());
	for(int i=0; i<catalog0.books.size(); i++){
		auto &book0 = catalog0.books[i];
		auto &book1 = catalog1.books[i];
		assert(book0.available == book1.available);
		assert(book0.id == book1.id);
		assert(book0.isbn == book1.isbn);
		assert(book0.genre == book1.genre);
		assert(book0.title == book0.title);
		assert(book0.title_lang == book0.title_lang);
		assert(book0.authors.size() == book1.authors.size());
		for(int j=0; j<book0.authors.size(); j++){
			auto &auth0 = book0.authors[j];
			auto &auth1 = book1.authors[j];
			assert(auth0.recommends == auth1.recommends);
			assert(auth0.name == auth1.name);
			assert(auth0.born == auth1.born);
			assert(auth0.died == auth1.died);
		}
	}
}
