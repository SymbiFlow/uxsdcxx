#include "rr_graph_uxsdcxx.h"
#include "rr_graph_uxsdcxx.capnp.h"
#include "rr_graph_uxsdcxx_capnp_impl.h"
#include "capnp/serialize.h"
#include "kj/filesystem.h"
#include <fstream>

int main(int argc, char * argv[]) {

	::capnp::MallocMessageBuilder builder;

	auto graph = builder.initRoot<ucap::RrGraph>();
	uxsd::CapnpRrGraph test;
	test.set_rr_graph_builder(graph);

	if(argc < 3) {
		throw std::runtime_error("No file argument provided");
	}

	std::ifstream file(argv[1]);
	if(!file) {
		throw std::runtime_error(std::string("File ") + argv[1] + " not found.");
	}
	pugi::xml_parse_result result = load_rr_graph_xml(test, file);

	try {
		auto fs = kj::newDiskFilesystem();
		auto path = fs->getCurrentPath().evalNative(argv[2]);

		const auto& dir = fs->getRoot();
		auto f = dir.openFile(path, kj::WriteMode::CREATE | kj::WriteMode::MODIFY);
		f->truncate(0);
		auto f_app = kj::newFileAppender(std::move(f));
		capnp::writeMessage(*f_app, builder);
	} catch (kj::Exception& e) {
		throw std::runtime_error(e.getDescription().cStr());
	}

    return 0;
}
