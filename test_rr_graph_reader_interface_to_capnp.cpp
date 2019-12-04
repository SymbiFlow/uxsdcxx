#include "rr_graph_uxsdcxx.h"
#include "rr_graph_uxsdcxx.capnp.h"
#include "capnp/serialize.h"
#include "kj/filesystem.h"
#include <fstream>

class TestRrGraph: public uxsd::RrGraphBase {
public:
    TestRrGraph(ucap::RrGraph::Builder *out) : out_(out) {}

	/** Generated for complex type "timing":
	 * <xs:complexType name="timing">
	 *   <xs:attribute name="R" type="xs:float" />
	 *   <xs:attribute name="Cin" type="xs:float" />
	 *   <xs:attribute name="Cinternal" type="xs:float" />
	 *   <xs:attribute name="Cout" type="xs:float" />
	 *   <xs:attribute name="Tdel" type="xs:float" />
	 * </xs:complexType>
	*/
	inline void set_timing_Cin(float Cin, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().getTiming().setCin(Cin);
	}
	inline void set_timing_Cinternal(float Cinternal, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().getTiming().setCinternal(Cinternal);
	}
	inline void set_timing_Cout(float Cout, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().getTiming().setCout(Cout);
	}
	inline void set_timing_R(float R, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().getTiming().setR(R);
	}
	inline void set_timing_Tdel(float Tdel, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().getTiming().setTdel(Tdel);
	}

	/** Generated for complex type "switch":
	 * <xs:complexType name="switch">
	 *   <xs:all>
	 *     <xs:element minOccurs="0" name="timing" type="timing" />
	 *     <xs:element name="sizing" type="sizing" />
	 *   </xs:all>
	 *   <xs:attribute name="id" type="xs:int" use="required" />
	 *   <xs:attribute name="name" type="xs:string" use="required" />
	 *   <xs:attribute name="type" type="switch_type" />
	 * </xs:complexType>
	*/
	inline void set_switch_name(const char * name, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().setName(name);
	}
	inline void set_switch_type(uxsd::enum_switch_type type, void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		switch(type) {
		case uxsd::enum_switch_type::MUX:
			sw->get().setType(ucap::SwitchType::MUX);
			break;
		case uxsd::enum_switch_type::TRISTATE:
			sw->get().setType(ucap::SwitchType::TRISTATE);
			break;
		case uxsd::enum_switch_type::PASS_GATE:
			sw->get().setType(ucap::SwitchType::PASS_GATE);
			break;
		case uxsd::enum_switch_type::SHORT:
			sw->get().setType(ucap::SwitchType::SHORT);
			break;
		case uxsd::enum_switch_type::BUFFER:
			sw->get().setType(ucap::SwitchType::BUFFER);
			break;
		default:
			throw std::runtime_error("Unknown SwitchType");
        }
    }
	inline void * init_switch_timing(void * data) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		sw->get().initTiming();
		return data;
	}
	inline void finish_switch_timing(void * data) override {}
	inline void * init_switch_sizing(void * data, float buf_size, float mux_trans_size) override {
		auto *sw = static_cast<capnp::Orphan<ucap::Switch>*>(data);
		auto sizing = sw->get().initSizing();
		sizing.setBufSize(buf_size);
		sizing.setMuxTransSize(mux_trans_size);
		return data;
	}
	inline void finish_switch_sizing(void * data) override {}

	/** Generated for complex type "switches":
	 * <xs:complexType name="switches">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="switch" type="switch" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void * add_switches_switch(void * data, int id) override {
		switches_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::Switch>());
		switches_.back().get().setId(id);
		return &switches_.back();
	}
	inline void finish_switches_switch(void * data) override {}

	inline void * init_rr_graph_switches(void * data) override {
		switches_.clear();
		return nullptr;
	}
	inline void finish_rr_graph_switches(void * data) override {
		auto switches = out_->initSwitches();
		auto switch_list = switches.initSwitches(switches_.size());
		for(size_t i = 0; i < switches_.size(); ++i) {
			switch_list.adoptWithCaveats(i, std::move(switches_[i]));
		}
	}

	/** Generated for complex type "meta":
	 * <xs:complexType name="meta">
	 *   <xs:simpleContent>
	 *     <xs:extension base="xs:string">
	 *       <xs:attribute name="name" type="xs:string" use="required" />
	 *     </xs:extension>
	 *   </xs:simpleContent>
	 * </xs:complexType>
	*/
	inline void set_meta_name(const char * name, void * data) override {
		auto *meta = static_cast<capnp::Orphan<ucap::Meta>*>(data);
		meta->get().setName(name);
	}
	inline void set_meta_value(const char * value, void * data) override {
		auto *meta = static_cast<capnp::Orphan<ucap::Meta>*>(data);
		meta->get().setValue(value);
	}

	/** Generated for complex type "metadata":
	 * <xs:complexType name="metadata">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="meta" type="meta" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void * add_metadata_meta(void * data) override {
		metadata_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::Meta>());

		return &metadata_.back();
	}
	inline void finish_metadata_meta(void * data) override {}

	/** Generated for complex type "node_loc":
	 * <xs:complexType name="node_loc">
	 *   <xs:attribute name="xlow" type="xs:int" use="required" />
	 *   <xs:attribute name="ylow" type="xs:int" use="required" />
	 *   <xs:attribute name="xhigh" type="xs:int" use="required" />
	 *   <xs:attribute name="yhigh" type="xs:int" use="required" />
	 *   <xs:attribute name="side" type="loc_side" />
	 *   <xs:attribute name="ptc" type="xs:int" use="required" />
	 * </xs:complexType>
	*/
	inline void set_node_loc_side(uxsd::enum_loc_side side, void * data) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);

		switch(side) {
		case uxsd::enum_loc_side::LEFT:
			node->get().getLoc().setSide(ucap::LocSide::LEFT);
			break;
		case uxsd::enum_loc_side::RIGHT:
			node->get().getLoc().setSide(ucap::LocSide::RIGHT);
			break;
		case uxsd::enum_loc_side::TOP:
			node->get().getLoc().setSide(ucap::LocSide::TOP);
			break;
		case uxsd::enum_loc_side::BOTTOM:
			node->get().getLoc().setSide(ucap::LocSide::BOTTOM);
			break;
		default:
			throw std::runtime_error("Unknown LocSide");
		}
	}

	/** Generated for complex type "node_timing":
	 * <xs:complexType name="node_timing">
	 *   <xs:attribute name="R" type="xs:float" use="required" />
	 *   <xs:attribute name="C" type="xs:float" use="required" />
	 * </xs:complexType>
	*/

	/** Generated for complex type "node_segment":
	 * <xs:complexType name="node_segment">
	 *   <xs:attribute name="segment_id" type="xs:int" use="required" />
	 * </xs:complexType>
	*/


	/** Generated for complex type "connection_box_annotation":
	 * <xs:complexType name="connection_box_annotation">
	 *   <xs:attribute name="x" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="y" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="id" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="site_pin_delay" type="xs:float" use="required" />
	 * </xs:complexType>
	*/

	/** Generated for complex type "canonical_loc":
	 * <xs:complexType name="canonical_loc">
	 *   <xs:attribute name="x" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="y" type="xs:unsignedInt" use="required" />
	 * </xs:complexType>
	*/


	/** Generated for complex type "node":
	 * <xs:complexType name="node">
	 *   <xs:all>
	 *     <xs:element name="loc" type="node_loc" />
	 *     <xs:element minOccurs="0" name="timing" type="node_timing" />
	 *     <xs:element minOccurs="0" name="segment" type="node_segment" />
	 *     <xs:element minOccurs="0" name="metadata" type="metadata" />
	 *     <xs:element minOccurs="0" name="canonical_loc" type="canonical_loc" />
	 *     <xs:element minOccurs="0" name="connection_box" type="connection_box_annotation" />
	 *   </xs:all>
	 *   <xs:attribute name="id" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="type" type="node_type" use="required" />
	 *   <xs:attribute name="direction" type="node_direction" />
	 *   <xs:attribute name="capacity" type="xs:unsignedInt" use="required" />
	 * </xs:complexType>
	*/
	inline void set_node_direction(uxsd::enum_node_direction direction, void * data) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);

		switch(direction) {
		case uxsd::enum_node_direction::INC_DIR:
			node->get().setDirection(ucap::NodeDirection::INC_DIR);
			break;
		case uxsd::enum_node_direction::DEC_DIR:
			node->get().setDirection(ucap::NodeDirection::DEC_DIR);
			break;
		case uxsd::enum_node_direction::BI_DIR:
			node->get().setDirection(ucap::NodeDirection::BI_DIR);
			break;
		default:
			throw std::runtime_error("Unknown NodeDirection");
		}
	}

	inline void * init_node_loc(void * data, int ptc, int xhigh, int xlow, int yhigh, int ylow) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);
		auto builder = node->get().initLoc();
		builder.setPtc(ptc);
		builder.setXlow(xlow);
		builder.setXhigh(xhigh);
		builder.setYlow(ylow);
		builder.setYhigh(yhigh);
		return data;
	}
	inline void finish_node_loc(void * data) override {}

	inline void * init_node_timing(void * data, float C, float R) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);
		auto builder = node->get().initTiming();
		builder.setC(C);
		builder.setR(R);
		return data;
	}
	inline void finish_node_timing(void * data) override {}
	inline void * init_node_segment(void * data, int segment_id) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);
		node->get().initSegment().setSegmentId(segment_id);
		return data;
	}
	inline void finish_node_segment(void * data) override {}

	inline void * init_node_metadata(void * data) override {
		metadata_.clear();
		return data;
	}
	inline void finish_node_metadata(void * data) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);
		auto builder = node->get();

		if(!metadata_.empty()) {
			auto metadata = builder.initMetadata();
			auto meta_list = metadata.initMetas(metadata_.size());
			for(size_t i = 0; i < metadata_.size(); ++i) {
				meta_list.adoptWithCaveats(i, std::move(metadata_[i]));
			}
		}
	}

	inline void * init_node_canonical_loc(void * data, unsigned int x, unsigned int y) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);
		auto builder = node->get().initCanonicalLoc();
		builder.setX(x);
		builder.setY(y);
		return nullptr;
	}
	inline void finish_node_canonical_loc(void * data) override {}

	inline void * init_node_connection_box(void * data, unsigned int id, float site_pin_delay, unsigned int x, unsigned int y) override {
		auto *node = static_cast<capnp::Orphan<ucap::Node>*>(data);
		auto builder = node->get().initConnectionBox();
		builder.setId(id);
		builder.setSitePinDelay(site_pin_delay);
		builder.setX(x);
		builder.setY(y);
		return nullptr;
	}
	inline void finish_node_connection_box(void * data) override {}

	/** Generated for complex type "rr_nodes":
	 * <xs:complexType name="rr_nodes">
	 *   <xs:choice maxOccurs="unbounded">
	 *     <xs:element name="node" type="node" />
	 *   </xs:choice>
	 * </xs:complexType>
	*/
	inline void * add_rr_nodes_node(void * data, unsigned int capacity, unsigned int id, uxsd::enum_node_type type) override {
		nodes_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::Node>());
		auto builder = nodes_.back().get();
		builder.setCapacity(capacity);
		builder.setId(id);

		switch(type) {
		case uxsd::enum_node_type::CHANX:
			builder.setType(ucap::NodeType::CHANX);
			break;
		case uxsd::enum_node_type::CHANY:
			builder.setType(ucap::NodeType::CHANY);
			break;
		case uxsd::enum_node_type::SOURCE:
			builder.setType(ucap::NodeType::SOURCE);
			break;
		case uxsd::enum_node_type::SINK:
			builder.setType(ucap::NodeType::SINK);
			break;
		case uxsd::enum_node_type::OPIN:
			builder.setType(ucap::NodeType::OPIN);
			break;
		case uxsd::enum_node_type::IPIN:
			builder.setType(ucap::NodeType::IPIN);
			break;
		default:
			throw std::runtime_error("Unknown NodeType");
		}

		return &nodes_.back();
	}
	inline void finish_rr_nodes_node(void * data) override {}

	inline void * init_rr_graph_rr_nodes(void * data) override {
		return nullptr;
	}
	inline void finish_rr_graph_rr_nodes(void * data) override {
		auto rr_nodes = out_->initRrNodes();
		auto nodes = rr_nodes.initNodes(nodes_.size());
		for(size_t i = 0; i < nodes_.size(); ++i) {
			nodes.adoptWithCaveats(i, std::move(nodes_[i]));
		}
	}

	/** Generated for complex type "edge":
	 * <xs:complexType name="edge">
	 *   <xs:all>
	 *     <xs:element minOccurs="0" name="metadata" type="metadata" />
	 *   </xs:all>
	 *   <xs:attribute name="src_node" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="sink_node" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="switch_id" type="xs:unsignedInt" use="required" />
	 * </xs:complexType>
	*/
	inline void * init_edge_metadata(void * data) override {
		metadata_.clear();
		return data;
	}
	inline void finish_edge_metadata(void * data) override {
		auto *edge = static_cast<capnp::Orphan<ucap::Edge>*>(data);
		auto builder = edge->get();

		if(!metadata_.empty()) {
			auto metadata = builder.initMetadata();
			auto meta_list = metadata.initMetas(metadata_.size());
			for(size_t i = 0; i < metadata_.size(); ++i) {
				meta_list.adoptWithCaveats(i, std::move(metadata_[i]));
			}
		}
	}

	/** Generated for complex type "channels":
	 * <xs:complexType name="channels">
	 *   <xs:sequence>
	 *     <xs:element name="channel" type="channel" />
	 *     <xs:element maxOccurs="unbounded" name="x_list" type="x_list" />
	 *     <xs:element maxOccurs="unbounded" name="y_list" type="y_list" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void * init_channels_channel(void * data, int chan_width_max, int x_max, int x_min, int y_max, int y_min) override {
		auto channels = out_->getChannels();
		auto channel = channels.getChannel();
		channel.setChanWidthMax(chan_width_max);
		channel.setXMin(x_min);
		channel.setXMax(x_max);
		channel.setYMin(y_min);
		channel.setYMax(y_max);
		return nullptr;
	}
	inline void finish_channels_channel(void * data) override {
	}

	inline void * add_channels_x_list(void * data, unsigned int index, int info) override {
		auto x_list = capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::XList>();
		x_list.get().setIndex(index);
		x_list.get().setInfo(info);
		x_list_.emplace_back(std::move(x_list));
		return nullptr;
	}
	inline void finish_channels_x_list(void * data) override {}
	inline void * add_channels_y_list(void * data, unsigned int index, int info) override {
		auto y_list = capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::YList>();
		y_list.get().setIndex(index);
		y_list.get().setInfo(info);
		y_list_.emplace_back(std::move(y_list));
		return nullptr;
	}
	inline void finish_channels_y_list(void * data) override {}

	inline void * init_rr_graph_channels(void * data) override {
		out_->initChannels();
		return nullptr;
	}
	inline void finish_rr_graph_channels(void * data) override {
		auto channels = out_->getChannels();

		auto x_list = channels.initXLists(x_list_.size());
		for(size_t i = 0; i < x_list_.size(); ++i) {
			x_list.adoptWithCaveats(i, std::move(x_list_[i]));
		}

		auto y_list = channels.initYLists(y_list_.size());
		for(size_t i = 0; i < y_list_.size(); ++i) {
			y_list.adoptWithCaveats(i, std::move(y_list_[i]));
		}
	}

	/** Generated for complex type "connection_box_declaration":
	 * <xs:complexType name="connection_box_declaration">
	 *   <xs:attribute name="id" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="name" type="xs:string" use="required" />
	 * </xs:complexType>
	*/
	inline void set_connection_box_declaration_name(const char * name, void * data) override {
		auto *box = static_cast<capnp::Orphan<ucap::ConnectionBoxDeclaration>*>(data);
		box->get().setName(name);
	}

	/** Generated for complex type "connection_boxes":
	 * <xs:complexType name="connection_boxes">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="connection_box" type="connection_box_declaration" />
	 *   </xs:sequence>
	 *   <xs:attribute name="x_dim" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="y_dim" type="xs:unsignedInt" use="required" />
	 *   <xs:attribute name="num_boxes" type="xs:unsignedInt" use="required" />
	 * </xs:complexType>
	*/
	inline void * add_connection_boxes_connection_box(void * data, unsigned int id) override {
		connection_boxes_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::ConnectionBoxDeclaration>());
		connection_boxes_.back().get().setId(id);
		return &connection_boxes_.back();
	}
	inline void finish_connection_boxes_connection_box(void * data) override {}

	inline void * init_rr_graph_connection_boxes(void * data, unsigned int num_boxes, unsigned int x_dim, unsigned int y_dim) override {
		auto builder = out_->initConnectionBoxes();
		builder.setNumBoxes(num_boxes);
		builder.setXDim(x_dim);
		builder.setYDim(y_dim);
		return nullptr;
	}
	inline void finish_rr_graph_connection_boxes(void * data) override {
		auto builder = out_->getConnectionBoxes();
		auto connection_boxes = builder.initConnectionBoxes(connection_boxes_.size());

		for(size_t i = 0; i < connection_boxes_.size(); ++i) {
			connection_boxes.adoptWithCaveats(i, std::move(connection_boxes_[i]));
		}
	}

	/** Generated for complex type "rr_edges":
	 * <xs:complexType name="rr_edges">
	 *   <xs:choice maxOccurs="unbounded">
	 *     <xs:element name="edge" type="edge" />
	 *   </xs:choice>
	 * </xs:complexType>
	*/
	inline void * add_rr_edges_edge(void * data, unsigned int sink_node, unsigned int src_node, unsigned int switch_id) override {
		edges_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::Edge>());

		auto builder = edges_.back().get();
		builder.setSinkNode(sink_node);
		builder.setSrcNode(src_node);
		builder.setSwitchId(switch_id);

		return &edges_.back();
	}
	inline void finish_rr_edges_edge(void * data) override {}
	inline void * init_rr_graph_rr_edges(void * data) override {
		return nullptr;
	}
	inline void finish_rr_graph_rr_edges(void * data) override {
		auto rr_edges = out_->initRrEdges();
		auto edges = rr_edges.initEdges(edges_.size());

		for(size_t i = 0; i < edges_.size(); ++i) {
			edges.adoptWithCaveats(i, std::move(edges_[i]));
		}
	}

	/** Generated for complex type "rr_graph":
	 * <xs:complexType xmlns:xs="http://www.w3.org/2001/XMLSchema">
	 *     <xs:all>
	 *       <xs:element name="channels" type="channels" />
	 *       <xs:element name="switches" type="switches" />
	 *       <xs:element name="segments" type="segments" />
	 *       <xs:element name="block_types" type="block_types" />
	 *       <xs:element name="connection_boxes" type="connection_boxes" />
	 *       <xs:element name="grid" type="grid_locs" />
	 *       <xs:element name="rr_nodes" type="rr_nodes" />
	 *       <xs:element name="rr_edges" type="rr_edges" />
	 *     </xs:all>
	 *     <xs:attribute name="tool_name" type="xs:string" />
	 *     <xs:attribute name="tool_version" type="xs:string" />
	 *     <xs:attribute name="tool_comment" type="xs:string" />
	 *   </xs:complexType>
	*/
	inline void set_rr_graph_tool_comment(const char * tool_comment, void * data) override {
		out_->setToolComment(tool_comment);
    }
	inline void set_rr_graph_tool_name(const char * tool_name, void * data) override {
		out_->setToolName(tool_name);
    }
	inline void set_rr_graph_tool_version(const char * tool_version, void * data) override {
		out_->setToolVersion(tool_version);
    }

	/** Generated for complex type "segment_timing":
	 * <xs:complexType name="segment_timing">
	 *   <xs:attribute name="R_per_meter" type="xs:float" />
	 *   <xs:attribute name="C_per_meter" type="xs:float" />
	 * </xs:complexType>
	*/
	inline void set_segment_timing_C_per_meter(float C_per_meter, void * data) override {
		auto *segment = static_cast<capnp::Orphan<ucap::Segment>*>(data);
		segment->get().getTiming().setCPerMeter(C_per_meter);
	}
	inline void set_segment_timing_R_per_meter(float R_per_meter, void * data) override {
		auto *segment = static_cast<capnp::Orphan<ucap::Segment>*>(data);
		segment->get().getTiming().setCPerMeter(R_per_meter);
	}

	/** Generated for complex type "segment":
	 * <xs:complexType name="segment">
	 *   <xs:all>
	 *     <xs:element minOccurs="0" name="timing" type="segment_timing" />
	 *   </xs:all>
	 *   <xs:attribute name="id" type="xs:int" use="required" />
	 *   <xs:attribute name="name" type="xs:string" use="required" />
	 * </xs:complexType>
	*/
	inline void set_segment_name(const char * name, void * data) override {
		auto *segment = static_cast<capnp::Orphan<ucap::Segment>*>(data);
		segment->get().setName(name);
	}
	inline void * init_segment_timing(void * data) override {
		auto *segment = static_cast<capnp::Orphan<ucap::Segment>*>(data);
		segment->get().initTiming();
		return data;
	}
	inline void finish_segment_timing(void * data) override {}

	/** Generated for complex type "segments":
	 * <xs:complexType name="segments">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="segment" type="segment" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void * add_segments_segment(void * data, int id) override {
		segments_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::Segment>());
		segments_.back().get().setId(id);
		return &segments_.back();
	}
	inline void finish_segments_segment(void * data) override {}
	inline void * init_rr_graph_segments(void * data) override {
		return nullptr;
	}
	inline void finish_rr_graph_segments(void * data) override {
		auto segments = out_->initSegments();
		auto segment_list = segments.initSegments(segments_.size());
		for(size_t i = 0; i < segments_.size(); ++i) {
			segment_list.adoptWithCaveats(i, std::move(segments_[i]));
		}
	}

	/** Generated for complex type "pin":
	 * <xs:complexType name="pin">
	 *   <xs:simpleContent>
	 *     <xs:extension base="xs:string">
	 *       <xs:attribute name="ptc" type="xs:int" use="required" />
	 *     </xs:extension>
	 *   </xs:simpleContent>
	 * </xs:complexType>
	*/
	inline void set_pin_value(const char * value, void * data) override {
		auto *pin = static_cast<capnp::Orphan<ucap::Pin>*>(data);
		pin->get().setValue(value);
	}

	/** Generated for complex type "pin_class":
	 * <xs:complexType name="pin_class">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="pin" type="pin" />
	 *   </xs:sequence>
	 *   <xs:attribute name="type" type="pin_type" use="required" />
	 * </xs:complexType>
	*/
	inline void * add_pin_class_pin(void * data, int ptc) override {
		pins_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::Pin>());
		pins_.back().get().setPtc(ptc);
		return &pins_.back();
	}
	inline void finish_pin_class_pin(void * data) override {}

	/** Generated for complex type "block_type":
	 * <xs:complexType name="block_type">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" minOccurs="0" name="pin_class" type="pin_class" />
	 *   </xs:sequence>
	 *   <xs:attribute name="id" type="xs:int" use="required" />
	 *   <xs:attribute name="name" type="xs:string" use="required" />
	 *   <xs:attribute name="width" type="xs:int" use="required" />
	 *   <xs:attribute name="height" type="xs:int" use="required" />
	 * </xs:complexType>
	*/
	inline void set_block_type_name(const char * name, void * data) override {
		auto *block = static_cast<capnp::Orphan<ucap::BlockType>*>(data);
		block->get().setName(name);
	}

	inline void * add_block_type_pin_class(void * data, uxsd::enum_pin_type type) override {
		if(!pins_.empty()) {
			throw std::runtime_error("Pins not empty!");
		}
		pin_classes_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::PinClass>());

		switch(type) {
		case uxsd::enum_pin_type::OPEN:
			pin_classes_.back().get().setType(ucap::PinType::OPEN);
			break;
		case uxsd::enum_pin_type::OUTPUT:
			pin_classes_.back().get().setType(ucap::PinType::OUTPUT);
			break;
		case uxsd::enum_pin_type::INPUT:
			pin_classes_.back().get().setType(ucap::PinType::INPUT);
			break;
		default:
			throw std::runtime_error("Unknown enum_pin_type");
		}

		return &pin_classes_.back();
	}
	inline void finish_block_type_pin_class(void * data) override {
		auto *pin_class = static_cast<capnp::Orphan<ucap::PinClass>*>(data);
		auto pin_list = pin_class->get().initPins(pins_.size());

		for(size_t i = 0; i < pins_.size(); ++i) {
			pin_list.adoptWithCaveats(i, std::move(pins_[i]));
		}

		pins_.clear();
	}
	/** Generated for complex type "block_types":
	 * <xs:complexType name="block_types">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="block_type" type="block_type" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void * add_block_types_block_type(void * data, int height, int id, int width) override {
		if(!pin_classes_.empty()) {
			throw std::runtime_error("pin classes not empty!");
		}

		block_types_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::BlockType>());

		auto builder = block_types_.back().get();
		builder.setId(id);
		builder.setHeight(height);
		builder.setWidth(width);

		return &block_types_.back();
	}
	inline void finish_block_types_block_type(void * data) override {
		auto *block = static_cast<capnp::Orphan<ucap::BlockType>*>(data);
		auto pin_classes = block->get().initPinClasses(pin_classes_.size());

		for(size_t i = 0; i < pin_classes_.size(); ++i) {
			pin_classes.adoptWithCaveats(i, std::move(pin_classes_[i]));
		}

		pin_classes_.clear();
	}
	inline void * init_rr_graph_block_types(void * data) override {
		return nullptr;
	}
	inline void finish_rr_graph_block_types(void * data) override {
		auto blocks = out_->initBlockTypes();
		auto blocks_list = blocks.initBlockTypes(block_types_.size());

		for(size_t i = 0; i < block_types_.size(); ++i) {
			blocks_list.adoptWithCaveats(i, std::move(block_types_[i]));
		}
	}

	/** Generated for complex type "grid_loc":
	 * <xs:complexType name="grid_loc">
	 *   <xs:attribute name="x" type="xs:int" use="required" />
	 *   <xs:attribute name="y" type="xs:int" use="required" />
	 *   <xs:attribute name="block_type_id" type="xs:int" use="required" />
	 *   <xs:attribute name="width_offset" type="xs:int" use="required" />
	 *   <xs:attribute name="height_offset" type="xs:int" use="required" />
	 * </xs:complexType>
	*/
	/** Generated for complex type "grid_locs":
	 * <xs:complexType name="grid_locs">
	 *   <xs:sequence>
	 *     <xs:element maxOccurs="unbounded" name="grid_loc" type="grid_loc" />
	 *   </xs:sequence>
	 * </xs:complexType>
	*/
	inline void * add_grid_locs_grid_loc(void * data, int block_type_id, int height_offset, int width_offset, int x, int y) override {
		grid_.emplace_back(
				capnp::Orphanage::getForMessageContaining(*out_).newOrphan<ucap::GridLoc>());
		auto builder = grid_.back().get();

		builder.setBlockTypeId(block_type_id);
		builder.setHeightOffset(height_offset);
		builder.setWidthOffset(width_offset);
		builder.setX(x);
		builder.setY(y);

		return nullptr;
	}
	inline void finish_grid_locs_grid_loc(void * data) override {}

	inline void * init_rr_graph_grid(void * data) override {
		return nullptr;
	}
	inline void finish_rr_graph_grid(void * data) override {
		auto grid = out_->initGrid();
		auto grid_locs = grid.initGridLocs(grid_.size());
		for(size_t i = 0; i < grid_.size(); ++i) {
			grid_locs.adoptWithCaveats(i, std::move(grid_[i]));
		}
	}
private:
    ucap::RrGraph::Builder *out_;
	std::vector<capnp::Orphan<ucap::XList>> x_list_;
	std::vector<capnp::Orphan<ucap::YList>> y_list_;
	std::vector<capnp::Orphan<ucap::Switch>> switches_;
	std::vector<capnp::Orphan<ucap::Node>> nodes_;
	std::vector<capnp::Orphan<ucap::Edge>> edges_;
	std::vector<capnp::Orphan<ucap::Meta>> metadata_;
	std::vector<capnp::Orphan<ucap::ConnectionBoxDeclaration>> connection_boxes_;
	std::vector<capnp::Orphan<ucap::Segment>> segments_;
	std::vector<capnp::Orphan<ucap::PinClass>> pin_classes_;
	std::vector<capnp::Orphan<ucap::Pin>> pins_;
	std::vector<capnp::Orphan<ucap::BlockType>> block_types_;
	std::vector<capnp::Orphan<ucap::GridLoc>> grid_;
};

int main(int argc, char * argv[]) {

	::capnp::MallocMessageBuilder builder;

	auto graph = builder.initRoot<ucap::RrGraph>();
	TestRrGraph test(&graph);

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
