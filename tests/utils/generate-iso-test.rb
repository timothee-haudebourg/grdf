#!/usr/bin/env ruby
# Usage: generate-iso-test.rb TEST_NAME DATASET_SIZE BLANK_NODE_COUNT OTHER_NODES_COUNT
#        Without arguments, it will generate an entire test file.
require 'set'

class BlankNode
	attr_reader :suffix

	include Comparable

	def initialize(suffix)
		@suffix = suffix
	end

	def to_s
		"_:#{@suffix}"
	end

	def <=>(other)
		return -1 if other.class == Iri
		@suffix <=> other.suffix
	end

	def eql?(other)
		other.class == BlankNode && @suffix == other.suffix
	end

	def hash
		@suffix.hash
	end

	def to_rust
		"Term::Blank(BlankIdBuf::new(\"#{self.to_s}\".to_string()).unwrap())"
	end

	def substituted
		BlankNode.new("s#{@suffix}")
	end
end

class Iri
	attr_reader :path

	include Comparable

	def initialize(path)
		@path = path
	end

	def to_s
		"http://example.com/#{@path}"
	end

	def <=>(other)
		return 1 if other.class == BlankNode
		@path <=> other.path
	end

	def eql?(other)
		other.class == Iri && @path == other.path
	end

	def hash
		@path.hash
	end

	def to_rust
		"Term::Iri(iri!(\"#{self.to_s}\").into())"
	end

	def substituted
		self
	end
end

class Triple
	attr_reader :subject, :predicate, :object

	def self.random_from_terms(terms)
		Triple.new(terms.sample, terms.sample, terms.sample)
	end

	def initialize(subject, predicate, object)
		@subject = subject
		@predicate = predicate
		@object = object
	end

	def eql?(other)
		other.class == Triple && @subject == other.subject && @predicate == other.predicate && @object == other.object
	end

	def hash
		@subject.hash ^ @predicate.hash ^ @object.hash
	end

	def to_s
		"#{@subject} #{@predicate} #{@object} ."
	end

	def to_rust
		"Quad(\n\t\t#{@subject.to_rust},\n\t\t#{@predicate.to_rust},\n\t\t#{@object.to_rust},\n\t\tNone\n\t)"
	end

	def substituted
		Triple.new(@subject.substituted, @predicate.substituted, @object.substituted)
	end
end

class Dataset
	def self.random_from_terms(terms, size)
		dataset = Dataset.new
		size.times do
			while !dataset.insert(Triple.random_from_terms(terms)) do
				# nothing
			end
		end
		dataset
	end

	def initialize()
		@triples = Set.new
	end

	def insert(triple)
		!@triples.add?(triple).nil?
	end

	def to_s
		result = ""
		@triples.each do |triple|
			result += "\n" unless result.empty?
			result += triple.to_s
		end
		result
	end

	def to_rust(id)
		result = "\tlet mut #{id} = HashDataset::new();"
		@triples.each do |triple|
			result += "\n\t#{id}.insert(#{triple.to_rust});"
		end
		result
	end

	def substituted
		dataset = Dataset.new
		@triples.each do |triple|
			dataset.insert(triple.substituted)
		end
		dataset
	end
end

def generate_blank_nodes(count)
	terms = []

	count.times do |i|
		terms.push BlankNode.new(i)
	end

	terms
end

def generate_iri_nodes(count)
	terms = []

	count.times do |i|
		terms.push Iri.new(i)
	end

	terms
end

def generate_test(name, size, blank_count, iri_count)
	blank_nodes = generate_blank_nodes(blank_count)
	iri_nodes = generate_iri_nodes(iri_count)

	dataset = Dataset.random_from_terms(blank_nodes + iri_nodes, size)

	puts "#[test]"
	puts "fn iso_#{name}() {"
	puts dataset.to_rust("a")
	puts dataset.substituted.to_rust("b")
	puts "\ttest(a, b)"
	puts "}"
end

if ARGV.empty? then
	puts "//! This file is auto generated using the"
	puts "//! `tests/utils/generate-iso-test.rb` script."

	puts File.open("tests/utils/iso.rs.template").read

	CLASSES = [
		# blank nodes count, graph size, test count
		[                 1,          5,         10],
		[                 2,          5,         20],
		[                 3,         10,         50],
		[                10,         20,        100],
		[                50,        100,         20],
	]

	# CLASSES = [
	# 	# blank nodes count, graph size, test count
	# 	[                 1,          5,          5],
	# 	[                 2,          5,          5],
	# 	[                 3,         10,          5],
	# 	[                10,         20,          4],
	# 	[                50,        100,          1],
	# ]

	n = 1
	CLASSES.each do |c|
		c[2].times do
			generate_test(n.to_s.rjust(3, '0'), c[1], c[0], c[1]-c[0])
			n += 1
		end
	end
else
	generate_test(ARGV[0], ARGV[1].to_i, ARGV[2].to_i, ARGV[3].to_i)
end