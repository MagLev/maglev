# Test the Psych based YAML support in MagLev

require 'test/unit'
require 'yaml'

# This class tests the top level lib/psych/psych.rb file.
# This is where module Psych is defined.
class PsychModuleTest < Test::Unit::TestCase

  def test_psych_constants
    assert_equal('1.0.0', YAML::VERSION,         'YAML::VERSION')
    assert_equal('0.1.3', YAML::LIBYAML_VERSION, 'YAML::LIBYAML_VERSION')
  end

  def test_psych_load
    assert_equal('a',        YAML.load("--- a"))
    assert_equal(['a', 'b'], YAML.load("---\n - a\n - b"))
  end

  def test_psych_parse
    scalar = YAML.parse("--- a")
    assert_equal(Psych::Nodes::Scalar, scalar.class)

    sequence = YAML.parse("---\n - a\n - b")
    assert_equal(Psych::Nodes::Sequence, sequence.class)
  end

  def test_psych_yaml_ast
    scalar = YAML.yaml_ast("--- a")
    assert_equal(Psych::Nodes::Stream, scalar.class)

    sequence = YAML.yaml_ast("---\n - a\n - b")
    assert_equal(Psych::Nodes::Stream, sequence.class)
  end

  def test_psych_dump
    assert_equal("---\n- a\n- b\n", YAML.dump(['a', 'b']))
  end

  def test_psych_to_json
    assert_equal("['a', 'b']\n", YAML.to_json(['a', 'b']))
  end

  def test_psych_load_documents
    results = Array.new
    yaml = <<EOS
# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals
EOS
    YAML.load_documents(yaml) { |ruby_obj| results << ruby_obj }
    assert_equal(2, results.size)
    assert_equal(['Mark McGwire', 'Sammy Sosa', 'Ken Griffey'], results[0])
    assert_equal(['Chicago Cubs', 'St Louis Cardinals'],        results[1])
  end

  def test_psych_load_file
    test_file = File.dirname(__FILE__) + '/test_data/psych_test_file.yml'
    expected = [ { 'item' => 'Super Hoop', 'quantity' => 1 },
                 { 'item' => 'Basketball', 'quantity' => 4 },
                 { 'item' => 'Big Shoes',  'quantity' => 1 } ]

    result = YAML.load_file(test_file)
    assert_equal(expected, result)
  end

  def test_parse_with_io
    sio = StringIO.new("- foo\n- bar\n- :baz\n- 123")
    assert_equal(["foo", "bar", :baz, 123], YAML.load(sio))
  end
end


# Support class for test_anchor_alias_bug
class Blarg
  attr_reader :foo
  def initialize v
    @foo = v
  end
  def ==(other)
    other.class == self.class and @foo == other.foo
  end
end

# Support class for test_anchor_alias_bug
class Quux
  attr_accessor :one, :two, :three
  def ==(other)
    other.class == self.class and
      other.one == @one and
      other.two == @two and
      other.three ==  @three
  end
end

class GemStonePsychPatchesTest < Test::Unit::TestCase
  # Patched to_yaml.rb to recognize !ruby/sym
  def test_symbol_patch
    assert_equal(:foo,  YAML.load("!ruby/sym foo"))
    assert_equal(:quux, YAML.load("!ruby/symbol quux"))
  end

  # There was a bug in rubygems trying to install sinatra 0.9.6.  The
  # problem was that objects with type !ruby/object were not getting saved
  # as an anchor.  This tests that bugfix.
  def test_anchor_alias_bug
    q1 = Quux.new
    q1.one   = Blarg.new 1
    q1.two   = Blarg.new 2
    # @one and @three are identical, so the YAML should have @one be an
    # anchor and @three should be an alias
    q1.three = q1.one

    yaml = YAML.dump q1

    # puts yaml
    q2 = YAML.load yaml

    # This yaml was generated from MRI, so this test ensures we can read
    # the MRI dump of the above classes.
    yaml =<<EOYAML
--- !ruby/object:Quux
one: &id001 !ruby/object:Blarg
  foo: 1
three: *id001
two: !ruby/object:Blarg
  foo: 2
EOYAML
    q3= YAML.load(yaml)

    assert_equal(q1, q2)
    assert_equal(q1, q3)
    assert_same(q3.one, q3.three)
  end
end

class TestAliasAndAnchor < Test::Unit::TestCase
  def test_mri_compatibility
    yaml = <<EOYAML
---
- &id001 !ruby/object {}

- *id001
- *id001
EOYAML
    result = YAML.load yaml
    result.each {|el| assert_same(result[0], el) }
  end

  def test_anchor_alias_round_trip
    o = Object.new
    original = [o,o,o]

    yaml = YAML.dump original
    result = YAML.load yaml
    result.each {|el| assert_same(result[0], el) }
  end
end
