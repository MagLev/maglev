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
end
