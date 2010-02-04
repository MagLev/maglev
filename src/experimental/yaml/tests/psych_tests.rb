require 'minitest/spec'
require 'psych'

MiniTest::Unit.autorun

describe 'Psych.load' do
  # Note: Many of these are from the psych tests
  it 'loads simple YAML examples' do
    Psych.load("--- foo\n").must_equal 'foo'
  end

  it 'loads documents' do
    docs = []
    Psych.load_documents("--- foo\n...\n--- bar\n...") do |doc|
      docs << doc
    end
    docs.must_equal ['foo', 'bar']
  end

  it 'has correct libyaml versions' do
    v = Psych.libyaml_version
    v.must_equal [0, 1, 3]
    v.join('.').must_equal Psych::LIBYAML_VERSION
  end

  it 'handles domain types' do
    got = nil
    Psych.add_domain_type 'foo.bar,2002', 'foo' do |type, val|
      got = val
    end

    Psych.load('--- !foo.bar,2002/foo hello')
    got.must_equal 'hello'

    Psych.load("--- !foo.bar,2002/foo\n- hello\n- world")
    got.must_equal ['hello', 'world']

    Psych.load("--- !foo.bar,2002/foo\nhello: world")
    got.must_equal({ 'hello' => 'world' })
  end
end
