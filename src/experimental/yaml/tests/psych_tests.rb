require 'minitest/spec'
require 'psych'
require 'test_handler'

MiniTest::Unit.autorun

Exception.install_debug_block do |e|
  puts "-- #{e}    (#{e.class})"
  case e
  when IndexError, ArgumentError
    nil.pause
  end
end
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

  it 'parses some yaml.org test cases' do
    yaml = [
            "%YAML 1.1\n%TAG ! tag:gemstone.com,2009:\n--- !squee\n",
            "- Mark McGwire\n- Sammy Sosa\n- Ken Griffey\n\n",
            "american:\n - Boston Red Sox\n - Detroit Tigers\nnational:\n - New York Mets\n - Chicago Cubs\n\n",
            "---\ntime: 20:30:20\nplayer: Sammy Sosa\naction: strike (miss)...\n",
           ]
    parser = Psych::Parser.new(TestHandler.new)
    yaml.each { |y| parser.parse(y) }
  end

  it 'raises an exception on ill-formed yaml' do
    parser = Psych::Parser.new(TestHandler.new)
    assert_raises(Psych::PsychSyntaxError) { parser.parse("invoice: foo\nbar:baz\n") }
  end
end
