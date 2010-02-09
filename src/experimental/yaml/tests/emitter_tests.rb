require 'minitest/spec'

require 'psych'

MiniTest::Unit.autorun
Exception.install_debug_block do |e|
  puts "-- #{e}    (#{e.class})"
  case e
  when NoMethodError, ArgumentError, NameError
    nil.pause
  end
end

describe Psych::Emitter do
  it 'Psych.dump("foo") produces valid YAML' do
    yaml = Psych.dump("foo")
    yaml.must_equal "--- foo\n...\n"
  end
end

describe "libpsych low level emitter" do
  it 'does basic-lifecycle correctly' do
    sio = StringIO.new
    emitter = Psych::Emitter.new(sio)
    emitter.start_stream(Psych::Parser::UTF8)
    emitter.start_document([1,1], [], false)

    value = "foo"
    tag = nil
    tag = nil
    anchor = nil
    plain = false
    quoted = false
    style = 3
    emitter.scalar(value, anchor, tag, plain, quoted, style)

    emitter.end_document(false)
    emitter.end_stream()
    sio.string.must_equal <<EOS
%YAML 1.1
%TAG ! tag:gemstone.com:2010:
--- !<%21str> "foo"
...
EOS
  end

  it 'does tags correctly' do
    sio = StringIO.new
    emitter = Psych::Emitter.new(sio)
    emitter.start_stream(Psych::Parser::UTF8)
    emitter.start_document([1,1], [["!", "tag:gemstone.com:2010:"]], false)

    value = "foo"
    tag = "!str"
    tag = nil
    anchor = nil
    plain = false
    quoted = false
    style = 3
    emitter.scalar(value, anchor, tag, plain, quoted, style)

    emitter.end_document(false)
    emitter.end_stream()
    sio.string.must_equal <<EOS
%YAML 1.1
%TAG ! tag:gemstone.com:2010:
--- !<%21str> "foo"
...
EOS
  end
end
