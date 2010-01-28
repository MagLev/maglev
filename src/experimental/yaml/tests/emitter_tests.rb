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
#   it 'Psych.dump("foo") produces valid YAML' do
#     yaml = Psych.dump("foo")
#     yaml.must_equal "--- foo\n...\n"
  #   end
end

# start of stream
#   -- encoding: :utf8
# document start event
#   -- version:        [0, 0]
#   -- tag_directives: []
#   -- implicit:       true
# mapping start event
#   -- anchor:   nil
#   -- tag:      nil
#   -- implicit: true
#   -- style:    1
# scalar event
#   -- value:  "invoice"
#   -- anchor: nil
#   -- tag:    nil
#   -- plain:  true
#   -- quoted: false
#   -- style:  1
# scalar event
#   -- value:  "foo"
#   -- anchor: nil
#   -- tag:    nil
#   -- plain:  true
#   -- quoted: false
#   -- style:  1

describe "libpsych low level emitter" do
  it 'does basic life-cycle correctly' do
    sio = StringIO.new
    emitter = Psych::Emitter.new(sio)
    emitter.start_stream(Psych::Parser::UTF8)
    emitter.start_document([1,1], [["!", "tag:gemstone.com:2010:"]], false)
#     emitter.scalar()
#     emitter.end_document()
#     emitter.end_stream()
  end
end
