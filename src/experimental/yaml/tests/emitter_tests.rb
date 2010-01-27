require 'minitest/spec'

require 'psych'

MiniTest::Unit.autorun
Exception.install_debug_block do |e|
  puts "-- #{e}    (#{e.class})"
  case e
  when NoMethodError, ArgumentError
    nil.pause
  end
end
describe Psych::Emitter do
  it 'Psych.dump("foo") produces valid YAML' do
    yaml = Psych.dump("foo")
    yaml.must_equal "--- foo\n...\n"
  end
end
