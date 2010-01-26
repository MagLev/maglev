require 'minitest/spec'

require 'psych'

MiniTest::Unit.autorun

describe Psych::Parser do
  it 'Psych.load() works on valid YAML' do
    o = Psych.load("--- foo")
    o.must_equal "foo"
  end
end

