require 'minitest/spec'

require 'psych'

MiniTest::Unit.autorun

describe Psych::Parser do
  it 'Psych.load() works on valid YAML' do
    o = Psych.load("--- foo")
    o.must_equal "foo"
  end

  # Blocked on Trac 656
  it 'loads a sequence' do
    o = Psych.load("- a\n- b\n- c\n")
    o.must_equal ["a", "b", "c"]
  end
end
