# Some minimal tests for Enumerable methods we've modified

require File.expand_path('simple', File.dirname(__FILE__))

class E
  include Enumerable

  def initialize(ary)
    @data = ary
  end

  def each
    @data.each do |el|
      yield el
    end
  end

end

e = E.new([0,1,2,3,4,5])
test(e.all? { |el| true }, true, 'all?')

# 1.9 feature
test(e.one? { |e| e > 4 }, true, 'one?: true case')
test(e.one? { |e| e < 4 }, false, 'one?: false case')

report
