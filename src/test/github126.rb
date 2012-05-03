class Example
  def each
    return to_enum unless block_given?
    [1,2,3].each {|i| yield i }
  end
end

enumerator = Example.new.each
raise "fail" unless enumerator.map {|i| i } == [1,2,3]
