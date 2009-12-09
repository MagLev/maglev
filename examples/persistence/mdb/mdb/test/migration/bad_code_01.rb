class BadCode
  FOO = 1
  #attr_reader :id
  def initialize(id); @id = id end
  def hello();        puts "hi" end
  def a_syntax_error( x end
end

