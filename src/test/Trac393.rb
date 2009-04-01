
class C < Array
  attr_reader :name
  def initialize(name)
    @name = name
  end
end

c = C.new('foo')
