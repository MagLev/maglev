class P
  attr_reader :tags
  def initialize
    @tags = []
  end
end

p = P.new
p.tags << :x
p p
