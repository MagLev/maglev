class X
  # Instance IVar
  def initialize
    @y = 0
  end

  def y
    @y
  end

  def x
    unless defined?(@x) then
      @y += 1
      @x = nil
    end
    @x
  end

  # Class IVar
  @y = 0

  def self.y
    @y
  end

  def self.x
    unless defined?(@x) then
      @y += 1
      @x = nil
    end
    @x
  end
end

x = X.new
x.x
x.x
X.x
X.x

raise "bad defined? in instance #{x.y}" unless x.y == 1
raise "bad defined? in class #{X.y}" unless X.y == 1
