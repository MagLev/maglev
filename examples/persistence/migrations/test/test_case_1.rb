class C
  VERSION = 1

  def initialize()
    @a = 1
    @b = 2
  end

  def m1
    raise "expecting @a to be 1 but was #{@a.inspect}" unless @a == 1
  end
  def m2
    raise "expecting @b to be 2 but was #{@b.inspect}" unless @b == 2
  end
  def m3
    raise "expecting @c to be nil but was #{@c.inspect}" unless @c.nil?
  end
end

class C
  VERSION = 2

  def initialize()
    @a = 10
    @c = 30
  end

  def m1
    raise "expecting @a to be 10 but was #{@a.inspect}" unless @a == 10
  end
  def m2
    raise "expecting @b to be nil but was #{@b.inspect}" unless @b.nil?
  end
  def m3
    raise "expecting @c to be 30 but was #{@c.inspect}" unless @c == 30
  end
end

