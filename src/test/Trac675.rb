class OrderedHash < Hash
  def initialize(*args, &block)
    super
    @keys = []
  end
end

class Errors < OrderedHash
  def initialize(base)
    @base = base
    super()
  end
end

e = Errors.new(Object)
