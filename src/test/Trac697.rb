class C

  C = Array.new

  def self.my_new(*args, &block)
    instance = new(*args, &block)
    C << instance
    instance
  end

  # Make C.new private
  class << self
    private :new
  end
end

p C.my_new    # Ok

p C.new       # MRI raises NoMethodError; MagLev executes

