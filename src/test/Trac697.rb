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

cx = C
begin
 cx.new       # MRI raises NoMethodError; MagLev executes
 raise 'fail'
rescue NoMethodError
  puts "OK"
end
true
