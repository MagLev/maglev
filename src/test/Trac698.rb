# Maglev makes initialize private, but sends of initialize within
# an implementation of   new   override the protection.  Otherwise
# it would be impossible to implement   new   in Ruby .
class C
  def self.new(*args, &block)
    instance = allocate
    instance.initialize(*args, &block)
    instance
  end
end

begin
  C.new
  raise "Failed to raise error about initialize() being private"
rescue NoMethodError
  # OK!
end
