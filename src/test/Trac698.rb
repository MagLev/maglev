# MagLev does not make initialize private.
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
