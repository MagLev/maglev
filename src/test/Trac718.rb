# Trac 718 , from machinist gem

class Sham

  @@shams = {}

  def self.method_missing(symbol, *args, &block)
    if block_given?
      @@shams[symbol] = Sham.new(symbol, *args, &block)
    else
      sham = @@shams[symbol]
      raise "No sham defined for #{symbol}" if sham.nil?
      sham.fetch_value
    end
  end

  def initialize(name, *args, &block)
    @name      = name
    @generator = block
    @offset    = 0
    @@shams[name] = self
  end

  def fetch_value
    @generator.call
  end
end

Sham.title { "My test title718" }
tx = Sham.title
unless tx == 'My test title718' ; raise 'error'; end
puts "OK"
true
