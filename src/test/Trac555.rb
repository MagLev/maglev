class C
  def self.initialize
    puts "Hello from #{self}"
    99
  end
end
x = C.initialize
unless x == 99 ; raise 'error' ; end
true
