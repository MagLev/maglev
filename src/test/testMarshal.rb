h = {'one' => {'two' => {'three' => 0}}}
b = Marshal.dump(h, 6);

hb = Marshal.load(b)
unless h == hb ; raise 'error' ; end
puts "Hash ok"

k = Class.new
a = 0
begin
  a = Marshal.dump(k)
rescue TypeError
  puts "ok a"
end

m = Module.new
b = 0
begin
  b = Marshal.dump(m)
rescue TypeError
  puts "ok b"
end

class Foo
  attr_reader :bar, :baz
  def initialize
    @bar = "hello"
    @baz = 1
  end
  def ==(other)
    other.class == self.class && other.bar == @bar && other.baz == @baz
  end
end

raise "nan"       unless Marshal.load(Marshal.dump(0.0/0.0)).nan?
raise "-infinity" unless Marshal.load(Marshal.dump(-1.0/0.0)).infinite? == -1
raise "infinity"  unless Marshal.load(Marshal.dump(1.0/0.0)).infinite? == 1

test_items = [ Foo.new,                # An ObjectThis used to fail...
#                (1..4),
               nil,
               true,
               false,
               :a_symbol,
               "xyz",
               1,
#               3.4,                   # Generic float
#                18888888888888888888888888888888888,
                %r{xyz},
# #               Struct
               ["hello", 12, ],
               { "one" => 'two'}
             ]

test_items.each do |item|
  # puts "== Marshal dump/load of #{item.inspect}"
  b = Marshal.dump(item)
  item2 = Marshal.load(b)
  raise "Bad unmarshal: item: #{item.inspect} item2: #{item2.inspect}" unless (item2 == item)
end

true
