require File.expand_path('simple', File.dirname(__FILE__))

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

# This array was generated from MRI.  The strings are the MRI 1.8.6
# marshaled strings for the given object.
test_items = [
              [Foo.new, "\004\bo:\bFoo\a:\t@bazi\006:\t@bar\"\nhello"],
              [1..4, "\004\bo:\nRange\b:\nbegini\006:\texclF:\bendi\t"],
              [nil, "\004\b0"],
              [true, "\004\bT"],
              [false, "\004\bF"],
              [:a_symbol, "\004\b:\ra_symbol"],
              ["xyz", "\004\b\"\bxyz"],
              [1, "\004\bi\006"],
              [2.71828, "\004\bf\0172.71828\000\367\220"],
              [/xyz/, "\004\b/\bxyz\000"],
              [["hello", 12], "\004\b[\a\"\nhelloi\021"],
              [{"one"=>"two"}, "\004\b{\006\"\bone\"\btwo"],
#              [18888888888888888888888888888888888, "\004\bl+\r8\216\3438\022\324'\367\243\216\271KK\243\003\000"],
#               Struct
             ]

test_items.each do |(item, marshal)|
  b = Marshal.dump(item)
  item2 = Marshal.load(b)
  #puts "=== #{item.inspect} => #{b.inspect}"
  test(item2, item, "Unmarshal: item: #{item.inspect} item2: #{item2.inspect}")

  item2 = Marshal.load(marshal)
  test(item2, item, "Unmarshal MRI: item: #{item.inspect} item2: #{item2.inspect}")
end

report
true
