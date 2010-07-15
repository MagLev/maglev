
$:.unshift(File.dirname(__FILE__))

begin
  require 'Trac757a'  # defines and raises Foo::FooException
rescue Foo::FooException => e
  if Foo::FooException === e
    puts "OK"
  else
    raise "Fail: caught wrong exception: #{e}"
  end
end

true
