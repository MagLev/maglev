
$:.unshift(File.dirname(__FILE__))

begin
  require File.expand_path('../Trac757a', __FILE__)  # defines and raises Foo::FooException
rescue Exception => e
  if Foo::FooException === e
    puts "OK"
  else
    raise "Fail: caught wrong exception: #{e}"
  end
end

true
