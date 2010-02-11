require File.expand_path('simple', File.dirname(__FILE__))

test(Maglev::RubyUtils.rb_path2class("::Object"), Object, "A: rb_path2class")

begin
  x = Maglev::RubyUtils.rb_path2class("::Foo::Bar::Quux")
  failed_test("expected ArgumentError but got none", nil, nil)
rescue ArgumentError
  # Ok
rescue Exception => e
  failed_test("expected ArgumentError but got none", ArgumentError, e.class)
end
