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


class C
  def to_str
    'foo'
  end
end
test(Maglev::RubyUtils.rb_string_value(nil), '', "rb_string_value nil")
test(Maglev::RubyUtils.rb_string_value(''), '', "rb_string_value ''")
test(Maglev::RubyUtils.rb_string_value(C.new), 'foo', "rb_string_value to_str")
test(Maglev::RubyUtils.rb_string_value('fred'), 'fred', "rb_string_value 'fred'")

report
