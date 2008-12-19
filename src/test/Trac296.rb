require File.expand_path('simple', File.dirname(__FILE__))

# In MRI, the first time foo is called @bar is undefined, so the
# initialization code runs.  In MagLev, @bar is defined, so initialization
# code never runs.
#
# This style is found in the ruby specs: rubyspec/1.8/core/dir/*
class C
  def foo
    unless defined? @bar
      puts "@bar undefined, defining it..."
      @bar = "bar"
    end
    @bar
  end
end

c = C.new
test(c.foo, "bar", "test 1")
test(c.foo, "bar", "test 2")

report

