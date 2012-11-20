# https://github.com/MagLev/maglev/issues/167
#
# Wrong visibility when using class_eval

module MyModule
  def MyModule.included(base)
    base.module_eval do
      def test
        # Calling this method should work.
      end
    end
  end
end

class MyClass
  private
  include MyModule

  def should_be_private_again
    puts "Does not work."
  end
end

myclass = MyClass.new
myclass.test
begin
  myclass.should_be_private_again
  # Should never reach here
  raise "Method should be private."
rescue NoMethodError
  # Works.
end
