class MyClass
end

# This should create an instance method
MyClass.class_eval do
  def instance_method
    puts "In MyClass#instance_method"
    33
  end
end

# This should create a class method
MyClass.instance_eval do
  def class_method
    puts "In MyClass.class_method"
    44
  end
end

unless MyClass.class_method() == 44 ; raise 'error'; end   
unless MyClass.new.instance_method() == 33 ; raise 'error'; end
true
