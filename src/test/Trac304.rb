class MyClass
end

# This should create an instance method
MyClass.class_eval do
  def instance_method
    puts "In MyClass#instance_method"
  end
end

# This should create a class method
MyClass.instance_eval do
  def class_method
    puts "In MyClass.class_method"
  end
end

MyClass.class_method   # MagLev blows up here
MyClass.new.instance_method
