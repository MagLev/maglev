# This file defines a class that has lots of interesting features.
#
# 1. The class is named to appear near the top of the class list (easy
#    access for demos).
# 2. The class has public, private and protected instance methods

Maglev.persistent do
  class AAADemo
    # Provide easy access to PERSISTENT_ROOT
    PROOT = Maglev::PERSISTENT_ROOT

    def self.foo
      "This is a class method"
    end

    def initialize
      @foo = "foo"
      @bar = "bar"
      @rand = rand(100)
      @a_hash = { :a => rand(51), :b => rand(72) }
    end

    protected
    def protected_method
      "This is a protected method"
    end

    private
    def private_method
      "This is a private method"
    end

    AN_INSTANCE = AAADemo.new
    AN_ARRAY = []
    30.times { |i| AN_ARRAY << i }
  end

  # Define a method programmatically

  AAADemo.define_method(:metaprogrammed) do
    x = :foo
    "This method metaprogrammed to return #{x}"
  end

end
