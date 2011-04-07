#=========================================================================
#
# Name:     demo_data.rb
#
# Purpose:  Define a persistent class with public, private and protected
#           instance methods, a class method, and some data we can explore
#           using WebTools. Name it AAADemo so it will appear at the top
#           of the Class list.
#
#=========================================================================

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

    # Add a few constants
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
