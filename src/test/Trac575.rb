# MRI calls the inherited callback before the body of the class is executed.
# MagLev calls the inherited callback after the body of the class is executed.
#
# Sinatra depends on the inherited callback occurrring before the body is evaluated.

$foo = nil

class Base
  class << self
    def print_foo
      p $foo
    end

    def inherited(subclass)
      $foo = subclass
      puts "#{self}.inherited(#{subclass}): $foo #{$foo.inspect}"
    end
  end
end

puts "#{self} before class D: $foo: #{$foo.inspect}"

class D < Base
  puts "#{self} top of class D: $foo: #{$foo.inspect}"
  raise 'Expecting non nil $foo' if $foo.nil?

  print_foo  # Sinatra uses methods like this to initialize things
             # and depends on Base#inherited already being called

  puts "#{self} bottom of class D: $foo: #{$foo.inspect}"
end

puts "After class D: $foo: #{$foo.inspect}"
