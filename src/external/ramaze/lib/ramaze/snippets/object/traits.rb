#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

Traits = Hash.new{|h,k| h[k] = {}} unless defined?(Traits)

# Extensions for Object

class Object

  # Adds a method to Object to annotate your objects with certain traits.
  # It's basically a simple Hash that takes the current object as key
  #
  # Example:
  #
  #   class Foo
  #     trait :instance => false
  #
  #     def initialize
  #       trait :instance => true
  #     end
  #   end
  #
  #   Foo.trait[:instance]
  #   # false
  #
  #   foo = Foo.new
  #   foo.trait[:instance]
  #   # true

  def trait hash = nil
    if hash
      Traits[self].merge! hash
    else
      Traits[self]
    end
  end

  # builds a trait from all the ancestors, closer ancestors
  # overwrite distant ancestors
  #
  # class Foo
  #   trait :one => :eins
  #   trait :first => :erstes
  # end
  #
  # class Bar < Foo
  #   trait :two => :zwei
  # end
  #
  # class Foobar < Bar
  #   trait :three => :drei
  #   trait :first => :overwritten
  # end
  #
  # Foobar.ancestral_trait
  # {:three=>:drei, :two=>:zwei, :one=>:eins, :first=>:overwritten}

  def ancestral_trait
    if respond_to?(:ancestors)
      ancs = ancestors
    else
      ancs = self.class.ancestors
    end
    ancs.reverse.inject({}){|s,v| s.merge(v.trait)}.merge(trait)
  end

  # trait for self.class

  def class_trait
    if respond_to?(:ancestors)
      trait
    else
      self.class.trait
    end
  end
end
