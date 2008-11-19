# depends on: module.rb class.rb

# class << MAIN
#   def include(*mods)
#     Object.include(*mods)
#   end

#   def public(*methods)
#     Object.public(*methods)
#   end

#   def private(*methods)
#     Object.private(*methods)
#   end

#   def protected(*methods)
#     Object.protected(*methods)
#   end

#   def add_method(name, obj)
#     Object.add_method(name, obj)
#   end

#   def alias_method(new_name, current_name)
#     Object.__send__ :alias_method, new_name, current_name
#   end

#   def __const_set__(name, obj)
#     Object.__const_set__(name, obj)
#   end
# end

# def self.to_s
#   "main"
# end

# class NilClass
#   alias_method :|, :^

#   def call(*a)
#     raise LocalJumpError, "not callable"
#   end
# end

# NIL = nil

# class TrueClass
#   alias_method :inspect, :to_s
# end

# TRUE = true

# class FalseClass
#   alias_method :|, :^
#   alias_method :inspect, :to_s
# end

# FALSE = false

# Undefined = Object.new

##
# This is used to prevent recursively traversing an object graph.

module RecursionGuard
  # TODO move this module to a Gemstone file/directory since it
  #  has been changed to use Gemstone identity Set .

  def self.inspecting?(obj)
    stack._includes(obj)
  end

  def self.inspect(obj, &block)
    stack << obj
    begin
      yield
    ensure
      stack._remove(obj)
    end
  end

  def self.stack
    stack = Thread.current[:inspecting] ||= Set.new 
  end
end
