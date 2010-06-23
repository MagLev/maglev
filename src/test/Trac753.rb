# From Sinatra.
#
# Assigning to the variable used as the block parameter to a method, does
# not work.
#
# The problem is in Base.define_route

class Base
  def handle_route(base=self.class)
    puts "-- #{self} handle_route"
    # Note: self will be Application when we come through here,
    # i.e., @block_params is an instance variable on ::Application
    @block_params = "favicon.ico"
    routes = base.routes
    blk = routes
    route_eval(&blk)
  end

  def route_eval(&block)
    puts "-- #{self} route_eval"
    instance_eval(&block)
  end

  class << self
    attr_reader :routes

    def define_route(verb, path, options={}, &block)
      puts "-- #{self} define_route"

      define_method "#{verb} #{path}", &block
      unbound_method = instance_method("#{verb} #{path}")

      ##############################
      #  THIS IS THE PROBLEM
      ##############################

      buggy_code = true  # set buggy_code to false to get working code

      if buggy_code
        # This branch exhibits the bug.
        #
        # We reuse the block parameter, and assign something else to it,
        # which breaks.  I think the if block.arity ... end conditional is
        # also important for creating the bug.
        block =
          if block.arity != 0
            proc { unbound_method.bind(self).call(*@block_params) }
          else
            proc { unbound_method.bind(self).call }
          end
        @routes = block
      else
        # This branch works.  We use a different variable name, 'blk'
        # rather than 'block'.
        blk =
          if block.arity != 0
            proc { unbound_method.bind(self).call(*@block_params) }
          else
            proc { unbound_method.bind(self).call }
          end
        @routes = blk
      end
    end

    def get(path, opts={}, &block)
      puts "-- #{self} get"
      define_route('GET', path, opts, &block)
    end

  end
end

class Application < Base
end

class Delegator
  def self.get(*args, &b)
    puts "-- #{self} get (delegator)"
    ::Application.send(:get, *args, &b)
  end
end

puts "------ Define routes -------"
Delegator.get("/:foo", {}) do |username|
  puts "In block: username: #{username}"
  raise "Fail" unless username == "favicon.ico"
end

puts "------ Call routes -------"
Application.new.handle_route
