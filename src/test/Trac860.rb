# Found in Ramaze. To reproduce in Ramaze, run the following program and
# hit the main URL:
#
#   require 'rubygems'
#   require 'ramaze'
#   class MainController < Ramaze::Controller
#         def index
#           'Hello, World!'
#         end
#   end
#   Ramaze.start

module M
  def cache_store(key, value, options={ })
    puts "M::cache_store(#{key}, #{value}, #{options.inspect})"
  end
end

class X
  include M
  
  def cache_store(*args)
    # Maglev complains of an Argument error.  MRI sees this as a zsuper, so
    # passes in *args, so I guess that blocks do not invalidate this being
    # a zsuper call.
    super { |k,v| puts "#{k} => #{v}" }
  end
end

X.new.cache_store(10, 20)
