# # From rack-mount-0.6.12/lib/rack/mount/regexp_with_named_groups.rb
# require 'rubygems'
# $:.unshift "#{ENV['MAGLEV_HOME']}/lib/maglev/gems/1.8/gems/rack-mount-0.6.12/lib/rack/mount/vendor/multimap"
# require 'rack/mount/vendor/multimap/nested_multimap'


# module Rack
#   module Mount
#     class Multimap < NestedMultimap #:nodoc:
#       def store(*args)
# puts "-- Rack::Mount::Multimap#store(#{args.inspect})"
#         keys  = args.dup
#         value = keys.pop
#         key   = keys.shift

#         raise ArgumentError, 'wrong number of arguments (1 for 2)' unless value

#         unless key.respond_to?(:=~)
#           raise ArgumentError, "unsupported key: #{args.first.inspect}"
#         end

# puts "-- Rack::Mount::Multimap#store:   A: key: #{key}"
#         if key.is_a?(Regexp)
# puts "-- Rack::Mount::Multimap#store:   B"
#           if keys.empty?
#             @hash.each_pair { |k, l| l << value if k =~ key }
#             self.default << value
#           else
#             @hash.each_pair { |k, _|
#               if k =~ key
#                 args[0] = k
#                 super(*args)
#               end
#             }

#             self.default = self.class.new(default) unless default.is_a?(self.class)
#             default[*keys.dup] = value
#           end
#         else
# puts "-- Rack::Mount::Multimap#store:   C"
#           super(*args)
#         end
#       end
#       alias_method :[]=, :store

#       undef :index, :invert

#       def height
#         containers_with_default.max { |a, b| a.length <=> b.length }.length
#       end

#       def average_height
#         lengths = containers_with_default.map { |e| e.length }
#         lengths.inject(0) { |sum, len| sum += len }.to_f / lengths.size
#       end
#     end
#   end
# end

# graph = Rack::Mount::Multimap.new

# graph[*["posts", /GET/]]                      = "route 1"
# graph[*["posts", /POST/]]                     = "route 2"
# # graph[*["posts", /GET/, "new"]]               = "route 3"
# # graph[*["posts", /GET/, /\A([^\/.?]+)\Z/]]    = "route 4"
# # graph[*["posts", /PUT/, /\A([^\/.?]+)\Z/]]    = "route 5"
# # graph[*["posts", /DELETE/, /\A([^\/.?]+)\Z/]] = "route 6"
# # graph[*["posts", /GET/, /\A([^\/.?]+)\Z/]]    = "route 7"
# graph[*["\000"]]                              = 'route 8'



# p graph.keys
# p graph.size

class C
  def store(*args)
    p *args
    raise "Fail: expecting: 4  actual: #{args.size}" unless args.size == 4
  end
  alias_method :[]=, :store
end

c = C.new
c[*["a", "b", "c"]] = "d"
