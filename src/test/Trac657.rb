# From http://copypastel.com/rofl/A_Maglev_Store-y

module Foo
  def self.included(klass)

    klass.class_eval do
      @@var = "Access me"

      # metaclass
      class << self
        def var
          @@var
        end
      end
    end

  end
end

class Bar
  include Foo
end

Bar.var
# => NameError: undefined class variable @@var




# This code also fails
#
#   $ maglev-ruby $pbm
#   ["@@store"]
#   error , undefined class variable @@store,
#             during /Users/pmclain/GemStone/dev/pbm.rb
#   ERROR 2023, Error, 'undefined class variable @@store' (NameError)
#
#   $ ruby $pbm
#   ["@@store"]
#   true

# module Persistable
#   def self.included(klass)
#     klass.class_eval do
#       @@store = [:foo]

#       class << self
#         include Enumerable
#         def each(&block)
#           @@store.each &block
#         end
#       end
#     end
#   end
# end

# class C
#   include Persistable
# end

# p C.class_variables
# p C.all?
