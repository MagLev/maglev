# Extracted from rubygems 1.8.5 deprecate.rb
#
# Expected behavior:
#
#   $ ruby $pbm
#   10
#   WARN: foo deprecated
#   10
#
# MagLev behavior
#
#   $ mruby $pbm
#   10
#   error , a ArgumentError occurred (error 2718), too few args,
#                during /Users/pmclain/tmp/pbm.rb
#   ERROR 2718 , a ArgumentError occurred (error 2718), too few args (ArgumentError)
#

class F
  def self.deprecate(name)
    class_eval {

      old = "_deprecated_#{name}"
      alias_method old, name

      # In the rubygems source, there is the following comment next to this
      # define_method:
      #     # TODO: really works on 1.8.7?
      define_method name do |*args, &block|
        puts "WARN: #{name} deprecated"
        send old, *args, &block
      end

    }
  end

  def foo
    10
  end
end

p F.new.foo  # before deprecation, foo() runs fine

class F; deprecate(:foo); end

p F.new.foo  # Problem after deprecation
#################### Trac Info
# ID:         911
# Summary:    Problem with RubyGems 1.8.5: argument error with alias and define_method
# Changetime: 2011-06-06 23:24:49+00:00
###

#  {{{
#  # Extracted from rubygems 1.8.5 deprecate.rb
#  #
#  # Expected behavior:
#  #
#  #   $ ruby $pbm
#  #   10
#  #   WARN: foo deprecated
#  #   10
#  #
#  # MagLev behavior
#  #
#  #   $ mruby $pbm
#  #   10
#  #   error , a ArgumentError occurred (error 2718), too few args,
#  #                during /Users/pmclain/tmp/pbm.rb
#  #   ERROR 2718 , a ArgumentError occurred (error 2718), too few args (ArgumentError)
#  #
#  
#  class F
#    def self.deprecate(name)
#      class_eval {
#  
#        old = "_deprecated_#{name}"
#        alias_method old, name
#  
#        # In the rubygems source, there is the following comment next to this
#        # define_method:
#        #     # TODO: really works on 1.8.7?
#        define_method name do |*args, &block|
#          puts "WARN: #{name} deprecated"
#          send old, *args, &block
#        end
#  
#      }
#    end
#  
#    def foo
#      10
#    end
#  end
#  
#  p F.new.foo  # before deprecation, foo() runs fine
#  
#  class F; deprecate(:foo); end
#  
#  p F.new.foo  # Problem after deprecation
#  }}}
#  