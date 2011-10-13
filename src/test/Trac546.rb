# Distilled from RubyGems
module Gem
  class Specification

    def initialize
      yield self if block_given?
    end

    def self.overwrite_accessor(name, &block)
      puts "#{self}.overwrite_accessor(#{name}, #{block_given?})"
      define_method(name, &block)
    end

    overwrite_accessor :version= do |version|
      @version = version
      return @version          # removing this line "fixes" the issue...
    end

  end
end

spec_code=<<EOS
  Gem::Specification.new do |s|
    s.version = 10
  end
EOS

xx = eval(spec_code, binding, "foo")

# more test cases from recent (15Oct09) rubyspecs
$A = "aa"
class C546
  def ma
       lambda do
         begin
           return 98
         ensure
           $A = $A + "bb"
         end
       end
  end
  def mb
       lambda do
         begin
           break 97 
         ensure
           $A = $A + "cc"
         end
       end
  end
end

x = C546.new.ma.call
unless x == 98 ; raise 'error'; end
y = C546.new.mb.call
unless y == 97 ; raise 'error'; end
unless $A == 'aabbcc' ; raise 'error'; end

true
#################### Trac Info
# ID:         546
# Summary:    Can't return from current Activation - define_method needs decompliation logic
# Changetime: 2009-07-06 21:04:00+00:00
###

#  Ruby Gems 1.3.4 runs into this trying to install the second gem:
#  
#  
#  {{{
#  $ maglev-ruby src/test/TracXXX.rb 
#  -- RubyFile>>load  : loading /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  Gem::Specification.overwrite_accessor(version=, true)
#  error , GemStone Smalltalk execution could not return from the current Activation. ' ' Home context of block to return from may no longer be active.,
#            during /Users/pmclain/projects/maglev/git/src/test/TracXXX.rb
#  ERROR 2079, GemStone Smalltalk execution could not return from the current Activation. ' ' Home context of block to return from may no longer be active.GemStone Smalltalk execution could not return from the current Activation. ' ' Home context of block to return from may no longer be active.
#  topaz 1> exit
#  
#  }}}
#  
#  The code:
#  
#  
#  {{{
#  # Distilled from RubyGems
#  module Gem
#    class Specification
#  
#      def initialize
#        yield self if block_given?
#      end
#  
#      def self.overwrite_accessor(name, &block)
#        puts "#{self}.overwrite_accessor(#{name}, #{block_given?})"
#        define_method(name, &block)
#      end
#  
#      overwrite_accessor :version= do |version|
#        @version = version
#        return @version          # removing this line "fixes" the issue...
#      end
#  
#    end
#  end
#  
#  spec_code=<<EOS
#    Gem::Specification.new do |s|
#      s.version = 10
#    end
#  EOS
#  
#  eval(spec_code, binding, "foo")
#  
#  }}}
#  
#  