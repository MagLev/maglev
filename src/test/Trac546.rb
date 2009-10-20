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
