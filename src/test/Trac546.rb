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
true
