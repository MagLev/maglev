# Distilled from lib/ruby/1.8/singleton.rb
#
# rubygems => rdoc which uses singleton.

module Singleton
  sb = self
  class << self
    sc = self
  end
end

class << Singleton
  sa = self  # should have sa == sc
  FirstInstanceCall = proc do
    @__instance__ = new   # => Undefined method `new' for Singleton
  end

  def __init__(klass)
    class << klass
      ss = self
      define_method(:instance, FirstInstanceCall)
    end
    klass
  end

  def included(klass)
    Singleton.__init__(klass)
  end
end

class Options
  include Singleton
end

xx = Options.instance
unless xx.class.equal?(Options) ; raise 'error' ; end
true
