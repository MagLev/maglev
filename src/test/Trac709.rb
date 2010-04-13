require 'forwardable'

class Multimap
  extend Forwardable

  def_delegators :@hash, :default

  def initialize(default = [])
    @hash = Hash.new(default)
  end

  def foo
    :foo.equal?(default)
  end
end

mm = Multimap.new
xx = mm.foo
unless xx.equal?(false) ; raise 'error'; end
true
