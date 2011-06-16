# From RubyGems 0.9.2 dependency_list.rb and an interaction
# with Rails 3.1.0rc4.

$aa = []
module Deprecate
  def deprecate(a)
    $aa << a
  end
end

class DB
  extend Deprecate
  deprecate(6)
end

class DependencyList
  class << self
    extend Deprecate
    deprecate(5)
  end
end


db = DB
dl = DependencyList

unless $aa == [6, 5] ; raise 'fail'; end

raise "Fail: DependencyList.class has Deprecate as ancestor" if DependencyList.class.ancestors.include?(Deprecate)
# And this is a side effect of previous failure:
raise "Fail: method deprecate() available to DependencyList" unless DependencyList.methods.grep(/deprecate/).empty?

true
