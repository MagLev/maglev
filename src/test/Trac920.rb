# From RubyGems 0.9.2 dependency_list.rb and an interaction
# with Rails 3.1.0rc4.

module Deprecate
  def deprecate
    p :foo
    :foo
  end
end

class DependencyList
  class << self
    extend Deprecate
    deprecate  # prints :foo
  end
end

raise "Fail: DependencyList.class has Deprecate as ancestor" if DependencyList.class.ancestors.include?(Deprecate)
# And this is a side effect of previous failure:
raise "Fail: method deprecate() available to DependencyList" unless DependencyList.methods.grep(/deprecate/).empty?

