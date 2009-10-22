# Setup some persistent objects in the repository to test migration

Maglev.persistent do
  class FooV1  # Version 1 of Foo
    attr_reader :foo
    def initialize(i)
      @foo = i
    end
  end
end

foos = Maglev::PERSISTENT_ROOT[:foov1] = Array.new
10.times { |i| foos << FooV1.new(i) }
Maglev.commit_transaction

