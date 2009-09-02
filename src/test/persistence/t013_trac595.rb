Maglev.persistent do
  class Tag
#    attr_reader :name
    def initialize(name)
      @name = name
    end
  end
end
Maglev.commit_transaction
t = Tag.new("fred")
