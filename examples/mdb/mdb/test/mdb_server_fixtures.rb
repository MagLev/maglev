# Used by mdb_server.rb
Maglev.persistent do
  class AppModel
    attr_reader :a, :b
    def self.view_42(docs)
      42
    end
    def self.view_67(docs, count, ary)
      67 + count.to_i + ary.size
    end
    def self.view_53(documents)
      53
    end
    def self.view_66(documents, count)
      66 + count.to_i
    end
    def initialize(a, b)
      @a, @b = a, b
    end

    # The to_json method should return either an array or a hash of
    # attributes.  It should be JSON-ish data (i.e., strings, numbers, nil,
    # true, false, array (of JSON-ish), hash (of JSON-ish)).
    def to_json(*a)
      { 'a' => @a, 'b' => @b }.to_json(*a)
    end
  end
end
Maglev.commit_transaction


