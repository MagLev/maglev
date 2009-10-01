# Used by mdb_server.rb
Maglev.persistent do
  class AppModel
    attr_reader :a, :b
    def view_53
      53
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


