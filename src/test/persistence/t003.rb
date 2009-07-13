Maglev.persistent do
  class C003
    XYZ = 45
    @@foo = "foo"
    @bar = "bar"

    def self.foo
      @@foo
    end
    def self.bar
      @bar
    end
  end
end
Maglev.commit_transaction
