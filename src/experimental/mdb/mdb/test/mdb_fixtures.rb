STDERR.puts "== Committing the ViewClass : Maglev.persistent? #{Maglev.persistent?}"
Maglev.abort_transaction
Maglev.persistent do
  class ViewClass
    def self.view_42
      42
    end

    def self.document_added(id, doc)
      @count ||= 0
      @count += 1
    end

    def self.count
      @count
    end

    def self.reset_count
      @count = 0
    end
  end
  class ViewClass2
    def self.view_42
      43 # one better...
    end
  end
end
Maglev.commit_transaction
