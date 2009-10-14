# These are the fixtures for database_tests.rb and server_tests.rb
STDERR.puts "== Committing the ViewClass : Maglev.persistent? #{Maglev.persistent?}"
Maglev.abort_transaction
Maglev.persistent do
  class ViewClass
    def self.view_42(docs)
      42
    end

    def self.view_55_plus(docs, count)
      55 + count.to_i
    end

    def self.view_66(docs, count)
      66 + count.to_i
    end

    def self.view_67(docs, count, ary)
      67 + count.to_i + ary.size
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
    def self.view_42(docs)
      43 # one better...
    end
  end

  module Foo
    module Bar
      class ViewClass3
        def self.view_42(docs)
          44 # better and better
        end
      end
    end
  end
end
Maglev.commit_transaction
