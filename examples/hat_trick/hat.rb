# Define a Hat class that is persistent
Maglev.persistent do
  class Hat
    def initialize
      @contents = []
    end

    def put(item)
      @contents << item
      nil
    end

    def contents
      @contents
    end

    def size
      @contents.size
    end
  end

  ::HAT = Hat.new
end
Maglev.commit_transaction # Save HAT and the class definition

puts "::HAT: #{::HAT.inspect}"
