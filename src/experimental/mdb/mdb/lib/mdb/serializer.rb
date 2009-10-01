module MDB
  class MarshalSerializer
    # A fake content type for ruby marshal objects
    def content_type
      'application/mdb'
    end

    def serialize(obj)
      Marshal.dump obj
    end

    def deserialize(string)
      Marshal.load string
    end
  end

  class JSONSerializer
    def content_type
      'application/json'
    end

    def serialize(obj)
      # We always wrap a response in an array, so that responses like bare
      # strings have a container.  The client assumes all responses are
      # JSON arrays, and will extract the first element as the content.
      JSON.generate [obj]
    end

    def deserialize(string)
      JSON.parse(string)[0]
    end
  end
end
