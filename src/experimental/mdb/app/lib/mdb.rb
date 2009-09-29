require 'rubygems'
require 'json'

module MDB
  # Manages the REST communication to MDB
  class REST
    require 'httpclient'

    def initialize(url)
      @url = url
      @server = HTTPClient.new
    end

    def mdb_delete(db)
      rest_op :delete, db
    end

    def mdb_get(path)
      rest_op :get, path
    end

    def mdb_put(path, data)
      rest_op :put, path, data
    end

    def mdb_post(path, data)
      rest_op :post, path, data
    end

    def from_json(obj)
      json = case obj
             when HTTP::Message
               obj.content
             else
               obj
             end
      # The MDB_SERVER app always wraps responses in an array,
      # so we always unpack the first element to get to the real
      # response
      JSON.parse(json)[0]
    end

    def rest_op(op, path, data = nil)
      begin
        url = @url + path
        from_json case op
                  when :get
                    @server.get_content url
                  when :put
                    @server.put url
                  when :delete
                    @server.delete url
                  when :post
                    @server.post url, data
                  end
      rescue HTTPClient::BadResponseError => bre
        raise "Error doing #{op} #{path} from database: #{bre.res.content}"
      end
    end
  end

  class RESTDatabase < REST
    def initialize(url, db_name)
      super(url)
      @db_name = db_name
    end

    # Return the result of running the named view on the server
    # NOTE: params not yet supported
    #
    # GET /:db/view/:view
    def execute_view(view_name, *params)
      mdb_get("/#{@db_name}/view/#{view_name}")
    end

    # GET /:db/:id
    def get(id)
      mdb_get("/#{@db_name}/#{id}")
    end

    # PUT /:db   returns docid
    def add(document)
      mdb_put("/#{@db_name}", document.to_json)
    end

    def size
      mdb_get("/#{@db_name}/send/size")
    end

    def list_ids
      mdb_get("/#{@db_name}/send/list_ids")
    end

    def clear
      mdb_get("/#{@db_name}/send/clear")
    end
  end

  class RESTServer < REST

    # Initialize a +RESTServer+ that will communicate with the remote
    # MDB::Server object over +url+.
    def initialize(url)
      super(url)
    end

    def create(db_name, view_class)
      mdb_post("/#{db_name}", ["view_class", view_class.name])
    end

    def update(db_name, view_class)

    end

    def delete(db_name)
      mdb_delete("/#{db_name}")
    end

    def [](db_name)
      RESTDatabase.new(@url, db_name)
    end

    def db_names
      raise NotImplementedError
    end
  end
end
