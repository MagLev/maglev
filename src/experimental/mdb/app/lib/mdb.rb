require 'rubygems'
require 'httpclient'

require 'mdb/serializer'

module MDB
  # Manages the REST communication to MDB and serialization
  class REST
    def initialize(url)
      @serializer = MDB::MarshalSerializer.new
      @url = url
      @server = HTTPClient.new
    end

    def mdb_delete(path)
      handle_result @server.delete(@url + path)
    end

    def mdb_get(path)
      handle_result @server.get_content(@url + path)
    end

    def mdb_put(path, data)
      handle_result @server.put(@url + path, @serializer.serialze(data))
    end

    def mdb_post(path, data)
      handle_result case data
                    when Hash
                      # URL encode the parameters
                      @server.post(@url + path, data)
                    else
                      @server.post(@url + path, @serializer.serialize(data))
                    end
    end

    private
    # Issue the HTTP request to the MDB server, and handle the result
    def rest_op(op, path, data = nil)
      begin
        result = request(op, path, data)
        puts "===== RESULT: #{result.inspect}"
        case result.status
        when 200..299
          deserialize(result)
        else
          raise "Error doing #{op} #{path} STATUS: #{result.status}"
        end
      rescue HTTPClient::BadResponseError => bre
        raise "Error doing #{op} #{path} from database: #{bre.res.content}"
      end
    end

    def handle_result(result)
      begin
        case result.status
        when 200..299
          deserialize(result)
        else
          raise "Error doing #{op} #{path} STATUS: #{result.status}"
        end
      rescue HTTPClient::BadResponseError => bre
        raise "Error doing #{op} #{path} from database: #{bre.res.content}"
      end
    end

    # Issue a request based on op and provided data.
    # Returns an HTTP::Message object.
    def request(op, path, data)
      url = @url + path
      case op
      when :get
        @server.get_content url
      when :put
        @server.put url
      when :delete

      when :post
        @server.post url, data
      end
    end

    def deserialize(obj)
      @serializer.deserialize case obj
                              when HTTP::Message
                                obj.content
                              else
                                obj
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
      mdb_put("/#{@db_name}", @serializer.serialize(document))
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
      mdb_post("/", :db_name => db_name, :view_class => view_class.to_s)
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
