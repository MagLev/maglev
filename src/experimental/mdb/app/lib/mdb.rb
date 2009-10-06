require 'rubygems'
require 'httpclient'

require 'mdb/serializer'

# TODO: Add proper Content-type header
module MDB
  # Manages the REST communication to MDB and serialization
  class REST
    def initialize(url)
      @serializer = MDB::MarshalSerializer.new
      @url = url
      @server = HTTPClient.new
    end

    def mdb_delete(path)
      request(:delete, path)
    end

    def mdb_get(path)
      request(:get, path)
    end

    def mdb_put(path, data)
      request(:put, path, @serializer.serialize(data))
    end

    def mdb_post(path, data)
#       if Hash === data
#         post_data = data
#       else
#         post_data = @serializer.serialize data
#       end
#       request(:post, path, post_data)
      request(:post, path, @serializer.serialize(data))
    end

    private


    # Issue a request based on op and provided data.
    # Returns an HTTP::Message object.
    def request(op, path, data = nil)
      @path = @url + path
      @method = op
      @data = data
      puts "--- request(#{@method}, #{@path}, #{@data}"
      response = case op
                 when :get
                   @server.get_content @path
                 when :put
                   @server.put @path
                 when :delete
                   @server.delete @path
                 when :post
                   #Content-Transfer-Encoding: binary
                   @server.post @path, data, {
          'Content-Type' => 'application/mdb',
          'Content-Transfer-Encoding' => "binary" }
#                    case data
#                    when Hash
#                      puts "=== post_content #{@path} #{@data.inspect}"
#                      @server.post_content @path, data
#                    else
#                      puts "=== post #{@path} #{@data.inspect}"
#                      @server.post @path, data
#                    end
                 end
      handle_response response
    end

    def handle_response(response)
      result = case response
               when HTTP::Message
                 begin
                   case response.status
                   when 200..299
                     deserialize(response)
                   else
                     raise "Error doing #{@method} #{@path} STATUS: #{response.status} #{response.body.content}"
                   end
                 rescue HTTPClient::BadResponseError => bre
                   raise "Error doing #{@method} #{@path} from database: #{bre.res.content}"
                 end
               else
                 # not an HTTP::Message
                 puts "--- handle_response(#{response.inspect})   class #{response.class}"
                 STDOUT.flush
                 @serializer.deserialize response
               end
      @path = @method = @data = nil
      result
    end

    def deserialize(obj)
      puts "--- deserialize(#{obj})"
      @serializer.deserialize case obj
                              when HTTP::Message
                                puts "--- deserialize content (#{obj.content})"
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

    # POST /:db   returns docid
    def add(document)
      mdb_post("/#{@db_name}", document)
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

    def key?(key)
      mdb_get("/#{key}")
    end

    def create(db_name, view_class)
      p = { :db_name => db_name, :view_class => view_class.to_s }
      result = mdb_post("/", p)
      if result == db_name
        RESTDatabase.new(@url, db_name)
      else
        raise result
      end
    end

    def delete(db_name)
      mdb_delete("/#{db_name}")
    end

    def [](db_name)
      # TODO: should we ask the server if db_name exists first?
      RESTDatabase.new(@url, db_name)
    end

    def db_names
      mdb_get("/")
    end
  end
end
