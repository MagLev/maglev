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
      request(:delete, path)
      #handle_response @server.delete(@url + path)
    end

    def mdb_get(path)
      request(:get, path)
      #handle_response @server.get_content(@url + path)
    end

    def mdb_put(path, data)
      request(:put, path, @serializer.serialize(data))
      #handle_response @server.put(@url + path, @serializer.serialize(data))
    end

    def mdb_post(path, data)
      post_data = case data
                  when Hash
                    # URL encode the parameters
                    # @server.post(@url + path, data)
                    data
                  else
                    # @server.post(@url + path, @serializer.serialize(data))
                    @serializer.serialize(data)
                  end
      request(:post, path, post_data)
    end

    private

    def handle_response(response)
      result = case response
               when HTTP::Message
                 begin
                   case response.status
                   when 200..299
                     deserialize(response)
                   else
                     raise "Error doing #{@method} #{@path} STATUS: #{response.status}"
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

    # Issue a request based on op and provided data.
    # Returns an HTTP::Message object.
    def request(op, path, data = nil)
      @path = @url + path
      @method = op
      @data = data
      puts "--- request(#{@method}, #{@path}, #{@data}"
      handle_response case op
                      when :get
                        @server.get_content @path
                      when :put
                        @server.put @path
                      when :delete
                        @server.delete @path
                      when :post
                        @server.post @path, data
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
      mdb_post("/#{@db_name}", @serializer.serialize(document))
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
      result = mdb_post("/", :db_name => db_name, :view_class => view_class.to_s)
      if result == db_name
        RESTDatabase.new(@url, db_name)
      else
        raise result
      end
    end

#     def update(db_name, view_class)

#     end

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
