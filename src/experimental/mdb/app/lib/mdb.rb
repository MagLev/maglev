module MDB
  # Manages the REST communication to MDB
  class REST
    require 'httpclient'

    def initialize(url)
      @url = url
      @server = HTTPClient.new
    end

    def db_get(path)
      begin
        SERVER.get_content(@url + path)
      rescue HTTPClient::BadResponseError => bre
        halt 404, "get_content: Error doing GET from database: #{bre.res.content}"
      end
    end

    def db_put(path, data)
      begin
        SERVER.put(@url + path, data)
      rescue HTTPClient::BadResponseError => bre
        halt 404, "put_to: Error doing PUT to database: #{bre.res.content}"
      end
    end
  end

  class RESTDatabase < REST
    def initialize(url, db_name)
      @url = url
      @db_name = db_name
    end

    # Return the result of running the named view on the server
    # NOTE: params not yet supported
    #
    # GET /:db/view/:view
    def execute_view(view_name, *params)
      db_get("/#{@db_name}/view/#{view_name}")
    end

    # GET /:db/:id
    def get(id)
      db_get("/#{@db_name}/#{id}")
    end

    # PUT /:db   returns docid
    def add(document)
      db_put("/#{@db_name}", document)
    end
  end

  class RESTServer < REST

    # Initialize a +RESTServer+ that will communicate with the remote
    # MDB::Server object over +url+.
    def initialize(url)
      @url = url
    end

    def create(db_name, view_class)
      raise NotImplementedError
    end

    def update(db_name, view_class)
      raise NotImplementedError
    end

    def delete(db_name)
      raise NotImplementedError
    end

    def [](db_name)
      RESTDatabase.new(@url, db_name)
    end

    def db_names
      raise NotImplementedError
    end
  end
end
