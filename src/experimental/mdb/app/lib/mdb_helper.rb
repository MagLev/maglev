module MDB
  # Manages the REST communication to MDB
  class Client
    def initialize(base_url)
      @url = base_url
    end

    def data_for(path)
      begin
        SERVER.get_content(SERVER_URI + path)
      rescue HTTPClient::BadResponseError => bre
        halt 404, "get_content: Error doing GET from database: #{bre.res.content}"
      end
    end

    def put_to(data, path)
      begin
        SERVER.put(SERVER_URI + path, data)
      rescue HTTPClient::BadResponseError => bre
        halt 404, "put_to: Error doing PUT to database: #{bre.res.content}"
      end
    end
  end
end
