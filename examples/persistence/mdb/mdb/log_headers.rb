class LogHeaders
  def initialize(app)
    @app = app
  end

  def call(env)
    print_headers(env, "REQUEST")
    puts "REQUEST BODY: #{env['rack.input'].string}"
    status, headers, body = @app.call env
    print_headers(env, "RESPONSE")
    [status, headers, body]
  end

  def print_headers(hash, msg)
    STDERR.puts "======== #{msg} Headers =========="
    hash.keys.sort.each do |key|
      puts "#{key} -> #{hash[key]}"
    end
  end

end
