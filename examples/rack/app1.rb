require 'rack'

Rack::Handler::WEBrick.run proc { |env|
    [200, { "Content-Type" => "text/html"}, "hello from #{__FILE__}"]
  }, :Port => 9292
