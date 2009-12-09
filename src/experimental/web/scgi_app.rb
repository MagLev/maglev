require 'rack'
app = Proc.new do |env|
  [200, { "Content-Type" => "text/html"}, "hello from #{__FILE__}"]
end
Rack::Handler::SCGI.run(app, :Host => '127.0.0.1', :Port => '4567')
