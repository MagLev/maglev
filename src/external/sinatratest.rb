$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Sinatra/lib')
$:.unshift(ENV['MAGLEV_HOME'] + '/src/external/Rack/lib')

require 'sinatra'

@default_options = {
  :dump_errors => true,
  :raise_errors => true
}
get '/' do
  "Hello world"
end
