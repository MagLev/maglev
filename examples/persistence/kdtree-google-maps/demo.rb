require 'sinatra'
require 'sinatra/base'
require 'json/pure'

class Demo < Sinatra::Base

  set :server, %w[ webrick ]
  set :host, 'localhost'
  set :environment, :development
  set :app_file, __FILE__
  set :public, 'public'
  set :static, true

  TREE = Maglev::PERSISTENT_ROOT[:kdtree_demo_data]

  get '/' do
    # @lat =   48.724
    # @lon = -122.488
    # @k = 30
    erb :index
  end

  post '/nearest' do

    @lat = params[:lat].to_f
    @lon = params[:lon].to_f
    @k = params[:k].to_i

    @target = Collections::Point2D.new( @lon, @lat, :user_target )

    raw_results = TREE.nearest_k(@target, @k)

    @results = raw_results.map do |r|
      [r.value, r.value.spherical_miles(@target)]
    end.sort {|a,b| a[1] <=> b[1] }

    # retrieve the zipcode from the resultset.
    zipcode = @results[0][0].zip

    { "zipcode" => zipcode }.to_json
  end

end
