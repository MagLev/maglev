require 'sinatra'
require 'sinatra/base'
require 'json'

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

    # store the request parameters into instance variables.
    @lat = params[ :lat ].to_f
    @lon = params[ :lon ].to_f
    @k   = params[ :k ].to_i

    # retrieve the 2D data for the selected location.
    @target = Collections::Point2D.new( @lon, @lat, :user_target )

    # extract the k raw results for the target from the tree.
    raw_results = TREE.nearest_k( @target, @k )

    # generate an array of hashes and sort the locations in
    # ascending order by miles.
    @results = raw_results.map do |r|

      {
        :latitude  => r.value.lat,
        :longitude => r.value.lon,
        :city      => r.value.name,
        :state     => r.value.state,
        :zipcode   => r.value.zip,
        :miles     => r.value.spherical_miles( @target )
      }

    end.sort_by { |location| location[ :miles ] }

    @results.to_json

  end

end
