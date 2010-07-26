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

  post '/zip_to_pos' do
    @zip = params[ :zip ].to_i

    # Find the zip code (note: in a real-world scenario, you'd 
    # probably want to use Maglev's indexing capabilities).
    # TODO: Tree.find would be nicer, but is currently not used here
    # (investigating an unusual issue with it).    
    val = nil
    TREE.each { |v|
      val = v if v.zip.to_i == @zip
    }
    #TREE.find { |val| val.zip.to_i == @zip }

    # We'll return an array, but it'll be empty if there was
    # no result.  Otherwise, it contains just one hash with
    # a few of the important fields we want to return/
    @results = []
    unless val.nil? then
      @results << { :latitude => val.lat, :longitude => val.lon }
    end
    
    @results.to_json
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
