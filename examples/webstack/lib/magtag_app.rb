require 'sinatra'
require 'magtag'


class MagTag < Sinatra::Base

  before do
    session[:foo] = Time.now
    @logged_in_user = User.find_by_name session[:logged_in_user]
    puts "\n=================================="
    puts "====== session:        #{session.inspect}"
    puts "====== logged in user: #{@logged_in_user.inspect}"
    puts "====== @request.url:   #{@request.request_method} #{@request.url}"
    puts "====== @request:       #{@request.inspect}"
    puts "====== params:         #{params.inspect}"
    if request.path_info !~ %r{/magtag\.css|/login|/signup|/debug} && @logged_in_user.nil?
      redirect '/login', 303
    end
  end

  get '/debug' do
    erb :debug
  end

  get '/home' do
    erb :home
  end

  get '/login' do  # show login form
    erb :login
  end

  post '/login' do # process login
    login(params['username'], params['password'])
  end

  get '/signup' do # show new user registration form
    erb :signup
  end

  post '/signup' do # process signup form
    begin
      user = User.signup(params['username'], params['password'], params['confirmpassword'])
      user.save
      login(params['username'], params['password'])
    rescue User::UserException => e
      @error = e.message
      erb :signup
    end
  end

  # Shared by login and by successful signup
  def login(user_name, password, target='/home')
    user = User.find_by_name user_name
    if user && user.login(password)
      session[:logged_in_user] = user.name
      redirect target
    else
      @error = "Incorrect username or password."
      erb :login
    end
  end
end
