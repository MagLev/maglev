require 'sinatra'
require 'magtag'

class MagTag < Sinatra::Base
  VERSION = '0.0.2'

  helpers do
    def is_logged_in?
      !@logged_in_user.nil?
    end

    # Returns either a static string, or a link to +page+, depending on
    # whether we are on that page or not.
    #
    # @param [String] page the page to generate a link for.  Should start with '/'.
    # @return [String] Either +page+, or an href to +page+.
    def nav_link_for(page)
      no_slash = page[1..-1]
      env['PATH_INFO'] == page ? no_slash : "<a href=\"#{page}\">#{no_slash}</a>"
    end

    def set_flash(msg)
      session[:flash] = msg
    end

    def get_flash
      msg = session[:flash]
      set_flash nil
      msg
    end

    def has_flash?
      !session[:flash].nil?
    end
  end

  before do
    session[:foo] = Time.now
    @logged_in_user = User.find_by_name session[:logged_in_user]
    if request.path_info !~ %r{/magtag\.css|/login|/signup|/debug|/setup|/info} && @logged_in_user.nil?
      redirect '/login', 303
    end
  end

  post '/tweet' do
    begin
      @logged_in_user.tweet params[:tweet]
      set_flash 'Tweet success!'
    rescue Exception => e
      set_flash e.message
    end
    redirect '/home'
  end

  get '/setup' do
    set_flash "setup done! login with user: pbm1 password: pbm1"
    users = []
    3.times do |i|
      name = "pbm#{i}"
      users << User.signup(name, name).save
    end
    # Everyone follows everyone else
    users.each { |u1| users.each { |u2| u1.follow u2 unless u1 == u2} }
    # Everyone tweets about it
    10.times { |i| users.each { |u| u.tweet("#{u.name}[#{i}] Hey... I'm following people!") } }
    erb :login
  end

  get '/debug' do
    erb :debug
  end

  # Print info about the ruby environment in text form.  This is used by
  # the performance testing scripts to document when and what was running
  # for a given test run.
  get '/info' do
    <<EOS
===== get /info ======================
RUBY_ENGINE  #{defined?(RUBY_ENGINE) ? RUBY_ENGINE : "MRI"}
Ruby         #{RUBY_VERSION}
Sinatra      #{Sinatra::VERSION}
Rack         #{Rack.release}
MagTag       #{MagTag::VERSION}
Date         #{Time.now}

========== Middleware ================
#{caller.grep(/in `call'/).join("\n")}
======================================
EOS
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

  get '/logout' do
    session[:logged_in_user] = nil
    redirect '/login'
  end

  get '/signup' do # show new user registration form
    erb :signup
  end

  post '/signup' do # process signup form
    begin
      user = User.signup(params['username'], params['password'], params['confirmpassword'])
      user.save
      login(params['username'], params['password']) # redirects to /home
    rescue User::UserException => e
      set_flash e.message
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
      set_flash "Incorrect username or password."
      erb :login
    end
  end
end
