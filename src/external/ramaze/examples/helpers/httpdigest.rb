require 'rubygems'
require 'ramaze'

REALM = 'ramaze authentication required'

class MainController < Ramaze::Controller

  helper :httpdigest

  def index
    %|
    <p><a href="#{Rs(:eyes_only)}">eyes only</a></p>
    <p><a href="#{R(SecretController,'/')}">secret area</a></p>
    <p><a href="#{R(GuestController,'/')}">guest area</a> username is <em>guest</em> password is <em>access</em></p>
     | 
  end

  def eyes_only
    httpdigest('eyes only',REALM) do |username|
      password = username.reverse
      MD5.hexdigest([username,REALM,password].join(':'))
    end
    "Shhhh don't tell anyone"
  end

end

class LoginController < Ramaze::Controller
  map '/login'

  helper :httpdigest

  def index
     @username ||= session[:username]
     @username ||= httpdigest('login area',REALM)
    "Hi there #@username!"
  end

  def login
    %|<form action="#{Rs(:post)}" method="post"><input type="text" name="username"/><input type="password" name="password"/><input type="submit"/></form>|
  end

  def post
    username = request.params["username"]
    password = request.params["password"]
    if password == "entry"
      session[:username] = username
      destination = session[ :redirect_after_login ]
      session.delete( :redirect_after_login )
      redirect destination
    end
    redirect Rs(:login)
  end

  protected

  def httpdigest_failure
    session[ :redirect_after_login ] = Rs(Ramaze::Action.current.method)
    redirect Rs(:login)
  end

end

class SecretController < Ramaze::Controller
  map '/secret'
  helper :aspect
  helper :httpdigest

  USERS = { 'admin' => 'secret', 'root' => 'password' }

  before_all do
    @username = httpdigest('secret area',REALM)
  end

  def index
    "Hello <em>#@username</em>, welcome to SECRET world"
  end

  protected

  def httpdigest_lookup_plaintext_password username
    USERS[ username ]
  end

end

class GuestController < Ramaze::Controller
  map '/guest'
  helper :aspect
  helper :httpdigest

  before_all do
    @username = httpdigest('guest area',REALM)
  end

  def index
    "Hello <em>#@username</em>, welcome to GUEST world."
  end

  protected

  def httpdigest_lookup_password username
    return "b71f15b2f6dd4834224fbe02169ed94c" if username == "guest"
  end

end

Ramaze.start
