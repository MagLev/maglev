require 'rubygems'
require 'ramaze'

Ramaze.contrib :facebook

# Settings for your facebook app
#   SERVER: where this ramaze app is running
#   URL: the apps.facebook.com url to your app
#   KEY: API key for your app
#   SECRET: Secret key for your app
#   ID: Your app's ID (from the about page url)
#   ADMINS: uids of facebook users that are admins
#   SESSION: an admin's session key
#
# These vars are used by the Facebook Helper are are also useful
# within your controller and templates. For example,
#   <a href="#{Facebook::URL}/search">click here to search</a>
#
# In addition, set the following options on your app's 'Edit Settings' page:
#   Callback URL: http://myapp.domain.com:7000/ (trailing slash required)
#   Canvas Page URL: ramazing
#   Can your app be added on Facebook: Yes
#   Who can add your app: Users
#   Post-Add URL: http://apps.facebook.com/ramazing/install
#   Post-Remove URL: http://myapp.domain.com:7000/uninstall
#   Side Nav URL: http://apps.facebook.com/ramazing

module Facebook
  SERVER  = 'http://myapp.domain.com:7000'
  URL     = 'http://apps.facebook.com/ramazing'
  KEY     = 'b32f58685c06a7f8107bc7915354bd89'
  SECRET  = 'e0f9........................9429'
  ID      = 2502696782
  ADMINS  = [ 15601088 ]
  SESSION = ''
end unless defined? Facebook::ID

# The facebook helper (included your controller using +helper :facebook+)
# defines a +facebook+ object, which is also aliased to +fb+.
#
# POST parameters sent by the facebook servers can be accessed via fb.params
# and also fb[]. Wherever possible, values will be converted to native ruby
# types (Time objects, true/false, arrays)
#
#   fb.params = { :api_key => "b32f58685c06a7f8107bc7915354bd89",
#                 :added => true,
#                 :profile_update_time => Mon Oct 15 15:07:28 -0400 2007,
#                 :friends => [1,2,3],
#                 :session_key => "f7bed6aefaa5467b3a6344f2-15601088",
#                 :time => Sat Dec 01 02:02:50 -0500 2007,
#                 :user => 15601088,
#                 :position_fix => false,
#                 :expires => 0,
#                 :in_canvas => true }
#
#   fb[:user] == facebook[:user] == facebook.params[:user]
#
# The facebook object can also be used to make API calls. Native ruby types (true, false, strings, hashes and arrays)
# will be returned. If facebook returns an array with only one element, that element will be returned instead.
#
#   facebook.auth.createToken # => "41b5dc65792ba4bf405faf857a17267c"
#   fb.users.getInfo :uids => 15601088, :fields => [:name] # => {"name"=>"Aman Gupta", "uid"=>15601088}
#   fb.profile.setFBML :uid => 15601088, :markup => 'Hi!'  # => true
#
# API calls will use the current viewing users' session key (from fb[:session_key]) if present,
# or Facebook::SESSION, which should be set to one of the admin's keys. You can also provide another
# session key yourself from your users database:
#
#   fb.feed.publishActionOfUser :session_key => "the user's session key", :title => 'is using Ramaze!'
#
# Finally, you can use the following facebook object methods:
#
#   facebook.valid? # verify fb_sig, if invalid fb.params will return {}
#   facebook.redirect '/url' # redirect to /url using <fb:redirect/>
#   facebook.addurl # return the add url for the current app
#   facebook.addurl '/url' # add url for app that redirects to /url after successful install

class MainController < Ramaze::Controller
  helper :formatting
  helper :facebook

  before {
    # show some information about current user in logs
    # INFO   Facebook {:user=>15601088, :in_canvas=>true, :added=>true}
    if fb[:user]
      # use facebook session_key as session cookie
      session.session_id = fb[:session_key] if fb[:session_key]

      Ramaze::Log.info "Facebook " + fb.params.reject{|k,v| k.to_s !~ /^(in|is|user|added|locale|request)/}.inspect
    else
      # require_add: redirect to add url
      # fb.redirect fb.addurl
    end

    # suggest setting SESSION key if one is not set, and current user is an admin
    # INFO   Set a default session key: SESSION = 'b3638446fa02466210c49f42-15601088'
    if Facebook::SESSION.empty? and Facebook::ADMINS.include? fb[:user]
      Ramaze::Log.info "Set a default session key: SESSION = '#{fb[:session_key]}'"
    end
  }

  def install
    Ramaze::Log.info "#{fb[:user]} installed app" if request['installed'] == '1'
    facebook.profile.setFBML :uid => fb[:user], :markup => "Isn't this a great surprise!?"
    facebook.redirect request['next'] || '/'
  end

  def uninstall
    Ramaze::Log.info "#{fb[:user]} uninstalled app"
  end

  def main
    fb.redirect '/index' unless fb[:user]
    @userinfo = facebook.users.getInfo :uids => facebook[:user], :fields => [:name, :pic_square]
    %q(
      Hey there #{@userinfo['name']}. You look like this: <img src="#{@userinfo['pic_square']}"/>. <br/>
      You updated your profile #{time_diff fb[:profile_update_time]} ago. You have #{fb[:friends].size} friends. <br/>
      <p>
        <?r unless fb[:added] ?>
          I know you already logged in, but you should really <a href="#{facebook.addurl}">add this app</a>.
          You'll get a real nifty surprise on your profile if you do.
        <?r else ?>
          Now that you added this app, I can access your profile and news feed. <br/>
          Did you see the cool surprise I left on <a href="http://www.facebook.com/profile.php?id=#{fb[:user]}">your profile</a>?
        <?r end ?>
      </p>
    )
  end

  def index
    facebook.redirect '/main' if facebook[:user]
    %q(
      This is a great app. Here are some reasons you'll like it:
      <ul>
        <li>It's on facebook</li>
        <li>It's written in Ramaze</li>
      </ul>
      <a href="#{Facebook::URL}/main" requirelogin="true">Login</a> to start using it. Or, to
      get the full benefit, you should <a href="#{facebook.addurl '/main'}">add it</a> so we have
      access to your newsfeed and can add a box to your profile.
    )
  end

  def layout
    %q(
      <fb:dashboard>
        <?r unless fb[:added] ?>
          <fb:create-button href="#{facebook.addurl '/main'}">Add App</fb:create-button>
        <?r end ?>
      </fb:dashboard>
      <div id="content" style="margin: 0 2em 2em 2em">
        #@content
      </div>
    )
  end
  layout :layout
end

Ramaze.start :adapter => :mongrel
