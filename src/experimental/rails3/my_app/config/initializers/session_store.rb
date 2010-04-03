# Be sure to restart your server when you modify this file.

# Your secret key for verifying cookie session data integrity.
# If you change this key, all old sessions will become invalid!
# Make sure the secret is at least 30 characters and all random, 
# no regular words or you'll be exposed to dictionary attacks.
ActionController::Base.session = {
  :key    => '_my_app_session',
  :secret => '1ae62e7b8bf510c448c39328f341378fb65d17082ed08550428967ea3453e16357d3b5727e43690eda605847ce8e825e433bb52f94051389ff5d00f8bf73bef5'
}

# Use the database for sessions instead of the cookie-based default,
# which shouldn't be used to store highly confidential information
# (create the session table with "rake db:sessions:create")
# ActionController::Base.session_store = :active_record_store
