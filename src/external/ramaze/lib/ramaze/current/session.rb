#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

begin
  require 'digest/sha2'
rescue LoadError
  require 'digest/sha1'
end

require 'ramaze/current/session/flash'
require 'ramaze/current/session/hash'

module Ramaze

  # The purpose of Session is to hold key/value pairs like a Hash for a series
  # of # request/response cycles from the same client.
  #
  # The persistence is achieved by setting a cookie with the session_id to
  # the client, which is then passed back and forth until the cookie is either
  # deleted or expires.

  class Session
    # The unique id for the current session which is also passed on to the cookie.

    attr_accessor :session_id

    # This variable holds the current SessionFlash

    attr_accessor :flash

    # Flag whether or not to drop this session on the floor

    attr_accessor :dropped

    # secret salt for client-side session data

    trait :secret => 'change_me_please_123'

    # the key used for the cookie

    SESSION_KEY = '_ramaze_session_id' unless defined?(SESSION_KEY)

    # Holds counter for IPs

    IP_COUNT = ::Hash.new{|h,k| h[k] = OrderedSet.new} unless defined?(IP_COUNT)

    # Limit the number of sessions one IP is allowed to hold.

    IP_COUNT_LIMIT = 1000 unless defined?(IP_COUNT_LIMIT)

    # Holds the default cookie used for sessions

    COOKIE = { :path => '/' } unless defined?(COOKIE)

    # Shortcut for Current.session
    def self.current
      Current.session
    end

    # called from Ramaze::startup and adds Cache.sessions if cookies are
    # enabled

    def self.startup(options = {})
      Cache.add(:sessions) if Global.sessions
    end

    # retrieve a specific session given a session id

    def self.[](sess_id)
      Session.new(sess_id)
    end

    # generate a random (and hopefully unique) id for the current session.
    #
    # It consists of the current time, the current request, the current PID of
    # ruby and object_id of this instance.
    #
    # All this is joined by some calls to Kernel#rand and returned as a
    # Digest::SHA256::hexdigest

    def self.random_key
      h = [
        Time.now.to_f.to_s, rand,
        Current.request.hash, rand,
        Process.pid, rand,
        object_id, rand
      ].join
      Digest::SHA256.hexdigest(h)
    end

    # Initialize a new Session, requires the original Rack::Request instance
    # given to us from Dispatcher#setup_environment.
    #
    # sets @session_id and @session_flash

    def initialize(sess_or_request = Current.request)
      return unless Global.sessions

      if sess_or_request.respond_to?(:cookies)
        request = sess_or_request
        @session_id = request.cookies[SESSION_KEY] || Session.random_key
      else
        request = nil
        @session_id = sess_or_request
      end

      unless IP_COUNT.nil? or request.nil?
        ip = request.ip
        IP_COUNT[ip] << @session_id
        sessions.delete(IP_COUNT[ip].shift) if IP_COUNT[ip].size > IP_COUNT_LIMIT
      end

      @flash = Session::Flash.new(self)
      @current = nil
      @dropped = false
    end

    # relay all messages we don't understand to the currently active session

    def method_missing(*args, &block)
      current.send(*args, &block)
    end

    # answers with the currently active session, which is set unless it is
    # existing already, the session itself is an instance of SessionHash

    def current
      return @current if @current
      @current = ( sessions[session_id] ||= Session::Hash.new(self) )
    end

    # shortcut to Cache.sessions

    def sessions
      Cache.sessions
    end

    # Inspect on Session.current

    def inspect
      current.inspect
    end

    # don't finish this session

    def drop!
      self.dropped = true
    end

    # at the end of a request delete the current[:FLASH] and assign it to
    # current[:FLASH_PREVIOUS]
    #
    # this is needed so flash can iterate over requests
    # and always just keep the current and previous key/value pairs.
    #
    # finalizes the session and assigns the key to the response via
    # set_cookie.

    def finish
      return unless Global.sessions
      return if dropped

      old = current.delete(:FLASH)
      current[:FLASH_PREVIOUS] = old if old

      request, response = Current.request, Current.response

      hash = {:value => session_id}.merge(COOKIE)
      response.set_cookie(SESSION_KEY, hash)

      # set client side session cookie
      if val = request['session.client'] and
         (!val.empty? or request.cookies["#{SESSION_KEY}-client"])
        cookie = hash.merge(:value => marshal(val))
        response.set_cookie("#{SESSION_KEY}-client", cookie)
      end
    end
  end
end
