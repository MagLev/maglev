#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

# StdLib
require 'abbrev'
require 'cgi'
require 'digest/md5'
require 'fileutils'
require 'ipaddr'
require 'optparse'
require 'ostruct'
require 'pathname'
require 'pp'
require 'set'
require 'socket'
require 'timeout'
require 'tmpdir'
require 'yaml'

begin
  require 'rubygems'
rescue LoadError
end

# Rack
require 'rack'
require 'rack/utils'
require 'rack/request'
require 'rack/response'

# The main namespace for Ramaze
module Ramaze
  BASEDIR = File.dirname(File.expand_path(__FILE__))
  $LOAD_PATH.unshift BASEDIR
  $LOAD_PATH.uniq!

  # Shortcut to the HTTP_STATUS_CODES of Rack::Utils
  # inverted for easier access

  STATUS_CODE = Rack::Utils::HTTP_STATUS_CODES.invert
end

Thread.abort_on_exception = true

# Bootstrap
require 'ramaze/version'
#require 'ramaze/reloader'
require 'ramaze/snippets'
require 'ramaze/log'
require 'ramaze/trinity'
require 'ramaze/dispatcher'
require 'ramaze/current'
require 'ramaze/adapter'
require 'ramaze/option'
require 'ramaze/cache'
require 'ramaze/tool'

# Startup
require 'ramaze/controller'

# Complete
require 'ramaze/template/ezamar'
require 'ramaze/contrib'
require 'ramaze/route'

module Ramaze

  # Each of these classes will be called ::startup upon Ramaze.startup

  trait :essentials => [
    Global, Cache, Contrib, Controller, Session, Adapter
  ]

  trait :started => false

  class << self

    # The one place to start Ramaze, takes an Hash of options to pass on to
    # each class in trait[:essentials] by calling ::startup on them.

    def startup options = {}
      options = options.to_hash

      force = options.delete(:force)
      force ||= !trait[:started]

      options[:runner] ||= caller[0][/^(.*?):\d+/, 1]
      Global.merge!(options)

      if force
        Log.info("Starting up Ramaze (Version #{VERSION})")
        trait[:started] = true

        trait[:essentials].each do |obj|
          obj.startup(options)
        end
      else
        Log.info "Ramaze already started, skipped start."
      end
    end

    # A shortcut for setting Ramaze.trait[:started] = true.

    def skip_start
      trait[:started] = true
    end

    # Forces the startup of Ramaze regardless if trait[:started] is set.

    def start!(options = {})
      trait[:started] = false
      startup(options)
    end

    # This will be called when you hit ^C or send SIGINT.
    # It sends ::shutdown to every class in trait[:essentials] and informs you
    # when it is done

    def shutdown
      Log.info "Initiate shutdown"

      Timeout.timeout(5) do
        trait[:essentials].each do |obj|
          obj.shutdown if obj.respond_to?(:shutdown)
        end

        puts "Ramazement is over, have a nice day."

        exit
      end
    rescue Timeout::Error
      puts "Shutdown timed out, issuing exit!"
      exit!
    end

    alias start startup
    alias stop shutdown
  end
end
