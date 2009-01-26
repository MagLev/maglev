#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

require 'ramaze/log/logging'
require 'ramaze/log/hub'
require 'ramaze/log/informer'

begin
  require 'win32console' if RUBY_PLATFORM =~ /win32/i
rescue LoadError => ex
  puts ex
  puts "For nice colors on windows, please `gem install win32console`"
  Ramaze::Logger::Informer.trait[:colorize] = false
end

module Ramaze
  autoload :Analogger, "ramaze/log/analogger.rb"
  autoload :Knotify,   "ramaze/log/knotify.rb"
  autoload :Syslog,    "ramaze/log/syslog.rb"
  autoload :Growl,     "ramaze/log/growl.rb"
  autoload :Xosd,      "ramaze/log/xosd.rb"
  autoload :Logger,    "ramaze/log/logger.rb"

  unless defined?(Log)
    Log = Logger::LogHub.new(Logger::Informer)
  end
end
