require 'sinatra/base'

module Sinatra
  class Default < Base
    set :app_file, lambda {
      ignore = [
        /lib\/sinatra.*\.rb$/, # all sinatra code
        /\(.*\)/,              # generated code
        /custom_require\.rb$/  # rubygems require hacks
      ]
      path =
        caller.map{ |line| line.split(/:\d/, 2).first }.find do |file|
          next if ignore.any? { |pattern| file =~ pattern }
          file
        end
      path || $0
    }.call

    # GEMSTONE: To work around a bug in caller (only full pathnames are
    # provided), we compare basenames (which may be wrong in some
    # circumstances).
    #
    # set :run, Proc.new { $0 == app_file }   # Original Version
    #
    # set :run, Proc.new { File.basename($0) == File.basename(app_file) }
    set :run, Proc.new { true }
    # END GEMSTONE

    set :reload, Proc.new{ app_file? && development? }

    if run? && ARGV.any?
      require 'optparse'
      OptionParser.new { |op|
        op.on('-x')        {       set :mutex, true }
        op.on('-e env')    { |val| set :environment, val.to_sym }
        op.on('-s server') { |val| set :server, val }
        op.on('-p port')   { |val| set :port, val.to_i }
      }.parse!(ARGV.dup)
    end
  end
end

include Sinatra::Delegator

def helpers(&block)
  Sinatra::Application.send :class_eval, &block
end

def mime(ext, type)
  ext = ".#{ext}" unless ext.to_s[0] == ?.
  Rack::Mime::MIME_TYPES[ext.to_s] = type
end

at_exit do
  raise $! if $!
  Sinatra::Application.run! if Sinatra::Application.run?
end
