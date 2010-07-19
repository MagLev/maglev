require 'rubygems'

# Set up gems listed in the Gemfile.
gemfile = File.expand_path('../../Gemfile', __FILE__)
begin
  ENV['BUNDLE_GEMFILE'] = gemfile
  require 'bundler'
  Bundler.setup
rescue Exception => e
  # NOTE: This file has been patched for MagLev Trac 757
  # See https://magtrac.gemstone.com/ticket/757 for details
  if Bundler::GemNotFound === e
    STDERR.puts e.message
    STDERR.puts "Try running `bundle install`."
    exit!
  else
    raise
  end
end if File.exist?(gemfile)
