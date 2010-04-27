MyApp::Application.configure do
  # Settings specified here will take precedence over those in config/environment.rb

  # In the development environment your application's code is reloaded on
  # every request.  This slows down response time but is perfect for development
  # since you don't have to restart the webserver when you make code changes.
  config.cache_classes = false

  # Log error messages when you accidentally call methods on nil.
  config.whiny_nils = true

  # Show full error reports and disable caching
  config.consider_all_requests_local       = true
  config.action_view.debug_rjs             = true
  config.action_controller.perform_caching = false

  # Don't care if the mailer can't send
  config.action_mailer.raise_delivery_errors = false
end

Exception.install_debug_block do |e|
  case e.class.name
  when 'RubyThrowException', 'RubyBreakException'
    nil
  when 'TypeError'
    # nil.pause if e.message =~ /_st_fileDescriptor=/
    nil
  when 'NameError'
    # nil.pause if e.message =~ /default_format not foun/
    nil.pause if e.message =~ /no method found for update_with_lock/
    nil
  when 'Errno::EPERM'
    # nil.pause
    nil
  when 'LoadError'
    # nil.pause if e.message =~ /no such file to load -- arel/
    nil
  else
    puts "-- #{e}    class (#{e.class}) class name: #{e.class.name}"
  end
end
