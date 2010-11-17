# Rack middleware that works around a bug in how Lighttpd + FastCGI handles
# SCRIPT_NAME and PATH_INFO.
#
# What the app expects is for PATH_INFO to be something like "/app/home".
# Lighttpd sets PATH_INFO to be "/home" and SCRIPT_NAME to "/app", which
# confuses Rack URLMap with the map Sinatra gives it.  So, I re-write
# PATH_INFO here.
#
class FixLighttpdFastCGI
  def initialize(app)
    @app = app
  end

  def call(env)
    puts "---- BEFORE: PATH_INFO:   #{env['PATH_INFO']} "
    puts "---- BEFORE: SCRIPT_NAME: #{env['SCRIPT_NAME']} "

    env['PATH_INFO'] = env['SCRIPT_NAME'] + env['PATH_INFO']
    env['SCRIPT_NAME'] = ''

    puts "---- AFTER: PATH_INFO:   #{env['PATH_INFO']} "
    puts "---- AFTER: SCRIPT_NAME: #{env['SCRIPT_NAME']} "
    @app.call env
  end
end
