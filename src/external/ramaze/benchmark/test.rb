# Usage:
#   $ ruby benchmark/test.rb
#
# or with darcs trackdown
#   $ cp benchmark/test.rb /tmp/bm.rb
#   $ darcs trackdown 'ruby /tmp/bm.rb; false'

begin
  require File.join(Dir.pwd, 'lib/ramaze')
rescue LoadError
  raise "Can't find lib/ramaze, are you in a ramaze src directory?"
end

ramaze = fork do
  class MainController < Ramaze::Controller
    engine :None
    def index() "Hello, World!" end
  end

  Ramaze::Log.loggers = []
  Ramaze.start :sessions => false, :sourcereload => false, :adapter => (ARGV[0] || :evented_mongrel).to_sym
end

sleep 2

# out = `ab -c 10 -n 1000 http://127.0.0.1:7000/ 2> /dev/null`
# out =~ /^Requests.+?(\d+\.\d+)/

out = `httperf --server=localhost --port=7000 --uri=/ --num-conns=500 --num-calls=1 2> /dev/null`
out =~ /^Request rate: (.+?)$/

puts $1

Process.kill('SIGKILL', ramaze)
Process.wait
