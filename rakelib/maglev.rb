# Support for the gemstone rake tasks
#
# These methods are the core building blocks used by the tasks in the
# rakefile.  There is (approximately) one method for each shell function in
# bin/gemstone.


# So many GemStone/S 64 processes depend on the environment variables, we
# just make these global.
MAGLEV_HOME = ENV['MAGLEV_HOME'] ||= File.expand_path("..", File.dirname(__FILE__))

PARSETREE_PORT = ENV['PARSETREE_PORT'] ||= "2001"
GEMSTONE = "#{MAGLEV_HOME}/gemstone"
TOPAZ_CMD ="#{GEMSTONE}/bin/topaz -q -I #{MAGLEV_HOME}/etc/.topazini -l "
TOPAZDEBUG_CMD = "#{GEMSTONE}/bin/topaz -I #{MAGLEV_HOME}/etc/.topazdebugini -l "
IRB_CMD = "$GEMSTONE/bin/topaz -q -I $MAGLEV_HOME/etc/.irbdebugini -l "

ENV['GEMSTONE_GLOBAL_DIR'] = MAGLEV_HOME
ENV['GEMSTONE_SYS_CONF']   = "#{MAGLEV_HOME}/etc/system.conf"
ENV['GEMSTONE_LOG']        = "#{MAGLEV_HOME}/log/gs64stone.log"
ENV['GEMSTONE']            = GEMSTONE

# RUBY186P287 must be set to a ruby 1.8.6 patchlevel 287 executable
# in order to run Parse Server. Earlier versions will fail.
PARSER_RUBY                = ENV['RUBY186P287'] || "ruby"

class String
  # This makes it nice to indent here documents and strip
  # out the leading space from the here document.  A here
  # document using this uses '|' to mark the beginning of line.
  # E.g.,
  #
  #   str = <<END.margin
  #       | This is the string.  All spaces before the '|
  #       | and the '|' will be stripped from each line...
  #   END
  #
  # From section 2.3 of The Ruby Way, 2nd Ed.
  def margin
    arr = self.split("\n")
    arr.map! { |x| x.sub!(/\s*\|/,"") }
    str = arr.join("\n")
    self.replace(str)
  end
end

# PARSE TREE SUPPORT

# Returns the PID of the process listening on PARSETREE_PORT, or nil.
def parser_pid
  `lsof -Fp -iTCP:#{PARSETREE_PORT}`.chomp[1..-1]
end

# Returns true iff there is a process listening on the PARSETREE_PORT
def parser_running?
  ! parser_pid.nil?
end

# Starts a ruby parsetree_parser on PARSETREE_PORT (no checks for prior
# running instances).
def start_parser
  cd "#{MAGLEV_HOME}/bin" do
    sh %{
      nohup #{PARSER_RUBY} parsetree_parser.rb \
          >#{MAGLEV_HOME}/log/parsetree.log 2>/dev/null &
      echo "MagLev Parse Server process $! starting on port $PARSETREE_PORT"
    }
  end
  wait_for_parser
end

# Starts a ruby parsetree_parser on PARSETREE_PORT (no checks for prior
# running instances).  Logs stderr to $MAGLEV_HOME/log/parsetree.err.
def start_parser_debug
  cd "#{MAGLEV_HOME}/bin" do
    sh %{
      nohup #{PARSER_RUBY} parsetree_parser.rb \
          >$MAGLEV_HOME/log/parsetree.log 2>$MAGLEV_HOME/log/parsetree.err &
      echo "MagLev DEBUG Parse Server process $! starting on port $PARSETREE_PORT"
    }
  end
  wait_for_parser
end

def wait_for_parser
  10.times do
    if parser_running?
      puts "MagLev Parser Server process started on port $PARSETREE_PORT"
      return true
    end
    puts "Waiting for parser to start..."
    sleep 2
  end
  puts "MagLev Parser Server process failed to start on port $PARSETREE_PORT"
  return false
end

# Tests for the parser on port PARSETREE_PORT, and kills it if found.
# returns the PID of the killed process, or nil if no process found on the
# parser port.
def stop_parser
  kill_pid = parser_pid
  sh %{ kill -9 #{kill_pid} } unless kill_pid.nil?
  parser_pid
end

# Returns true iff the GemStone server is running (gslit -clp)
def server_running?
  `#{GEMSTONE}/bin/gslist -clp`.tr("\n", ' ').strip != "0"
end

def start_netldi
  sh %{ ${GEMSTONE}/bin/startnetldi -g &>/dev/null } do |ok, status|
    raise "Couldn't start netldi #{ok}: #{status}" unless ok
  end
end

# Start the GemStone servers (startnetldi; startstone).  Does no checking
# if server is already started.
def start_server
  sh %{
    ${GEMSTONE}/bin/startstone gs64stone &>/dev/null
    ${GEMSTONE}/bin/waitstone gs64stone &>/dev/null
  } do |ok, status|
    puts "GemStone server gs64stone started" if ok
  end
end

# Start the GemStone servers with debug flags set (startnetldi;
# startstone).  Does no checking if server is already started.
def start_server_debug
  sh %{
    ${GEMSTONE}/bin/startnetldi -g
    ${GEMSTONE}/bin/startstone  -z ${MAGLEV_HOME}/etc/system-debug.conf gs64stone
    ${GEMSTONE}/bin/waitstone gs64stone &>/dev/null
  } do |ok, status|
    puts "GemStone server gs64stone started in verbose mode" if ok
  end
end

# Start the GemStone servers with larger page cache and /dev/null tranlogs
# startstone).  Does no checking if server is already started.
def start_server_bench
  sh %{
    ${GEMSTONE}/bin/startnetldi -g
    ${GEMSTONE}/bin/startstone  -z ${MAGLEV_HOME}/etc/system-benchmark.conf gs64stone
    ${GEMSTONE}/bin/waitstone gs64stone &>/dev/null
  } do |ok, status|
    puts "GemStone server gs64stone started with performance optimizations" if ok
  end
end

def stop_server
  sh %{
    ${GEMSTONE}/bin/stopstone gs64stone DataCurator swordfish -i >/dev/null 2>&1
  } do |ok, status|
    puts "GemStone server stopped." if ok
  end
end

def stop_netldi
  sh %{
    ${GEMSTONE}/bin/stopnetldi > /dev/null 2>&1
  } do |ok, status|
    puts "NetLDI stopped" if ok
  end
end

def status
  if server_running?
    puts "\nMAGLEV_HOME = #{MAGLEV_HOME}"
    sh %{ #{GEMSTONE}/bin/gslist -clv }
  else
    puts "GemStone server not running."
  end

  if parser_running?
    puts "\nMagLev Parse Server port = #{PARSETREE_PORT}"
    sh %{ lsof -P -iTCP:#{PARSETREE_PORT} }
    # if you don't have permission to run lsof, use the following instead
    # netstat -an | grep "[:.]$PARSETREE_PORT " | grep "LISTEN"
  else
    puts "MagLev Parse Server is not running on port #{PARSETREE_PORT}"
  end
end

def create_debug_script(code)
  script_name = "#{MAGLEV_HOME}/etc/rake_debug_script"
  sh %{
    cp #{MAGLEV_HOME}/etc/.topazdebugini #{script_name}
    cat - >> #{script_name} <<EOF
#{code}
EOF
  }
  script_name
end


def run_topaz(snippet, debug=false)
  sh %{ #{debug ? TOPAZDEBUG_CMD : TOPAZ_CMD} <<EOF
#{snippet}
EOF
  } do |ok, status|
    # TODO: Right now, topaz + maglev always exits with a non-zero error
    # count, so hide that
    # puts "topaz #{ok}  #{status}"
  end
  puts ""  # clean up after topaz command prompt
  true
end

# Run the topaz commands in +snippet+ in debug mode.  If an error or a
# pause is encountered, this method leaves you at the topaz command prompt
# with a live stack to work with.
def debug_topaz(snippet)
  # The key here, is to run topaz with stdin connected to the TTY.  In
  # order to do that, we combine the topaz login commands with the code
  # from +snippet+ into a temp file and pass that into topaz via +-I+.
  script = create_debug_script(snippet)
  sh %{
    #{GEMSTONE}/bin/topaz -I #{script} -l
    rm -f #{script}
  } do |ok, status|
    # TODO: Right now, topaz + maglev always exits with a non-zero error
    # count, so hide that
    # puts "topaz #{ok}  #{status}"
  end
  true
end

