# Support for the gemstone rake tasks
#
# These methods are the core building blocks used by the tasks in the
# rakefile.  There is (approximately) one method for each shell function in
# bin/gemstone.


# So many GemStone/S 64 processes depend on the environment variables, we
# just make these global.
MAGLEV_HOME = ENV['MAGLEV_HOME'] ||= File.expand_path("..", File.dirname(__FILE__))

PARSETREE_PORT = ENV['PARSETREE_PORT'] ||= "2001"
GEMSTONE = ENV['GEMSTONE'] || "#{MAGLEV_HOME}/gemstone"
TOPAZ_CMD ="#{GEMSTONE}/bin/topaz -q -I #{MAGLEV_HOME}/etc/.topazini -l "
TOPAZDEBUG_CMD = "#{GEMSTONE}/bin/topaz -I #{MAGLEV_HOME}/etc/.topazini -l "
IRB_CMD = "$GEMSTONE/bin/topaz -q -I $MAGLEV_HOME/etc/.irbdebugini -l "


# Maglev doesn't allow changes to $GEMSTONE* variables during execution
# (i.e., you can't change the stone your connected to, once you've
# connected).  Assume that if $GEMSTONE is set correctly, then all the
# others are too.
if ENV['GEMSTONE'].nil? or ENV['GEMSTONE'].empty?
  ENV['GEMSTONE_GLOBAL_DIR'] = MAGLEV_HOME
  ENV['GEMSTONE_SYS_CONF']   = "#{MAGLEV_HOME}/etc/system.conf"
  ENV['GEMSTONE_DATADIR']    = "#{MAGLEV_HOME}/data/maglev"
  ENV['GEMSTONE_LOG']        = "#{MAGLEV_HOME}/log/maglev/maglev.log"
  ENV['GEMSTONE']            = GEMSTONE
end

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

# Returns true iff there is a valid ruby 186 patchlevel 287 to run the parser
# Depends on parsetree_verscheck.rb printing 'installed'.
def valid_ruby_for_parser?
  rubyversion = `#{PARSER_RUBY} --version`
  parsetree_check = `#{PARSER_RUBY} #{MAGLEV_HOME}/bin/parsetree_verscheck.rb`
  (rubyversion.include? "ruby 1.8.6") \
  && (rubyversion.include? "patchlevel 287") \
  && (parsetree_check.include? "installed")
end


# Returns true iff the GemStone server is running (gslist -clp)
def server_running?
  `#{GEMSTONE}/bin/gslist -clp`.tr("\n", ' ').strip != "0"
end

# Start the NetLDI but hide output
def start_netldi
  logout = "#{MAGLEV_HOME}/log/rake.out"
  logerr = "#{MAGLEV_HOME}/log/rake.err"
  rm_f logout
  rm_f logerr
  begin
    sh %{ ${GEMSTONE}/bin/startnetldi -g -a #{ENV['USER']} >#{logout} 2> #{logerr} } do |ok, status|
      unless ok
        puts "== STDOUT ====="
        puts IO.readlines(logout)
        puts "== STDERR ====="
        puts IO.readlines(logerr)
        puts "==============="
        raise "Couldn't start netldi #{ok}: #{status}"
      end
    end
  ensure
    rm_f logout
    rm_f logerr
  end
end

# Start the NetLDI with visible output
def start_netldi_debug
  sh %{ ${GEMSTONE}/bin/startnetldi -g -a #{ENV['USER']} } do |ok, status|
    raise "Couldn't start netldi #{ok}: #{status}" unless ok
  end
end

# Start the GemStone server (startstone).  Does no checking
# if server is already started.
def start_server
  sh %{
    ${GEMSTONE}/bin/startstone maglev >/dev/null 2>&1
    ${GEMSTONE}/bin/waitstone maglev >/dev/null 2>&1
  } do |ok, status|
    puts "GemStone server maglev started" if ok
    ok
  end
end

# Start the GemStone server with debug flags set (startstone).
# Does no checking if server is already started.
def start_server_debug
  sh %{
    ${GEMSTONE}/bin/startstone  -z ${MAGLEV_HOME}/etc/system-debug.conf maglev
    ${GEMSTONE}/bin/waitstone maglev &>/dev/null
  } do |ok, status|
    puts "GemStone server maglev started in verbose mode" if ok
    ok
  end
end

# Start the GemStone servers with larger page cache and /dev/null tranlogs
# startstone).  Does no checking if server is already started.
def start_server_bench
  sh %{
    ${GEMSTONE}/bin/startstone  -z ${MAGLEV_HOME}/etc/system-benchmark.conf maglev >/dev/null 2>&1
    ${GEMSTONE}/bin/waitstone maglev >/dev/null 2>&1
  } do |ok, status|
    puts "GemStone server maglev started with performance optimizations" if ok
    ok
  end
end

def prims_loaded?
  sh %{ #{TOPAZ_CMD} << EOF >/dev/null
set user DataCurator pass swordfish
login
obj RubyPrimsLoaded
quit
EOF
  } do |ok, status|
    #puts "prims_loaded: #{ok}  status: #{status.inspect}"
    ok
  end
end

def load_prims
  Parser.start unless Parser.running?
  sh %{ #{TOPAZ_CMD} << EOF >/dev/null
input #{MAGLEV_HOME}/gemstone/upgrade/ruby/allprims.topaz
EOF
  } do |ok, status|
    #puts "load_prims_new ok: #{ok}  status: #{status.inspect}"
    ok
  end
end

def ensure_prims_loaded
  puts "Loading kernel if needed -- it may take a few seconds..."
  if prims_loaded?
    puts "Primitives already loaded"
  else
    puts "loading prims"
    load_prims
  end
end

def stop_server
  sh %{
    ${GEMSTONE}/bin/stopstone maglev DataCurator swordfish -i >/dev/null 2>&1
  } do |ok, status|
    puts "GemStone server stopped." if ok
    ok
  end
end

def stop_netldi
  sh %{
    ${GEMSTONE}/bin/stopnetldi > /dev/null 2>&1
  } do |ok, status|
    puts "NetLDI stopped" if ok
    ok
  end
end

def status
  if server_running?
    puts "\nMAGLEV_HOME = #{MAGLEV_HOME}"
    sh %{ #{GEMSTONE}/bin/gslist -clv }
  else
    puts "GemStone server not running."
  end

  if Parser.running?
    puts "\nMagLev Parse Server port = #{PARSETREE_PORT}"
    sh %{ lsof -P -iTCP:#{PARSETREE_PORT} }
    # if you don't have permission to run lsof, use the following instead
    # netstat -an | grep "[:.]#{PARSETREE_PORT} " | grep "LISTEN"
  else
    puts "MagLev Parse Server is not running on port #{PARSETREE_PORT}"
  end
end

def create_debug_script(code)
  script_name = "#{MAGLEV_HOME}/etc/rake_debug_script"
  sh %{
    cp #{MAGLEV_HOME}/etc/.topazini #{script_name}
    cat - >> #{script_name} <<EOF
login
#{code}
EOF
  }
  script_name
end


def run_topaz(snippet, debug=false)
  sh %{ #{debug ? TOPAZDEBUG_CMD : TOPAZ_CMD} <<EOF
login
#{snippet}
EOF
  } do |ok, status|
    ok
  end
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
    ok
  end
end

