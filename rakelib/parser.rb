# A class to manage the old MRI Parser state
class Parser
  def self.running?
    ! parser_pid.nil?
  end

  # Starts a ruby parsetree_parser on PARSETREE_PORT (no checks for prior
  # running instances).
  def self.start
    unless valid_ruby_for_parser?
      puts "ERROR: #{PARSER_RUBY} won't run the Parse server,"
      puts "       ruby 1.8.6 patchlevel 287 with ParseTree 3.0.3 is required."
      puts "       Upgrade #{PARSER_RUBY} or set the environment variable"
      puts "       RUBY186P287 to point to a ruby 1.8.6 patchlevel 287 executable."
      exit 1
    end
    if running?
      puts "Parser already running..."
    else
      cd "#{MAGLEV_HOME}/bin" do
        sh %{
          nohup #{PARSER_RUBY} parsetree_parser.rb \
            >#{MAGLEV_HOME}/log/parsetree.log 2>/dev/null &
          echo "MagLev Parse Server process $! starting on port #{PARSETREE_PORT}"
        }
      end
      wait_until_started
    end
  end

  def self.wait_until_started
    10.times do
      if running?
        puts "MagLev Parse Server process running on port #{PARSETREE_PORT}"
        return true
      end
      puts "Waiting for MagLev Parse Server process to start..."
      sleep 2
    end
    puts "MagLev Parse Server process failed to start on port #{PARSETREE_PORT}"
    return false
  end

  # Tests for the parser on port PARSETREE_PORT, and kills it if found.
  # returns the PID of the killed process, or nil if no process found on the
  # parser port.
  def self.stop
    kill_pid = parser_pid
    sh %{ kill -9 #{kill_pid} } unless kill_pid.nil?
    parser_pid
  end
end
