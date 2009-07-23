#
#   irb/init.rb - irb initialize module
#     $Release Version: 0.9.5$
#     $Revision: 11708 $
#     $Date: 2007-02-13 08:01:19 +0900 (Tue, 13 Feb 2007) $
#     by Keiju ISHITSUKA(keiju@ruby-lang.org)
#
# --
#
#
#

module IRB

  # initialize config
  def IRB.setup(ap_path)
    IRB.init_config(ap_path)
    IRB.init_error
    IRB.parse_opts
    IRB.run_config
    IRB.load_modules

    cnf = IRB.conf
    unless cnf[:PROMPT][cnf[:PROMPT_MODE]]
      IRB.fail(UndefinedPromptMode, cnf[:PROMPT_MODE])
    end
  end

  # @CONF default setting
  def IRB.init_config(ap_path)
    # class instance variables
    @TRACER_INITIALIZED = false

    # default configurations
    cnf = IRB.conf
    unless ap_path and cnf[:AP_NAME]
      ap_path = File.join(File.dirname(File.dirname(__FILE__)), "irb.rb")
    end
    cnf[:AP_NAME] = File::basename(ap_path, ".rb")

    cnf[:IRB_NAME] = "irb"
    cnf[:IRB_LIB_PATH] = File.dirname(__FILE__)

    cnf[:RC] = true
    cnf[:LOAD_MODULES] = []
    cnf[:IRB_RC] = nil

    cnf[:MATH_MODE] = false
    cnf[:USE_READLINE] = false unless defined?(ReadlineInputMethod)
    cnf[:INSPECT_MODE] = nil
    cnf[:USE_TRACER] = false
    cnf[:USE_LOADER] = false
    cnf[:IGNORE_SIGINT] = true
    cnf[:IGNORE_EOF] = false
    cnf[:ECHO] = nil
    cnf[:VERBOSE] = nil

    cnf[:EVAL_HISTORY] = nil
    cnf[:SAVE_HISTORY] = nil

    cnf[:BACK_TRACE_LIMIT] = 16

    cnf[:PROMPT] = {
      :NULL => {
  :PROMPT_I => nil,
  :PROMPT_N => nil,
  :PROMPT_S => nil,
  :PROMPT_C => nil,
  :RETURN => "%s\n"
      },
      :DEFAULT => {
  :PROMPT_I => "%N(%m):%03n:%i> ",
  :PROMPT_N => "%N(%m):%03n:%i> ",
  :PROMPT_S => "%N(%m):%03n:%i%l ",
  :PROMPT_C => "%N(%m):%03n:%i* ",
  :RETURN => "=> %s\n"
      },
      :CLASSIC => {
  :PROMPT_I => "%N(%m):%03n:%i> ",
  :PROMPT_N => "%N(%m):%03n:%i> ",
  :PROMPT_S => "%N(%m):%03n:%i%l ",
  :PROMPT_C => "%N(%m):%03n:%i* ",
  :RETURN => "%s\n"
      },
      :SIMPLE => {
  :PROMPT_I => ">> ",
  :PROMPT_N => ">> ",
  :PROMPT_S => nil,
  :PROMPT_C => "?> ",
  :RETURN => "=> %s\n"
      },
      :INF_RUBY => {
  :PROMPT_I => "%N(%m):%03n:%i> ",
# :PROMPT_N => "%N(%m):%03n:%i> ",
  :PROMPT_N => nil,
  :PROMPT_S => nil,
  :PROMPT_C => nil,
  :RETURN => "%s\n",
  :AUTO_INDENT => true
      },
      :XMP => {
  :PROMPT_I => nil,
  :PROMPT_N => nil,
  :PROMPT_S => nil,
  :PROMPT_C => nil,
  :RETURN => "    ==>%s\n"
      }
    }

    cnf[:PROMPT_MODE] = (STDIN.tty? ? :DEFAULT : :NULL)
    cnf[:AUTO_INDENT] = false

    cnf[:CONTEXT_MODE] = 3 # use binding in function on TOPLEVEL_BINDING
    cnf[:SINGLE_IRB] = false

#    @CONF[:LC_MESSAGES] = "en"
    cnf[:LC_MESSAGES] = Locale.new

    cnf[:DEBUG_LEVEL] = 1
  end

  def IRB.init_error
    cnf = IRB.conf
    cnf[:LC_MESSAGES].load("irb/error.rb")
  end

  FEATURE_IOPT_CHANGE_VERSION = "1.9.0"

  # option analyzing
  def IRB.parse_opts
    load_path = []
    cnf = IRB.conf
    while opt = ARGV.shift
      case opt
      when "-f"
        cnf[:RC] = false
      when "-m"
        cnf[:MATH_MODE] = true
      when "-d"
         $DEBUG = true
      when /^-r(.+)?/
        opt = $1 || ARGV.shift
  	cnf[:LOAD_MODULES].push opt if opt
      when /^-I(.+)?/
        opt = $1 || ARGV.shift
        load_path.concat(opt.split(File::PATH_SEPARATOR)) if opt
      when /^-K(.)/
        $KCODE = $1
      when "--inspect"
        cnf[:INSPECT_MODE] = true
      when "--noinspect"
        cnf[:INSPECT_MODE] = false
      when "--readline"
        cnf[:USE_READLINE] = true
      when "--noreadline"
        cnf[:USE_READLINE] = false
      when "--echo"
        cnf[:ECHO] = true
      when "--noecho"
        cnf[:ECHO] = false
      when "--verbose"
        cnf[:VERBOSE] = true
      when "--noverbose"
        cnf[:VERBOSE] = false
      when "--prompt-mode", "--prompt"
        prompt_mode = ARGV.shift.upcase.tr("-", "_").intern
        cnf[:PROMPT_MODE] = prompt_mode
      when "--noprompt"
        cnf[:PROMPT_MODE] = :NULL
      when "--inf-ruby-mode"
        cnf[:PROMPT_MODE] = :INF_RUBY
      when "--sample-book-mode", "--simple-prompt"
        cnf[:PROMPT_MODE] = :SIMPLE
      when "--tracer"
        cnf[:USE_TRACER] = true
      when "--back-trace-limit"
        cnf[:BACK_TRACE_LIMIT] = ARGV.shift.to_i
      when "--context-mode"
        cnf[:CONTEXT_MODE] = ARGV.shift.to_i
      when "--single-irb"
        cnf[:SINGLE_IRB] = true
      when "--irb_debug"
        cnf[:DEBUG_LEVEL] = ARGV.shift.to_i
      when "-v", "--version"
        print IRB.version, "\n"
        exit 0
      when "-h", "--help"
        require "irb/help"
        IRB.print_usage
        exit 0
      when /^-/
        IRB.fail UnrecognizedSwitch, opt
      else
        cnf[:SCRIPT] = opt
        $0 = opt
        break
      end
    end
    if RUBY_VERSION >= FEATURE_IOPT_CHANGE_VERSION
      load_path.collect! do |path|
        /\A\.\// =~ path ? path : File.expand_path(path)
      end
    end
    $LOAD_PATH.unshift(*load_path)
  end

  # running config
  def IRB.run_config
    cnf = IRB.conf
    if cnf[:RC]
      begin
         load rc_file if rc_file
      rescue LoadError, Errno::ENOENT
      rescue Exception => e
        print "load error: #{e} on #{rc_file}\n"
        print $!.class, ": ", $!, "\n"
        #for err in $@[0, $@.size - 2]
        #  print "\t", err, "\n"
        #end
      end
    end
  end

  IRBRC_EXT = "rc"
  def IRB.rc_file(ext = IRBRC_EXT)
    cnf = IRB.conf
    wtf = nil
    if !cnf[:RC_NAME_GENERATOR]
      cnf[:RC_NAME_GENERATOR] = proc { |rc| wtf }
      rc_file_generators do |rcgen|
      if File.exist?(fn = rcgen.call(ext))
        wtf ||= fn
      end
      end
    end
    cnf[:RC_NAME_GENERATOR].call ext
  end

  # enumerate possible rc-file base name generators
  def IRB.rc_file_generators
    if irbrc = ENV["IRBRC"]
      yield proc{|rc|  rc == "rc" ? irbrc : irbrc+rc}
    end
    # GEMSTONE: Work around a bug.  The original code was:
    #  if home = ENV["HOME"]
    #    yield proc{|rc| home+"/.irb#{rc}"}
    #  end
    #
    # But home was getting set to nil, but this workaround seems to get us past it...
    home = ENV["HOME"]
    if home
      yield proc{|rc| home+"/.irb#{rc}"}
    end
    home = Dir.pwd
    yield proc{|rc| home+"/.irb#{rc}"}
    yield proc{|rc| home+"/irb#{rc.sub(/\A_?/, '.')}"}
    yield proc{|rc| home+"/_irb#{rc}"}
    yield proc{|rc| home+"/$irb#{rc}"}
  end

  # loading modules
  def IRB.load_modules
    cnf = IRB.conf
    for m in cnf[:LOAD_MODULES]
      begin
        require m
      rescue
        print $@[0], ":", $!.class, ": ", $!, "\n"
      end
    end
  end

end
