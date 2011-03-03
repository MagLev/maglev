# In other implementation this file gets created by mkconfig.rb when ruby
# is built.  Since MagLev potentially supports VMs from multiple
# architectures connecting to the same stone, we have to figure out
# architecture specific information at runtime.
#
# To prevent a VM from one architecture (e.g., Solaris/Sparc) from
# persisting configurations values that will mess up a VM from another
# architecture (e.g., OSX x86), we wrap most of the code in a transient
# block.  Then, we remove this file from $LOADED_FEATURES, so that it will
# be re-loaded if needed.  We should really only remove if from the
# persistent $LOADED_FEATURES, but we don't support that yet.
#
Maglev.transient do
module Config
  RUBY_VERSION == "1.8.7" or
    raise "ruby lib version (1.8.7) doesn't match executable version (#{RUBY_VERSION})"
  VERSION = '1.8'
  ARCH = `uname -m`.chomp

  # Note: MAGLEV_HOME and TOPDIR should end up being the same
  # in a default installation
  MAGLEV_HOME = ENV['MAGLEV_HOME']

  # TODO: MRI puts rbconfig in the architecture specific subdir of
  # .../lib/ruby/1.8/<archdir>/rbconfig.rb
  TOPDIR = File.dirname(__FILE__).chomp!("/lib/ruby/1.8")
  DESTDIR = '' unless defined? DESTDIR

  CONFIG = {}
  CONFIG['prefix']            = MAGLEV_HOME
  CONFIG['exec_prefix']       = MAGLEV_HOME
  CONFIG['bindir']            = File.join(MAGLEV_HOME, 'bin')
  CONFIG['sysconfdir']        = File.join(MAGLEV_HOME, 'etc')
  CONFIG['includedir']        = File.join(MAGLEV_HOME, 'lib/ruby/1.8/include')
  CONFIG['libdir']            = File.join(MAGLEV_HOME, 'lib')
  CONFIG['gemlibdir']         = File.join(MAGLEV_HOME, 'gemstone', 'lib')

  CONFIG['rubylibdir']        = File.join(CONFIG['libdir'], 'ruby', VERSION)
  CONFIG['archdir']           = File.join(CONFIG['rubylibdir'], ARCH)
  CONFIG['topdir']            = CONFIG['archdir']

  CONFIG['sitedir']           = File.join(CONFIG['libdir'], 'ruby', 'site_ruby')
  CONFIG['sitelibdir']        = File.join(CONFIG['sitedir'], VERSION)
  CONFIG['sitearchdir']       = File.join(CONFIG['sitelibdir'], ARCH)

  CONFIG['DESTDIR']           = DESTDIR

  # CONFIG['sbindir']         = File.join(MAGLEV_HOME, 'sbin')
  # CONFIG['libexecdir']      = File.join(MAGLEV_HOME, 'libexec')
  CONFIG['datadir']           = File.join(MAGLEV_HOME, 'share')
  CONFIG['datarootdir']       = File.join(MAGLEV_HOME, 'share')
  CONFIG['docdir']            = File.join(CONFIG['datarootdir'], 'doc')
  CONFIG['infodir']           = File.join(CONFIG['datarootdir'], 'info')
  CONFIG['htmldir']           = CONFIG['docdir']

  CONFIG['vendordir']         = File.join(CONFIG['libdir'], 'vendor_ruby')
  CONFIG['vendorlibdir']      = File.join(CONFIG['vendordir'], VERSION)
  CONFIG['vendorarchdir']     = File.join(CONFIG['vendorlibdir'], ARCH)

  # TODO SHELL
  # TODO PATH_SEPARATOR
  CONFIG['arch']              = ARCH
  CONFIG['ruby_version']      = '1.8'
  cpu_os = Exception.__cpu_os_kind
  CONFIG['host_os']           = %w( not_used sparc_solaris linux-gnu PowerPC_AIX
                                    darwin9.0 x86_64_solaris Itanium_HP-UX)[ cpu_os  - 1]
  CONFIG['target_os']       = CONFIG['host_os']
  CONFIG["LN_S"]            = "ln -s"
  CONFIG["SET_MAKE"]        = ""
  CONFIG["INSTALL"]         = "install -vp"
  CONFIG["INSTALL_PROGRAM"] = "$(INSTALL)"
  CONFIG["INSTALL_SCRIPT"]  = "$(INSTALL)"
  CONFIG["INSTALL_DATA"]    = "$(INSTALL) -m 644"
  CONFIG["RM"]              = "rm -f"
  CONFIG["CP"]              = "cp"
  CONFIG["MAKEDIRS"]        = "mkdir -p"

  CONFIG['EXEEXT']            = ''
  CONFIG['LIBEXT']            = 'a'
  CONFIG['OBJEXT']            = 'o'
  CONFIG['ruby_install_name'] = 'maglev-ruby'  # The name of the interpreter executable
  CONFIG['RUBY_INSTALL_NAME'] = CONFIG['ruby_install_name']
  CONFIG['RUBY_SO_NAME']      = CONFIG['ruby_install_name']
  CONFIG['BASERUBY']          = 'ruby'  # MRI ruby used to build maglev-ruby?

  # First set plausible defaults, then override on specific architectures
  # in the case statement immediately below
  CONFIG['ARCH_FLAG']      = ' -m64 '
  CONFIG['CC']             = ENV["CC"] || 'cc '
  CONFIG['CFLAGS']         = ' -fPIC -g '
  CONFIG['COMMON_HEADERS'] = 'ruby.h'
  CONFIG['CPP']            = ENV["CPP"] || 'cc -E '
  CONFIG['CPPFLAGS']       = ' $(cppflags) '
  CONFIG['CXX']            = ENV["CXX"] || 'c++'
  CONFIG['DLEXT']          = 'so'
  CONFIG['LDFLAGS']        = "-L#{CONFIG['gemlibdir']}"
  CONFIG['LDSHARED']       = CONFIG["CC"] + ' -shared '
  CONFIG['OUTFLAG']        = ' -o '
  CONFIG['configure_args'] = ''

  case Config::CONFIG['host_os']
  when /darwin/
    CONFIG['CFLAGS']     = ' -fPIC -DTARGET_RT_MAC_CFM=0 -fno-omit-frame-pointer -fno-strict-aliasing -fexceptions $(cflags) '
    CONFIG['CPPFLAGS']   = ' -D_XOPEN_SOURCE -D_DARWIN_C_SOURCE $(DEFS) $(cppflags) '
    CONFIG['CXXFLAGS']   = CONFIG['CFLAGS'] + ' $(cxxflags) '
    CONFIG['LDSHARED']   = CONFIG["CC"] + ' -dynamic -bundle -undefined dynamic_lookup '
    CONFIG['LDSHAREDXX'] = CONFIG['LDSHARED']
    CONFIG['DLDFLAGS']   = " -bundle "
    CONFIG['ARCH_FLAG']  = ' -arch x86_64 '
    CONFIG['LIBS']       = "-lgcilnk"
    CONFIG['CC']         = ENV["CC"] || 'gcc '

  when /x86_64_solaris/
    CONFIG['CC']         = ENV["CC"] || "/opt/sunstudio12.1/bin/cc"
    CONFIG['LIBS']       = "-lrt -ldl -lm -lc"
    CONFIG['DLDFLAGS']   = " -L."           # -m64 should be picked up by ARCH_FLAG ?
    CONFIG['CFLAGS']     = " -fPIC -g "     # -m64 should be picked up by ARCH_FLAG ?

  when /HP-UX/
    CONFIG['DLEXT']      = 'sl'

  end
  MAKEFILE_CONFIG = {}
  CONFIG.each{|k,v| MAKEFILE_CONFIG[k] = v.dup}

  def Config::expand(val, config = Config::CONFIG)
    (val || "").gsub!(/\$\$|\$\(([^()]+)\)|\$\{([^{}]+)\}/) do |var|
      if !(v = $1 || $2)
        '$'
      elsif key = config[v = v[/\A[^:]+(?=(?::(.*?)=(.*))?\z)/]]
        pat, sub = $1, $2
        config[v] = false
        Config::expand(key, config)
        config[v] = key
        key = key.gsub(/#{Regexp.quote(pat)}(?=\s|\z)/n) {sub} if pat
        key
      else
        " "
      end
    end
    val
  end
  CONFIG.each_value do |val|
    Config::expand(val)
  end

end
RbConfig = Config
CROSS_COMPILING = nil unless defined? CROSS_COMPILING

end  # End Maglev.transient

$LOADED_FEATURES.shift  # take this file off of $LOADED_FEATURES so that reloads will work.
