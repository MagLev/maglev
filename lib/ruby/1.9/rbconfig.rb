# In other implementation this file gets created by mkconfig.rb when ruby
# is built.  Since MagLev potentially supports VMs from multiple
# architectures connecting to the same stone, we have to figure out
# architecture specific information at runtime.
#
# Uses transient_const_set so that the architecture specific information
# is recomputed the first time each constant is accessed during the lifetime
# of a VM, even if this file is persistently loaded.

module Config
  RUBY_VERSION == "1.9.3" or
    raise "ruby lib version (1.9.3) doesn't match executable version (#{RUBY_VERSION})"
  VERSION = '1.9'
  transient_const_set( :ARCH ) { `uname -m`.chomp }

  # Note: MAGLEV_HOME and TOPDIR should end up being the same
  # in a default installation
  transient_const_set( :MAGLEV_HOME ) { ENV['MAGLEV_HOME'] }

  # TODO: MRI puts rbconfig in the architecture specific subdir of
  # .../lib/ruby/1.8/<archdir>/rbconfig.rb

  # TOPDIR = File.dirname(__FILE__).chomp!("/lib/ruby/1.8")
  transient_const_set( :TOP_DIR ) { ENV['MAGLEV_HOME'] }

  # DESTDIR = '' unless defined? DESTDIR
  DESTDIR = ''

  def Config::__expand(val, cfg_arg)
      (val || "").gsub!(/\$\$|\$\(([^()]+)\)|\$\{([^{}]+)\}/) do |var|
  if !(v = $1 || $2)
    '$'
  elsif key = cfg_arg[v = v[/\A[^:]+(?=(?::(.*?)=(.*))?\z)/]]
    pat, sub = $1, $2
    cfg_arg[v] = false
    Config::__expand(key, cfg_arg)
    cfg_arg[v] = key
    key = key.gsub(/#{Regexp.quote(pat)}(?=\s|\z)/n) {sub} if pat
    key
  else
    " "
  end
      end
      val
  end

  transient_const_set( :CONFIG ) {
    env = ENV
    maglev_home = env['MAGLEV_HOME']
    config = {}
    config['prefix']            = maglev_home
    config['exec_prefix']       = maglev_home
    config['bindir']            = File.join(maglev_home, 'bin')
    config['sysconfdir']        = File.join(maglev_home, 'etc')
    config['includedir']        = File.join(maglev_home, 'lib/ruby/1.9/include')
    config['libdir']            = File.join(maglev_home, 'lib')
    config['gemlibdir']         = File.join(maglev_home, 'gemstone', 'lib')
    version = VERSION
    arch = ARCH

    config['rubylibdir']        = File.join(config['libdir'], 'ruby', version)
    config['archdir']           = File.join(config['rubylibdir'], arch)
    config['topdir']            = config['archdir']

    config['sitedir']           = File.join(config['libdir'], 'ruby', 'site_ruby')
    config['sitelibdir']        = File.join(config['sitedir'], version)
    config['sitearchdir']       = File.join(config['sitelibdir'], arch)

    config['DESTDIR']           = DESTDIR

    # config['sbindir']         = File.join(maglev_home, 'sbin')
    # config['libexecdir']      = File.join(maglev_home, 'libexec')
    config['datadir']           = File.join(maglev_home, 'share')
    config['datarootdir']       = File.join(maglev_home, 'share')
    config['docdir']            = File.join(config['datarootdir'], 'doc')
    config['infodir']           = File.join(config['datarootdir'], 'info')
    config['htmldir']           = config['docdir']

    config['vendordir']         = File.join(config['libdir'], 'vendor_ruby')
    config['vendorlibdir']      = File.join(config['vendordir'], version)
    config['vendorarchdir']     = File.join(config['vendorlibdir'], arch)

    # TODO SHELL
    config['arch']              = arch
    config['ruby_version']      = '1.9'
    cpu_os = Exception.__cpu_os_kind
    host_os = Object.__platform_str
    config['host_os'] = host_os
    config['target_os']       = config['host_os']
    config["LN_S"]            = "ln -s"
    config["SET_MAKE"]        = ""
    config["INSTALL"]         = "install -vp"
    config["INSTALL_PROGRAM"] = "$(INSTALL)"
    config["INSTALL_SCRIPT"]  = "$(INSTALL)"
    config["INSTALL_DATA"]    = "$(INSTALL) -m 644"
    config["RM"]              = "rm -f"
    config["CP"]              = "cp"
    config["MAKEDIRS"]        = "mkdir -p"
    config['PATH_SEPARATOR']  = ':'

    config['EXEEXT']            = ''
    config['LIBEXT']            = 'a'
    config['OBJEXT']            = 'o'
    config['ruby_install_name'] = 'maglev-ruby'  # The name of the interpreter executable
    config['RUBY_INSTALL_NAME'] = config['ruby_install_name']
    config['RUBY_SO_NAME']      = config['ruby_install_name']
    config['BASERUBY']          = 'ruby'  # MRI ruby used to build maglev-ruby?

    # First set plausible defaults, then override on specific architectures
    # in the case statement immediately below
    config['ARCH_FLAG']      = ' -m64 '
    config['CC']             = env["CC"] || 'cc '
    config['CFLAGS']         = ' -fPIC -g '
    config['COMMON_HEADERS'] = 'ruby.h'
    config['CPP']            = env["CPP"] || 'cc -E '
    config['CPPFLAGS']       = ' $(cppflags) '
    config['CXX']            = env["CXX"] || 'c++'
    config['DLEXT']          = 'so'
    config['LDFLAGS']        = "-L#{config['gemlibdir']}"
    config['LDSHARED']       = config["CC"] + ' -shared '
    config['OUTFLAG']        = ' -o '
    config['configure_args'] = ''

    case host_os
#   when /linux/        # linux uses defaults above

    when /darwin/
      config['CFLAGS']     = ' -fPIC -DTARGET_RT_MAC_CFM=0 -fno-omit-frame-pointer -fno-strict-aliasing -fexceptions $(cflags) '
      config['CPPFLAGS']   = ' -D_XOPEN_SOURCE -D_DARWIN_C_SOURCE $(DEFS) $(cppflags) '
      config['CXXFLAGS']   = config['CFLAGS'] + ' $(cxxflags) '
      config['LDSHARED']   = config["CC"] + ' -dynamic -bundle -undefined dynamic_lookup '
      config['LDSHAREDXX'] = config['LDSHARED']
      config['DLDFLAGS']   = " -bundle "
      config['ARCH_FLAG']  = ' -arch x86_64 '
      config['CC']         = env["CC"] || 'gcc '
      config['DLEXT']      = 'bundle'

    when /x86_64_Solaris/i     # accept either "solaris" or "Solaris"
      config['CC']         = env["CC"] || "/opt/sunstudio12.1/bin/cc"
      config['LIBS']       = "-lrt -ldl -lm -lc"
      config['DLDFLAGS']   = " -L."           # -m64 should be picked up by ARCH_FLAG ?
      config['CFLAGS']     = " -fPIC -g "     # -m64 should be picked up by ARCH_FLAG ?
      config["INSTALL"]         = "/usr/ucb/install"   # Solaris default install is too wonky"
      config["INSTALL_PROGRAM"] = "$(INSTALL)"
      config["INSTALL_SCRIPT"]  = "$(INSTALL)"
      config["INSTALL_DATA"]    = "$(INSTALL) -m 644"

#   when /HP-UX/    # not supported on Maglev yet
#     config['DLEXT']      = 'sl'

    end
    config.each_value do |val|
      Config::__expand(val, config)
    end
    config
  }

  transient_const_set( :MAKEFILE_CONFIG ) {
    makefile_config = {}
    CONFIG.each{|k,v| makefile_config[k] = v.dup}
    makefile_config
  }

  def Config::expand(val, cfg_arg = Config::CONFIG)
    Config::__expand(val, cfg_arg)
  end
end
RbConfig = Config
CROSS_COMPILING = nil unless defined? CROSS_COMPILING  # transiently defined

