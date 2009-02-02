# In other rubies this file gets created by mkconfig.rb when ruby is
# built. It's usually used to describe various settings and other
# information used in the build For more informations refer to pickaxe
# p.183
#
# Various files in the MSpec libs require this Module. At this point it's
# hard coded rather than generated to fulfill the mspec requirement.


# TODO: To get a per-install view of this, without having to generate the
# data each time the vm starts up, we can do the real work, once, when
# prims are loaded, then have rbconfig.rb reference that data.  For right
# now, we'll just calculate each time.

module Config
  # The proposed layout is fairly standard, except I'm putting bindir
  # under the main dir:
  #
  #  +-$MAGLEV_HOME/
  #    |
  #    +-1.8/                      rubylibdir
  #    | |
  #    | +-<arch>/                 archdir, topdir
  #    |
  #    +-gems/
  #    |
  #    +-site_ruby/                sitedir
  #    | |
  #    | +-1.8/                    sitelibdir
  #    |   |
  #    |   +-<arch>/               sitearchdir
  #    +-vendor_ruby/              vendordir
  #      |
  #      +-1.8/                    vendorlibdir
  #        |
  #        +-<arch>/               vendorarchdir
  #
  RUBY_VERSION == "1.8.6" or
    raise "ruby lib version (1.8.6) doesn't match executable version (#{RUBY_VERSION})"
  VERSION = '1.8'   # Used in paths below
  ARCH = `uname -m`
  MAGLEV_HOME = ENV['MAGLEV_HOME']

  CONFIG = {}
  CONFIG['EXEEXT']            = ''
  CONFIG['BASERUBY']          = 'ruby'  # MRI ruby used to build maglev-ruby?
  CONFIG['RUBY_INSTALL_NAME'] = 'maglev-ruby'
  CONFIG['ruby_install_name'] = CONFIG['RUBY_INSTALL_NAME']
  CONFIG['RUBY_SO_NAME']      = CONFIG['RUBY_INSTALL_NAME']
  CONFIG['arch']              = ARCH
  CONFIG['ruby_version']      = '1.8.6'

  CONFIG['rubylibdir']        = File.join(MAGLEV_HOME, VERSION)
  CONFIG['archdir']           = File.join(CONFIG['rubylibdir'], ARCH)
  CONFIG['topdir']            = CONFIG['archdir']

  CONFIG['bindir']            = File.join(MAGLEV_HOME, 'bin')
  CONFIG['datadir']           = File.join(MAGLEV_HOME, 'share')

  CONFIG['sitedir']           = File.join(MAGLEV_HOME, 'site_ruby')
  CONFIG['sitelibdir']        = File.join(CONFIG['sitedir'], VERSION)
  CONFIG['sitearchdir']       = File.join(CONFIG['sitelibdir'], ARCH)

  CONFIG['vendordir']         = File.join(MAGLEV_HOME, 'vendor_ruby')
  CONFIG['vendorlibdir']      = File.join(CONFIG['vendordir'], VERSION)
  CONFIG['vendorarchdir']     = File.join(CONFIG['vendorlibdir'], ARCH)

  CONFIG['libdir']            = File.join(MAGLEV_HOME, 'gemstone/lib')

  # TODO: this is usually something like /usr/share need to decide where to
  # put this
  CONFIG['datadir']           = File.join(MAGLEV_HOME, 'data')

  CONFIG['host_os'] = %w( not_used sparc_solaris linux-gnu PowerPC_AIX
                          darwin9.0 x86_64_Solaris Itanium_HP-UX
                        )[Exception._cpuOsKind - 1]

end

RbConfig = Config
