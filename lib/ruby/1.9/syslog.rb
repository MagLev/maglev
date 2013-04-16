# This file is generated Rubinius::FFI::FileProcessor from lib/syslog.rb.ffi.

#  Created by Ari Brown on 2008-02-23.
#  For rubinius. All pwnage reserved.
#

# ** Syslog(Module)

# Included Modules: Syslog::Constants

# require 'syslog'

# A Simple wrapper for the UNIX syslog system calls that might be handy
# if you're writing a server in Ruby.  For the details of the syslog(8)
# architecture and constants, see the syslog(3) manual page of your
# platform.

module Syslog
  module Constants
    LOG_ALERT = 1
    LOG_AUTH = 32
    LOG_AUTHPRIV = 80
    LOG_CONS = 2
    # LOG_CONSOLE not defined
    LOG_CRIT = 2
    LOG_CRON = 72
    LOG_DAEMON = 24
    LOG_DEBUG = 7
    LOG_EMERG = 0
    LOG_ERR = 3
    LOG_FTP = 88
    LOG_INFO = 6
    LOG_KERN = 0
    LOG_LOCAL0 = 128
    LOG_LOCAL1 = 136
    LOG_LOCAL2 = 144
    LOG_LOCAL3 = 152
    LOG_LOCAL4 = 160
    LOG_LOCAL5 = 168
    LOG_LOCAL6 = 176
    LOG_LOCAL7 = 184
    LOG_LPR = 48
    LOG_MAIL = 16
    LOG_NDELAY = 8
    LOG_NEWS = 56
    LOG_NOTICE = 5
    LOG_NOWAIT = 16
    # LOG_NTP not defined
    LOG_ODELAY = 4
    LOG_PERROR = 32
    LOG_PID = 1
    # LOG_SECURITY not defined
    LOG_SYSLOG = 40
    LOG_USER = 8
    LOG_UUCP = 64
    LOG_WARNING = 4







    ##
    #   LOG_MASK(pri)
    #
    # HACK copied from macro
    # Creates a mask for one priority.
    def self.LOG_MASK(pri)
      1 << pri
    end

    ##
    #   LOG_UPTO(pri)
    # HACK copied from macro
    # Creates a mask for all priorities up to pri.
    def self.LOG_UPTO(pri)
      (1 << ((pri)+1)) - 1
    end
  end

  include Constants

  module Foreign
    extend FFI::Library

    # methods
    attach_function :open,     "openlog",    [:pointer, :int, :int], :void
    attach_function :close,    "closelog",   [], :void
    attach_function :write,    "syslog",     [:int, :string, :string], :void
    attach_function :set_mask, "setlogmask", [:int], :int
  end

  # Not open by default.
  #
  # Yes, a normal ivar, on Syslog, the module.
  @open = false

  ##
  # returns the ident of the last open call
  def self.ident
    @open ? @ident : nil
  end

  ##
  # returns the options of the last open call
  def self.options
    @open ? @options : nil
  end

  ##
  # returns the facility of the last open call
  def self.facility
    @open ? @facility : nil
  end

  ##
  # mask
  #   mask=(mask)
  #
  # Returns or sets the log priority mask.  The value of the mask
  # is persistent and will not be reset by Syslog::open or
  # Syslog::close.
  #
  # Example:
  #   Syslog.mask = Syslog::LOG_UPTO(Syslog::LOG_ERR)
  def self.mask=(mask)
    unless @open
      raise RuntimeError, "must open syslog before setting log mask"
    end

    @mask_before_reopen = nil

    @mask = Maglev::Type.coerce_to(mask, Fixnum, :to_int)

    Foreign.set_mask(@mask)
  end

  def self.mask
    @open ? @mask : nil
  end

  ##
  #   open(ident = $0, logopt = Syslog::LOG_PID | Syslog::LOG_CONS, facility = Syslog::LOG_USER) [{ |syslog| ... }]
  #
  # Opens syslog with the given options and returns the module
  # itself.  If a block is given, calls it with an argument of
  # itself.  If syslog is already opened, raises RuntimeError.
  #
  # Examples:
  #   Syslog.open('ftpd', Syslog::LOG_PID | Syslog::LOG_NDELAY, Syslog::LOG_FTP)
  #   open!(ident = $0, logopt = Syslog::LOG_PID | Syslog::LOG_CONS, facility = Syslog::LOG_USER)
  #   reopen(ident = $0, logopt = Syslog::LOG_PID | Syslog::LOG_CONS, facility = Syslog::LOG_USER)
  def self.open(ident=nil, opt=nil, fac=nil)
    raise "Syslog already open" unless not @open

    ident ||= $0
    opt ||= Constants::LOG_PID | Constants::LOG_CONS
    fac ||= Constants::LOG_USER

    @ident = ident
    @options = opt
    @facility = fac

    # syslog rereads the string everytime syslog() is called, so we have to use
    # an FFI pointer to keep the memory the string is in alive
    @ident_pointer = FFI::MemoryPointer.new(@ident.size + 1)
    @ident_pointer.write_string(@ident)

    Foreign.open(@ident_pointer, opt, fac)

    @open = true

    # Calling set_mask twice is the standard way to set the 'default' mask
    self.mask = @mask_before_reopen || Foreign.set_mask(0)

    if block_given?
      begin
        yield self
      ensure
        close
      end
    end

    self
  end

  def self.reopen(*args, &block)
    @mask_before_reopen = mask
    close
    open(*args, &block)
  end

  class << self
    alias_method :open!, :reopen
  end

  ##
  # Is it open?
  def self.opened?
    @open
  end

  ##
  # Close the log
  # close will raise an error if it is already closed
  def self.close
    raise "Syslog not opened" unless @open

    Foreign.close
    @ident = nil
    @options = @facility = @mask = nil;
    @open = false
  end

  ##
  #   log(Syslog::LOG_CRIT, "The %s is falling!", "sky")
  #
  # Doesn't take any platform specific printf statements
  #   logs things to $stderr
  #   log(Syslog::LOG_CRIT, "Welcome, %s, to my %s!", "leethaxxor", "lavratory")
  def self.log(pri, format, *args)
    raise "Syslog must be opened before write" unless @open

    pri = Maglev::Type.coerce_to(pri, Fixnum, :to_i)

    message = StringValue(format) % args
    Foreign.write(pri, "%s", message)
  end

  ##
  # handy little shortcut for LOG_EMERG as the priority
  def self.emerg(*args);
    log(LOG_EMERG, *args)
  end

  ##
  # handy little shortcut for LOG_ALERT as the priority
  def self.alert(*args)
    log(LOG_ALERT, *args)
  end

  ##
  # handy little shortcut for LOG_ERR as the priority
  def self.err(*args)
    log(LOG_ERR, *args)
  end

  ##
  # handy little shortcut for LOG_CRIT as the priority
  def self.crit(*args)
    log(LOG_CRIT, *args)
  end

  ##
  # handy little shortcut for LOG_WARNING as the priority
  def self.warning(*args)
    log(LOG_WARNING, *args)
  end

  ##
  # handy little shortcut for LOG_NOTICE as the priority
  def self.notice(*args)
    log(LOG_NOTICE, *args)
  end

  ##
  # handy little shortcut for LOG_INFO as the priority
  def self.info(*args)
    log(LOG_INFO, *args)
  end

  ##
  # handy little shortcut for LOG_DEBUG as the priority
  def self.debug(*args)
    log(LOG_DEBUG, *args)
  end

  def self.LOG_MASK(pri)
    Constants.LOG_MASK(pri)
  end

  ##
  #   LOG_UPTO(pri)
  # HACK copied from macro
  # Creates a mask for all priorities up to pri.
  def self.LOG_UPTO(pri)
    Constants.LOG_UPTO(pri)
  end

  def self.inspect
    if @open
      "#<%s: opened=true, ident=\"%s\", options=%d, facility=%d, mask=%d>" %
      [self.name, @ident, @options, @facility, @mask]
    else
      "#<#{self.name}: opened=false>"
    end
  end

  ##
  #   Syslog.instance # => Syslog
  # Returns the Syslog module
  def self.instance
    self
  end
end

