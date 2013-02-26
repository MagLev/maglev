module Process
  def self.times
    arr = Maglev.__system.__host_times
    Struct::Tms.new(*arr)
  end

  module GID
    # MNI: Process::GID.change_privilege

    def self.eid
      Process.egid
    end

    # MNI: Process::GID.eid=
    # MNI: Process::GID.grant_privilege
    # MNI: Process::GID.re_exchange
    # MNI: Process::GID.re_exchangeable?

    def self.rid
      Process.rgid
    end

    # MNI: Process::GID.sid_available?
    # MNI: Process::GID.switch
  end

  module UID
    # MNI: Process::UID.change_privilege

    def self.eid
      Process.euid
    end

    # MNI: Process::UID.eid=
    # MNI: Process::UID.grant_privilege
    # MNI: Process::UID.re_exchange
    # MNI: Process::UID.re_exchangeable?

    def self.rid
      Process.rgid
    end

    # MNI: Process::UID.sid_available?
    # MNI: Process::UID.switch
  end

  class Status  # [
    # Process::Status is identical to Smalltalk  RubyProcessStatus,
    #    see bootstrap/Process.rb
    #
    # In the current implementation, an instance is only available
    # after a child process has finished execution.

    def __status
      @_st_stat
    end

    def __prim_result
      # returns the Array that was returned by System(C)>>_performOnServer:
      # which is [ rawStatus , childStatus, resutlStr, errMsg , errno]
      @_st_primStatus
    end

    # Returns true if the integer value of +receiver+ equals +other+.
    def ==(other)
      if other._is_a?( Status )
        @_st_stat == other.__status
      else
        @_st_stat == other
      end
    end

    # Returns the logical AND of the bits in stat with +fixnum+.
    def &(fixnum)
      arg = Maglev::Type.coerce_to(fixnum, Fixnum, :to_int)
      @_st_stat & arg
    end

    # Shift the bits in receiver right +fixnum+ places.
    def >>(fixnum)
      arg = Maglev::Type.coerce_to(fixnum, Fixnum, :to_int)
      @_st_stat >> arg
    end

    # coredump?  # MNI

    # Returns true if stat exited normally.
    def exited?
      # instances of Process::Status not available while a child is running
      true
    end
    alias stopped? exited?

    def exitstatus
      # equivalent to posix WEXITSTATUS macro
      (@_st_stat >> 8) & 0xFF
    end

    # pid    #  MNI
    # signaled?  # MNI

    def success?
      @_st_stat == 0
    end

    # stopped? # MNI
    # stopsig  # MNI
    # termsig  # MNI

    def to_i
      @_st_stat
    end

    def to_int
      @_st_stat
    end

    def to_s
      @_st_stat.to_s
    end
  end  # ]

  module Sys
    def self.getegid
      Process.egid
    end
    def self.geteuid
      Process.euid
    end
    def self.getgid
      Process.gid
    end
    def self.getuid
      Process.uid
    end

    def issetugid
      throw NotImplementedError, 'Process::Sys.issetugid'
    end

    def self.setegid(gid)
      Process.egid=(gid)
    end
    def self.seteuid(euid)
      Process.euid=(euid)
    end
    def self.setgid(gid)
      Process.gid=(gid)
    end

    def self.setregid(rgid, egid)
      throw NotImplementedError, 'Process::Sys.setregid'
    end

    def self.setresgid(rgid, egid)
      throw NotImplementedError, 'Process::Sys.setresgid'
    end

    def self.setresuid(rgid, egid)
      throw NotImplementedError, 'Process::Sys.setresuid'
    end

    def self.setreuid(rgid, egid)
      throw NotImplementedError, 'Process::Sys.setreuid'
    end

    def self.setrgid(rgid)
      Process.rgid=rgid
    end

    def self.setruid(ruid)
      Process.rid=ruid
    end

    def self.setuid(uid)
      Process.uid=uid
    end
  end
end
Process.__freeze_constants
