module Process
  def self.times
    arr = Gemstone._host_times
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
