class IO

  module WaitReadable; end
  module WaitWritable; end

  FNM_CASEFOLD = File::FNM_CASEFOLD
  FNM_DOTMATCH = File::FNM_DOTMATCH
  FNM_NOESCAPE = File::FNM_NOESCAPE
  FNM_PATHNAME = File::FNM_PATHNAME
  FNM_SYSCASE = File::FNM_SYSCASE

  # For flock(2)
  LOCK_SH  = File::LOCK_SH         # shared file lock
  LOCK_EX  = File::LOCK_EX         # exclusive file lock
  LOCK_NB  = File::LOCK_NB         # non block when locking
  LOCK_UN  = File::LOCK_UN         # unlock

  # Flags for open(2), fcntl(2)
  APPEND = File::APPEND	    # append mode
  CREAT  = File::CREAT      # create if not there
  EXCL   = File::EXCL       # open with exclusive lock
  NOCTTY   = File::NOCTTY   # open with no controlling tty
  NONBLOCK = File::NONBLOCK # don't block
  SYNC     = File::SYNC     # sync
  TRUNC    = File::TRUNC    # truncate at open

  RDONLY  = File::RDONLY    # open readonly
  WRONLY  = File::WRONLY    # open write only
  RDWR    = File::RDWR      # open read and write

  SEEK_SET = File::SEEK_SET # set file position to offset
  SEEK_CUR = File::SEEK_CUR # set file position to current + offset
  SEEK_END = File::SEEK_END # set file position to end of file + offset

end
IO.__freeze_constants
class File
  module Constants
    APPEND = File::APPEND
    CREAT = File::CREAT
    EXCL  = File::EXCL
    FNM_CASEFOLD = File::FNM_CASEFOLD
    FNM_DOTMATCH = File::FNM_DOTMATCH
    FNM_NOESCAPE = File::FNM_NOESCAPE
    FNM_PATHNAME = File::FNM_PATHNAME
    FNM_SYSCASE = File::FNM_SYSCASE
    LOCK_EX = File::LOCK_EX
    LOCK_NB = File::LOCK_NB
    LOCK_SH = File::LOCK_SH
    LOCK_UN = File::LOCK_UN
    NOCTTY = File::NOCTTY
    NONBLOCK = File::NONBLOCK
    RDONLY = File::RDONLY
    RDWR = File::RDWR
    SYNC = File::SYNC
    TRUNC = File::TRUNC
    WRONLY = File::WRONLY
  end
  Constants.__freeze_constants
  include Constants
end

class Socket
  module Constants
    # constants filled in by Socket>>_initTransientSocketConstants , 
    #   called from RubyContext>>initTransient during VM initialization .
  end
  include Constants
end

class IO
  include File::Constants
end
