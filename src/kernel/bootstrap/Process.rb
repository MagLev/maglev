module Process

  Status = __resolve_smalltalk_global( :RubyProcessStatus ) 
  # implementation of Process::Status  in delta/Process.rb

  def self._procInfoResultCheck(status)
    if (status < 0)
      errnoValue = - status
      Errno.handle( errnoValue )
    else
      return status # the result value
    end
  end

  def self.egid
    r = Maglev.__system.__process_info(3, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.egid=(arg)
    status = Maglev.__system.__process_info(5, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.euid
    r = Maglev.__system.__process_info(1, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.euid=(arg)
    status = Maglev.__system.__process_info(6, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.gid
    r = Maglev.__system.__process_info(2, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.gid=(arg)
    status = Maglev.__system.__process_info(7, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.getpgid(arg)
    arg = Maglev::Type.coerce_to(arg, Fixnum, :to_int)
    r = Maglev.__system.__process_info(11, arg, nil)
    _procInfoResultCheck(r)
  end

  def self.getpgrp
    r = Maglev.__system.__process_info(9, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.uid
    r = Maglev.__system.__process_info(0, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.uid=(arg)
    status = Maglev.__system.__process_info(4, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.pid
    r = Maglev.__system.__process_info(8, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.pppid
    r = Maglev.__system.__process_info(10, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.__kill(signal, aPid)
    # aPid < 0 or  aPid == Process.pid() are not supported
    #    and will raise an error
    status = Maglev.__system.__process_info(12, signal, aPid)
    _procInfoResultCheck(status)
    1
  end

  def self.kill(signal, *pids)
    lim = pids.__size
    if lim._equal?(0)
      raise ArgumentError, 'Process.kill, expected at least 2 args'
    end
    unless signal._isFixnum
      signal = Maglev::Type.coerce_to(signal, String, :to_s )
      signal = Signal.__name_to_number(signal)
    end
    count = 0
    while count < lim
      apid = pids[count]
      self.__kill(signal, apid)
      count = count + 1
    end
    return count
  end

  def self.waitpid2(pid, flags=0)
    #  __waitpid2  updates $? as side effect
    r = Kernel.__waitpid2(pid, flags)    
    if r._isFixnum
      Errno.handle( r )  
    end
    r
  end

  def self.waitpid(pid, flags=0)
    arr = waitpid2(pid, flags)
    arr[0]
  end

  def self.wait2(pid=-1)
    waitpid2(pid, 0)
  end

  def self.wait(pid=-1)
    waitpid(pid, 0)
  end

  def self.fork
    raise NotImplementedError, "fork not available on MagLev"
  end
end
Process.__freeze_constants
