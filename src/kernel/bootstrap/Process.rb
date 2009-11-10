module Process

  def self._procInfoResultCheck(status)
    if (status < 0)
      errnoValue = - status
      raise SystemCallError # TODO: Errno::xxx
    else
      return status # the result value
    end
  end

  def self.egid
    r = Maglev::System.__process_info(3, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.egid=(arg)
    status = Maglev::System.__process_info(5, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.euid
    r = Maglev::System.__process_info(1, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.euid=(arg)
    status = Maglev::System.__process_info(6, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.gid
    r = Maglev::System.__process_info(2, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.gid=(arg)
    status = Maglev::System.__process_info(7, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.getpgid(arg)
    r = Maglev::System.__process_info(11, arg, nil)
    _procInfoResultCheck(r)
  end

  def self.getpgrp
    r = Maglev::System.__process_info(9, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.uid
    r = Maglev::System.__process_info(0, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.uid=(arg)
    status = Maglev::System.__process_info(4, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.pid
    r = Maglev::System.__process_info(8, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.pppid
    r = Maglev::System.__process_info(10, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.kill(signal, aPid)
    # aPid < 0 or  aPid == Process.pid() are not supported
    #    and will raise an error
    status = Maglev::System.__process_info(12, signal, aPid)
    _procInfoResultCheck(status)
    1
  end

  def self.kill(signal, *pids)
    count = 0
    pids.each { |aPid|
      self.kill(signal, aPid)
      count = count + 1
    }
    return count
  end

end
