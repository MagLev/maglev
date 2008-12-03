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
    r = Gemstone._processInfo(3, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.egid=(arg)
    status = Gemstone._processInfo(5, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.euid
    r = Gemstone._processInfo(1, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.euid=(arg)
    status = Gemstone._processInfo(6, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.gid
    r = Gemstone._processInfo(2, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.gid=(arg)
    status = Gemstone._processInfo(7, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.getpgid(arg)
    r = Gemstone._processInfo(11, arg, nil)
    _procInfoResultCheck(r)
  end

  def self.getpgrp
    r = Gemstone._processInfo(9, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.uid
    r = Gemstone._processInfo(0, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.uid=(arg)
    status = Gemstone._processInfo(4, arg, nil)
    _procInfoResultCheck(status)
    arg
  end

  def self.pid
    r = Gemstone._processInfo(8, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.pppid
    r = Gemstone._processInfo(10, nil, nil)
    _procInfoResultCheck(r)
  end

  def self.kill(signal, aPid)
    # aPid < 0 or  aPid == Process.pid() are not supported
    #    and will raise an error
    status = Gemstone._processInfo(12, signal, aPid)
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
