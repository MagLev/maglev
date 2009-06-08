
class SystemExit
  def status
    @status  # a dynamic instVar
  end

  def initialize(*args)
    status = if args.first._isFixnum
               args.shift
             else
               0
             end
    super(*args)
    @status = status
  end
end
