
class SystemExit
  attr_reader :status
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
