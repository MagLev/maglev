module Kernel
  # so far contains just enough to get benchmarks to run

  def Kernel.format(str, *args)
        args.each{|a| str.sub!(/%(d|s)/, a.to_s)}
        str
  end
end
