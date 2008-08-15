module Kernel
  # Object has an empty module Kernel installed as its superclass
  #   during the slowrubyimage step of server build.
  #  this file will extend Kernel by adding methods to it.

  # following methods are just those needed to get some benchmarks and
  #   specs running .

  def format(str, *args)
        args.each{|a| str.sub!(/%(d|s)/, a.to_s)}
        str
  end

  def open(fName) 
     File.open(fName)
  end

  def open(fName, mode) 
     File.open(fName, mode)
  end
end
