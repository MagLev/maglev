# Distilled from Thor
#
# MagLev gives following error:
#
#   error , NoMethodError: protected method `method_added' for ,
#                during /Users/pmclain/tmp/pbm.rb
#   ERROR 2010 , NoMethodError: protected method `method_added' for  (NoMethodError)

class C
  class << self
    protected
    def method_added(meth)
      puts "Adding method"
    end
  end

  def help()
    puts "Help"
  end
end
