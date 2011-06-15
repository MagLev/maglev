#  within a class C
#   def self.x       uses C.theMetaClass
#     inner  def         uses  outer.inClass.theNonMetaClass  # outer C
#     innner def self.x  uses outer.inClass                # outer C.class
# 
#   def  uses    C
#     inner def           uses outer.inClass.theNonMetaClass   # outer C
#     innner def self.x   uses outer.inClass            # outer C
#
#  within a block 
#   def        uses self
#       innner def   uses  outer.inClass
#
#  within an instance eval
#   def uses   self.class
#
#  within a class eval
#    def user    self

unless defined?(Maglev)
  class NilClass
    def pause
    end
  end
end

class C
  def self.ma  # ma is classmethod
    puts "In ma"
    $selfma = self
    def cma 	# cma is instance meth
      puts "In cma"
    end
    def self.cmaa   # cmaa is class method
      puts "In cmaa"
    end
  end
 

  def mb	# mb, cmb are instance methods
    $selfmb = self
    puts "In mb"
    def cmb
      puts "In cmb"
    end
    def self.ccmb   # ccmb is an INSTANCE method
      puts "In ccmb"
    end
  end

  class << self
    def sa	# sa, sb are class methods
      puts "In sa"
      def sb
        puts "In sb"
      end
    end
    sx = self
  end
end


C.instance_eval do
  $selfev = self
  def cmi	# cmi, cmii are classmethods of C
    puts "In cmi"
    def cmii
      puts "In cmii"
    end
  end
end

C.instance_eval(" $ciy = self ; def cmie ; puts 'In cmie' ; end ")  # cmie is classmeth

C.class_eval( " $ccy = self; def ccie ; puts 'In ccie' ; end ")  # ccie is instance meth

cnn = Class.new do 
  $selfcnn = self
  def cdi	# cdi, cdii are instance methods of cnn
    puts "In cdi"
    def cdii
      puts "In cdii"
    end
  end
end

class D
  def dcls_new
    ck = Class.new do
       def food
         yield
       end
    end
    ak = ck.new
    bres = ak.food { puts "In dcls block"; break 42 }
    unless bres == 42 ; raise 'fail'; end
    puts "dcls_new ok"
  end
end

class E
  def maa
    puts "In maa"
    [1].each do
      def mbb
        puts "In mbb"
      end
    end
  end
end

cx = C
ox = C.new
C.ma
ox.cma
C.cmaa

ox.mb
ox.cmb
ox.ccmb  # ccmb is on a singleton
# C.new.ccmb  # would get MNU

C.sa
C.sb
C.cmi
C.cmii
C.cmie
C.new.ccie
onn = cnn.new
onn.cdi
onn.cdii
sma = $selfma
smb = $selfmb
sev = $selfev
snn = $selfcnn
puts "ciy #{$ciy.inspect}"
puts "ccy #{$ccy.inspect}"
D.new.dcls_new
E.new.maa
E.new.mbb

true
