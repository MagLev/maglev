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
    def cma 	# cma is instance meth   # compileIn: GsProcess currentMethDefTarget
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

cx = C
ox = C.new
C.ma
ox.cma
C.cmaa

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

cnn = Class.new do 
  $selfcnn = self
  def cdi	# cdi, cdii are instance methods of cnn
    puts "In cdi"
    def cdii
      puts "In cdii"
    end
  end
end

onn = cnn.new
onn.cdi
onn.cdii

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

D.new.dcls_new

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

E.new.maa
E.new.mbb

class F
  def self.defmf
    eval( "def mf ; $fax = 9 ; end ")  # makes an instance method
  end
end
F.defmf
F.new.mf
unless $fax == 9 ; raise 'fail'; end

module M
   def self.extend_cmd
     eval( "def mma ; $max = 9 ; end ") # expect a module method
   end 
end 
M.extend_cmd
class CM
  include M
 end 
CM.new.mma ; unless $max == 9 ; raise 'fail'; end

class H
  def initialize
    eval("def irb_xx; puts 'In irb_xx'; self ; end ", TOPLEVEL_BINDING)
  end
end
H.new # to run initialize
isx = irb_xx()
unless isx.equal?(self) ; raise 'fail'; end

class HB
  def initialize
    eval("def hb_xx; puts 'In hb_xx'; self ; end ")
    hb_xx
  end
end
hb = HB.new
hb.hb_xx()
begin
  Object.new.hb_xx()
  raise 'should not find hb_xx in Object'
rescue NoMethodError
  # ok
end
 
# Maglev::System.session_temp_put(:Mdbg, true)
class G
  class << self
    def ev_ma
      eval "def ev_mb ; end"  # expect a classmethod
    end
  end
end 
G.ev_ma
G.ev_mb

sma = $selfma
smb = $selfmb
sev = $selfev
snn = $selfcnn
puts "ciy #{$ciy.inspect}"
puts "ccy #{$ccy.inspect}"


true
