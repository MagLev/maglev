# Tests for methods in Kernel.rb
require File.expand_path('simple', File.dirname(__FILE__))

class Mock
  def to_hash ; { }  end
  def to_ary  ; [ ]  end
  def to_a    ; [ ]  end
  def to_str  ; 'from to_str' end
  def to_s    ; 'from to_s'   end
  def to_int  ;  3   end  # Pi, in Kansas
  def to_i    ;  2   end
  def to_f    ;  2.7 end  # Bigger than E, in Kansas
  def to_sym  ;  :a_sym end
end
#h = Type.coerce(Hash.new, Hash, :to_hash)

m = Mock.new

test(Maglev::Type.coerce_to(m, Hash,    :to_hash).class, Hash,   "Hash")

test(Maglev::Type.coerce_to(m, Array,   :to_ary).class,  Array,  "Array#to_ary")
test(Maglev::Type.coerce_to(m, Array,   :to_a).class,    Array,  "Array#to_a")

test(Maglev::Type.coerce_to(m, String,  :to_str).class,  String, "String#to_str")
test(Maglev::Type.coerce_to(m, String,  :to_s).class,    String, "String#to_s")

test(Maglev::Type.coerce_to(m, Fixnum,  :to_int).class,  Fixnum, "Fixnum#to_int")

test(Maglev::Type.coerce_to(m, Integer, :to_i).class,    Fixnum, "Integer#to_i")
test(Maglev::Type.coerce_to(m, Integer, :to_int).class,  Fixnum, "Integer#to_int")

test(Maglev::Type.coerce_to(m, Symbol,  :to_sym).class,  Symbol, "Symbol#to_sym")

report
true
