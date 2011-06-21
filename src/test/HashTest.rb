# Tests the following:
#    Instantiation
#    Equals
#    Clear
#    Copy
#    Default
#    Delete
#    DeleteIf
#    Fetch
#    HasKey
#    Include
#    Member
#    HasValue
#    Index
#    Replce
#    Inspect
#    Invert
#    Length
#    Size
#    Merge
#    Update
#    Sort
#    ToHash
#    ToString
require File.dirname(__FILE__) + '/simple.rb'

# Class definition for hash tests
Hash.new.any? { |x| true }  # ensure Enumerable was included

class HashTest
    # Expected value: true
    def new1
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash2 = hash1.each_pair {|key, value|}
        test(hash1 == hash2 , true, "new1")
    end

    # Expected value: true
    def new2
        hash1 = Hash[1=>100, 2=>200, 3=>300]
        hash2 = hash1.each_pair {|key, value|}
        test(hash1 == hash2 , true, "new2")
    end

    # Expected value: true
    def new3
        hash1 = Hash.new
        hash1[1] = 100
        hash1[2] = 200
        hash1[3] = 300
        hash2 = hash1.each_pair {|key, value|}
        test(hash1 == hash2 , true, "new3")
    end

    # Expected value: true
    def new4
        hash1 = Hash.new
        hash1 = {1=>100, 2=>200, 3=>300}
        hash2 = hash1.each_pair {|key, value|}
        test(hash1 == hash2 , true, "new4")
    end

    def new5
      # Apr 1,09  from Michael Latta
      exp = { 8=>9, 20=>21 }

      h = Hash[ *[8,9,20,21] ]
      unless h == exp ; raise 'error'; end

      h = Hash[8,9,20,21]
      unless h == exp ; raise 'error'; end
      true
    end

    # Expected value: true
    def equals
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash2 = Hash.new
        hash2[1] = 100
        hash2[2] = 200
        hash2[3] = 300
        test(hash1 == hash2 , true, "equals")
    end

    # Expected value: true
    def copy
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash2 = hash1
        test(hash1 == hash2 , true, "copy")
    end

    # Expected value: true
    def clear
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.clear()
        test(hash.empty?() , true, "clear")
    end

    # Expected value: nil
    def default1
        hash = Hash[1, 100, 2, 200, 3, 300]
        d = hash.default(key=nil)
        test(d.equal?(nil), true, "default1")
    end

    # Expected value: 200
    def default2
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.default = 200
        d = hash.default
        test(d, 200, "default2")
    end

    # Expected value: true
    def delete
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash1.delete(2)
        hash2 = hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        test( hash1 == hash2 , true, "delete")
    end

    # Expected value: true
    def deleteIf
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash1.delete_if {|key, value| key == 2}
        hash2 = hash1.each_pair {|key, value|}
        test( hash1 == hash2, true, "deleteIf")
    end

    # Expected value: '300'
    def fetch
        hash = Hash[1, 100, 2, 200, 3, 300]
        x = hash.fetch(3)
        test(x, 300, "fetch");
    end

    # Expected value: true if key = a..c
    def hasKey(key, exp)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        test( hash.has_key?(key), exp, 'hasKey')
    end

    # Expected value: true if key = a..c
    def include(key, exp)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        test( hash.include?(key) , exp, 'include')
    end

    # Expected value: true if key = a..c
    def member(key, exp)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        test( hash.member?(key) , exp, 'member')
    end

    # Expected value: true if key = a..c
    def key(key, exp)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        test( hash.key?(key) , exp, 'key')
    end

    # Expected value: true if value = "abc", "def", or "ghi"
    def hasValue(value, exp)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        test( hash.has_value?(value), exp, 'hasValue')
    end

    # Expected value: "a" if value = "abc", "b" if value "def",
    #                 or "c" is value = "ghi", otherwise nil
    def index(value, exp)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        @hh = hash
        test( hash.index(value) , exp, 'index')
    end

    # Expected value: true
    def replace
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        hash1.replace(hash2)
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        test( hash1 == hash2, true, 'replace')
    end

    # Expected value: false
    def invert
        hash1 = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        hash2 = hash1.invert()
        test( hash1 == hash2, false, 'invert')
    end

    # Expected value: 4
    def length
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst', 'd', 'deq']
        x = hash.length()
        test(x , 4, 'length')
    end

    # Expected value: 5
    def size
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst', 'd', 'deq', 'e', 'fbi']
        x = hash.size()
        test(x, 5, 'size')
    end

    def merge1
        hash1 = Hash['a', 'abc', 'b', 'def']
        hash2 = Hash['y', 'uvw', 'z', 'xyz']
        hash3 = hash1.merge(hash2)
        ax = hash3 == hash1
        bx = hash3 == hash2
        test(ax || bx, false, 'merge1')
    end

    def merge2
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash3 = hash1.merge(hash2)
        ax = hash3 == hash1
        bx = hash3 == hash2
        test(ax || bx, false, 'merge2')
    end

    def merge!
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash3 = hash1.merge!(hash2)
        test( hash3 == hash2, false, 'merge!')
    end

    def merge3
      h1 = Hash.new
      h2 = Hash.new
      keys1_2 = []
      vals_3 = []
      (0..9).each {|i| 
         v = i.to_s
         h1[i] = v 
         keys1_2 << i
         vals_3 << v
      }
      (10..19).each { |i|
        h1[i] = i.to_s
        v = (2*i).to_s
        h2[i] = v
        keys1_2 << i
        vals_3 << v
      }
      keys_3 = keys1_2.dup 
      (20..29).each {|i| 
         v = i.to_s
         h2[i] = v
         keys_3 << i 
        vals_3 << v
      }
      h3 = h1.merge(h2)
      allvalues = Array.new(30)
      h3.each_pair { |k,v| 
        if k < 0 || k > 29 ; raise 'error'; end
        allvalues[k] = v 
      } 
      (0..29).each { |i|
        unless (ax = allvalues[i]) == (bx = vals_3[i]) ; raise 'error'; end
      }
      allkeys3 = []
      h3.each_key { |k|  allkeys3 << k }
      unless (sorted3 = allkeys3.sort) == keys_3 ; raise 'error'; end

      (20..29).each {|i|
        v = h3.delete(i)
        unless v == i.to_s ; raise 'error'; end
      }
      allkeys3 = []
      h3.each_key { |k|  allkeys3 << k }
      unless (sorted3 = allkeys3.sort) == keys1_2 ; raise 'error'; end

      allvalues = Array.new(20)
      h3.each_pair { |k,v| 
        if k < 0 || k > 19 ; raise 'error'; end
        allvalues[k] = v
      } 
      (0..19).each { |i|
        unless (ax = allvalues[i]) == (bx = vals_3[i]) ; raise 'error'; end
      }
      (0..9).each { |i|
         v = h3.delete( i )
         unless v == i.to_s ; raise 'error'; end
       }
      (10..19).each {|i|
         v = h3.delete( i )
         unless v == (2*i).to_s ; raise 'error'; end
      }
      allkeys3 = []
      h3.each_key { |k|  allkeys3 << k }
      unless allkeys3.size == 0 ; raise 'error'; end
      h3.each_pair { |k,v|
        raise 'error'
      }
       
      true
    end

    def update
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash1.update(hash2)
        test( hash1 == hash2 , false, 'update')
    end

    def sort
        hash1 = Hash['a', 'abc', 'c', 'ghi', 'b', 'def']
        a1 = hash1.to_a
        a2 = hash1.sort()
        test( a1 == a2 , true , 'sort')
    end

    def toHash
        hash1 = Hash['a', 'abc', 'c', 'ghi', 'b', 'def']
        hash2 = hash1.to_hash()
        test( hash1 == hash2 , true, 'toHash')
    end

    def toString
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        ret = hash.to_s
        test(ret, 'aabcbdefcghi', 'toString')
    end

    def testInspect
      h = Hash['a', 'abc', 'b', 'def' ]
      h['c'] = h
      s = h.inspect
      # exp = '{"b"=>"def", "c"=>{...}, "a"=>"abc"}'
      exp = '{"a"=>"abc", "b"=>"def", "c"=>{...}}' # with new impl
      unless s == exp ; raise 'error'; end
      true
    end
end

# Call test methods
HashTest.new.new1()
HashTest.new.new2()
HashTest.new.new3()
HashTest.new.new4()
HashTest.new.new5()
HashTest.new.equals()
HashTest.new.copy()
HashTest.new.clear()
HashTest.new.default1()
HashTest.new.default2()
HashTest.new.delete()
HashTest.new.deleteIf()
HashTest.new.fetch()
HashTest.new.hasKey('a', true)
HashTest.new.hasKey('d',false)
HashTest.new.include('a',true)
HashTest.new.include('d',false)
HashTest.new.member('a',true)
HashTest.new.member('d',false)
HashTest.new.key('a',true)
HashTest.new.key('d',false)
HashTest.new.hasValue('abc',true)
HashTest.new.hasValue('xyz',false)
HashTest.new.index('def','b')

HashTest.new.index( 'xyz', nil )

HashTest.new.replace()
HashTest.new.invert()
HashTest.new.length()
HashTest.new.size()
HashTest.new.merge1()
HashTest.new.merge2()
HashTest.new.merge!()
test( HashTest.new.merge3(), true, "merge3")
HashTest.new.update()
# HashTest.new.sort()   # TODO reenable ; do not checkin
HashTest.new.toHash()
# HashTest.new.testInspect() # TODO reenable ; do not checkin

# expectvalue 'aabcbdefcghi'
# TODO is result always sorted ??? irb seems to be
# ret = HashTest.new.toString()
# raise "ERROR" unless ret == 'aabcbdefcghi'

Hash.new.any? { |x| true }

test({1 => "x"}.key?(1.0), false, "eql? test A")
test({1 => "x"}.key?(1),   true,  "eql? test B")
test({:a => 1, :b => 2, :c => 3, 4 => 0 }.key?(4.0), false, "eql? test C")
test({:a => 1, :b => 2, :c => 3, 4 => 0 }.key?(4 ),  true,  "eql? test D")

#  Test passing block to fetch
test(Hash.new.fetch('xxx') { |el| "go fish #{el}" }, 'go fish xxx', 'Fetch with block')


# There was a bug where you could get a stack overflow because
# Hash#delete(x,&b) called self.delete(x), but if delete was implemented by
# a subclass to call super(x), then you'd be in trouble.  This tests passes
# if we don't have a stack overflow.
class HHash < Hash
  def delete(k)
    super k
  end
end
hh = HHash.new
hh.delete(nil)

class BHash < Hash
  def initialize
    # don't call super
  end
  def self.testLarge(tsize)
    # ensure rebuildTable: tested in presense of nil keys
    h = self.new
    n = 0
    h[nil] = :nilValue
    while n < tsize
      h[n] = n * 10
      h[n.to_s] = 0 - n
      n += 1
    end
    # puts "done building large Hash"
    exp_size = (tsize * 2) + 1
    unless (sx = h.size) == exp_size ; raise 'error'; end
    n = 0
    unless (x = h[nil])._equal?( :nilValue)  ; raise 'error'; end
    while n < tsize
      unless (x = h[n]) == n * 10   ; raise 'error'; end
      unless (x = h[n.to_s]) == 0 - n   ; raise 'error'; end
      n += 1
    end
    n = tsize - 1
    h.delete(nil)
    while n >= 0
       unless (y = h.delete(n))._equal?(n * 10); raise 'error'; end
       unless (y = h.delete(n.to_s)) == 0 - n; raise 'error'; end
       n -= 1
    end
    h.each_key {|k| raise 'error' }
    h.each_pair {|k,v | raise 'error' }
    unless h.size == 0 ; raise 'error'; end
    # puts "done testLarge"
    true
  end

  def self.testLargeIdentityHash(test_size) # [
    allows_nil = true
    nil_count = 1
    # puts "begin large IdentityHash"
    tsize = test_size
    h = IdentityHash.new
    n = 0
    strings = []
    vals = []
    if allows_nil ; h[nil] = :nilValue ; end
    while n < tsize
      h[n] = n * 10
      str = n.to_s
      strings << str
      h[str] =  n
      vals << n
      unless (y = h[str])._equal?(n) ; raise 'error'; end

      str = str.dup
      strings << str
      h[str] = (v = n * 3)
      vals << v
      unless (y = h[str])._equal?(v) ; raise 'error'; end
      n += 1
    end
    if allows_nil
      unless (y = h[nil])._equal?( :nilValue) ; raise 'error'; end
    end
    unless (y = h.size) == (vx = (tsize * 3) + nil_count ) ; raise 'error'; end
    vidx = vals.size - 1
    while vidx >= 0
      str = strings[vidx]
      unless (y = h[str])._equal?( vx = vals[vidx]) ; raise 'error'; end
      vidx -= 1
    end
    n = 0
    while n < tsize
      unless (y = h[n])._equal?( n * 10) ; raise 'error'; end
      n += 1
    end
    if allows_nil
      unless (y = h.delete(nil))._equal?( :nilValue)  ; raise 'error'; end
    end
    vidx = vals.size - 1
    n = 0
    while vidx >= 0
      unless (y = h.delete(strings[vidx]))._equal?( vx = vals[vidx]); raise 'err';end
      unless (y = h.delete(n))._equal?( n * 10 ) ; raise 'err';end
      vidx -= 2
      n += 1
    end
    vidx = vals.size - 2
    while vidx >= 0
      unless (y = h.delete(strings[vidx]))._equal?( vals[vidx]); raise 'err';end
      vidx -= 2
    end
    h.each_key {|k| raise 'error' }
    h.each_pair {|k,v | raise 'error' }
    unless h.size == 0 ; raise 'error'; end
    unless h.size == 0 ; raise 'error'; end
    # puts "done testLargeIdentityHash #{tsize}  "
  end # ]

end

# Another bug where subclasses don't call super in their initialize method
# caused problems.  This test case passes if there is no exception.
bh = BHash.new
bh['foo']

[ 50 , 500, 5000, 50000 ].each {|tsize|
  puts "large Hashes size #{tsize}"
  BHash.testLarge(tsize) # test of rebuildTable
  BHash.testLargeIdentityHash(tsize)
}
puts "done large Hashes"

# A bug when doing a merge of a certain size forgetting to copy the default
# value over to the new hash.
#
# From Cucumber lib/cucumber/formatter/ansicolor.rb
orig = Hash.new do |h,k|
  if k.to_s =~ /(.*)_param/
    h[$1] + ',bold'
  end
end.merge({'undefined' => 'yellow',
           'pending'   => 'yellow',
           'failed'    => 'red',
           'passed'    => 'green',
           'outline'   => 'cyan',
           'skipped'   => 'cyan',
           'comment'   => 'grey',
           'tag'       => 'cyan'})
test(orig['pending_param'], 'yellow,bold', "merge bug")

# Test case from rails:
#
# Maglev was complaining of "too many args" on the super call, due to block
# being passed behind the scenes.
#
# Test case passes if no exception
class Foo < Hash
  def initialize(a,b,c)
    super(a)
  end
end

f = Foo.new(0,1,2) { 10 }

# Test case from I18N:  *some* modifications are allowed during iteration,
# in particular, it looks like delete is allowed.

# From I18N gem:  Test case passes if no exception
values = { :a => "A", :b => "B" }
values.each do |key,value|
  values.delete(key) if key == :a
end

test(values, { :b => "B"}, "delete allowed during iteration")

# coverage for extraious nil values showing up after rebuild in the
#  presence of dynamic instvars
h = h = Hash.new
h.instance_variable_set( '@aa', 'AA' )
h.instance_variable_set( '@ac', 'AC' )
arr = []
100.times { |i| arr << i * 1000 ; h[i] = i * 1000 }
vb = h.values

sa = IdentitySet.with_all(vb)
sb = IdentitySet.with_all(arr)
unless sa == sb ; raise 'fail'; end
vb.each { |elem| 
  if elem.equal?(nil)
     raise 'fail'
  end
}


report
