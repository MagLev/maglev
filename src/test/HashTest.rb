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
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: true
    def new2
        hash1 = Hash[1=>100, 2=>200, 3=>300]
        hash2 = hash1.each_pair {|key, value|}
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: true
    def new3
        hash1 = Hash.new
        hash1[1] = 100
        hash1[2] = 200
        hash1[3] = 300
        hash2 = hash1.each_pair {|key, value|}
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: true
    def new4
        hash1 = Hash.new
        hash1 = {1=>100, 2=>200, 3=>300}
        hash2 = hash1.each_pair {|key, value|}
        if hash1 == hash2
            return true
        else
            return false
        end
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
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: true
    def copy
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash2 = hash1
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: true
    def clear
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.clear()
        if hash.empty?()
            return true
        else
            return false
        end
    end

    # Expected value: nil
    def default1
        hash = Hash[1, 100, 2, 200, 3, 300]
        return hash.default(key=nil)
    end

    # Expected value: 200
    def default2
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.default = 200
        return hash.default
    end

    # Expected value: true
    def delete
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash1.delete(2)
        hash2 = hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: true
    def deleteIf
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash1.delete_if {|key, value| key == 2}
        hash2 = hash1.each_pair {|key, value|}
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: '300'
    def fetch
        hash = Hash[1, 100, 2, 200, 3, 300]
        return hash.fetch(3)
    end

    # Expected value: true if key = a..c
    def hasKey(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.has_key?(key)
            return true
        else
            return false
        end
    end

    # Expected value: true if key = a..c
    def include(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.include?(key)
            return true
        else
            return false
        end
    end

    # Expected value: true if key = a..c
    def member(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.member?(key)
            return true
        else
            return false
        end
    end

    # Expected value: true if key = a..c
    def key(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.key?(key)
            return true
        else
            return false
        end
    end

    # Expected value: true if value = "abc", "def", or "ghi"
    def hasValue(value)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.has_value?(value)
            return true
        else
            return false
        end
    end

    # Expected value: "a" if value = "abc", "b" if value "def",
    #                 or "c" is value = "ghi", otherwise nil
    def index(value)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        @hh = hash
        return hash.index(value)
    end

    # Expected value: true
    def replace
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        hash1.replace(hash2)
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: false
    def invert
        hash1 = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        hash2 = hash1.invert()
        if hash1 == hash2
            return false
        else
            return true
        end
    end

    # Expected value: 4
    def length
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst', 'd', 'deq']
        return hash.length()
    end

    # Expected value: 5
    def size
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst', 'd', 'deq', 'e', 'fbi']
        return hash.length()
    end

    # Expected value: true
    def merge1
        hash1 = Hash['a', 'abc', 'b', 'def']
        hash2 = Hash['y', 'uvw', 'z', 'xyz']
        hash3 = hash1.merge(hash2)
        if hash3 == hash1 or hash3 == hash2
            return false
        else
            return true
        end
    end

    # Expected value: true
    def merge2
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash3 = hash1.merge(hash2)
        if hash3 == hash1 or hash3 == hash2
            return false
        else
            return true
        end
    end

    # Expected value: true
    def merge!
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash3 = hash1.merge!(hash2)
        if hash3 == hash2
            return false
        else
            return true
        end
    end

    # Expected value: true
    def update
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash1.update(hash2)
        if hash1 == hash2
            return false
        else
            return true
        end
    end

    # Expected value: true
    def sort
        hash1 = Hash['a', 'abc', 'c', 'ghi', 'b', 'def']
        a1 = hash1.to_a
        a2 = hash1.sort()
        if a1 == a2
            return false
        else
            return true
        end
    end

    # Expected value: true
    def toHash
        hash1 = Hash['a', 'abc', 'c', 'ghi', 'b', 'def']
        hash2 = hash1.to_hash()
        if hash1 == hash2
            return true
        else
            return false
        end
    end

    # Expected value: aabcbdefcghi
    def toString
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        ret = hash.to_s
        return ret
    end

    def testInspect
      h = Hash['a', 'abc', 'b', 'def' ]
      h['c'] = h
      s = h.inspect
      exp = '{"b"=>"def", "c"=>{...}, "a"=>"abc"}'
      unless s == exp ; raise 'error'; end
      true
    end
end

# Call test methods
test(HashTest.new.new1(),true,           "new1")
test(HashTest.new.new2(),true,           "new2")
test(HashTest.new.new3(),true,           "new3")
test(HashTest.new.new4(),true,           "new4")
test(HashTest.new.new5(),true,           "new5")
test(HashTest.new.equals(),true,         "equals")
test(HashTest.new.copy(),true,           "copy")
test(HashTest.new.clear(),true,          "clear")
test(HashTest.new.default1(),nil,        "default1")
test(HashTest.new.default2(),200,        "default2")
test(HashTest.new.delete(),true,         "delete")
test(HashTest.new.deleteIf(),true,       "deleteIf")
test(HashTest.new.fetch(),300,           "fetch")
test(HashTest.new.hasKey('a'),true,      "hasKey1")
test(HashTest.new.hasKey('d'),false,     "hasKey2")
test(HashTest.new.include('a'),true,     "include1")
test(HashTest.new.include('d'),false,    "include2")
test(HashTest.new.member('a'),true,      "member1")
test(HashTest.new.member('d'),false,     "member2")
test(HashTest.new.key('a'),true,         "key1")
test(HashTest.new.key('d'),false,        "key2")
test(HashTest.new.hasValue('abc'),true,  "hasValue1")
test(HashTest.new.hasValue('xyz'),false, "hasValue2")
test(HashTest.new.index('def'),'b',      "index")

h = HashTest.new
test(h.index('xyz'),nil,                 "A")
test(HashTest.new.replace(),true,        "replace")
test(HashTest.new.invert(),true,         "invert")
test(HashTest.new.length(),4,            "length")
test(HashTest.new.size(),5,              "size")
test(HashTest.new.merge1(),true,         "merge1")
test(HashTest.new.merge2(),true,         "merge2")
test(HashTest.new.merge!(),true,         "merge!")
test(HashTest.new.update(),true,         "update")
test(HashTest.new.sort(),true,           "sort1")
test(HashTest.new.toHash(),true,         "toHash")
test(HashTest.new.testInspect(),true,    "inspect")

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

report
