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
      return s == exp
    end
end


###
# Call test methods
###

puts ""
puts "****************************"
puts "Beginning method invocations"
puts "****************************"
puts ""

# expectvalue true
ret = HashTest.new.new1()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.new2()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.new3()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.new4()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.equals()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.copy()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.clear()
raise "ERROR" unless ret == true

# expectvalue nil
ret = HashTest.new.default1()
raise "ERROR" unless ret == nil

# expectvalue 200
ret = HashTest.new.default2()
raise "ERROR" unless ret == 200

# expectvalue true
ret = HashTest.new.delete()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.deleteIf()
raise "ERROR" unless ret == true

# expectvalue 300
ret = HashTest.new.fetch()
raise "ERROR" unless ret == 300

# expectvalue true
ret = HashTest.new.hasKey('a')
raise "ERROR" unless ret == true

# expectvalue false
ret = HashTest.new.hasKey('d')
raise "ERROR" unless ret == false

# expectvalue true
ret = HashTest.new.include('a')
raise "ERROR" unless ret == true

# expectvalue false
ret = HashTest.new.include('d')
raise "ERROR" unless ret == false

# expectvalue true
ret = HashTest.new.member('a')
raise "ERROR" unless ret == true

# expectvalue false
ret = HashTest.new.member('d')
raise "ERROR" unless ret == false

# expectvalue true
ret = HashTest.new.key('a')
raise "ERROR" unless ret == true

# expectvalue false
ret = HashTest.new.key('d')
raise "ERROR" unless ret == false

# expectvalue true
ret = HashTest.new.hasValue('abc')
raise "ERROR" unless ret == true

# expectvalue false
ret = HashTest.new.hasValue('xyz')
raise "ERROR" unless ret == false

# expectvalue 'b'
ret = HashTest.new.index('def')
raise "ERROR" unless ret == 'b'

# expectvalue nil
h = HashTest.new
ret = h.index('xyz')
raise "ERROR" unless ret == nil

# expectvalue true
ret = HashTest.new.replace()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.invert()
raise "ERROR" unless ret == true

# expectvalue 4
ret = HashTest.new.length()
raise "ERROR" unless ret == 4

# expectvalue 5
ret = HashTest.new.size()
raise "ERROR" unless ret == 5

# expectvalue true
ret = HashTest.new.merge1()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.merge2()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.merge!()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.update()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.sort()
raise "ERROR" unless ret == true

# expectvalue true
ret = HashTest.new.toHash()
raise "ERROR" unless ret == true

ret = HashTest.new.testInspect()
raise "ERROR" unless ret == true


# expectvalue 'aabcbdefcghi'
# TODO is result always sorted ??? irb seems to be
# ret = HashTest.new.toString()
# raise "ERROR" unless ret == 'aabcbdefcghi'

Hash.new.any? { |x| true }
