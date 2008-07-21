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

class HashTest
    # Expected value: '1=>100|2=>200|3=>300|'
    def new1
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: '1=>100|2=>200|3=>300|'
    def new2
        hash = Hash[1=>100, 2=>200, 3=>300]
        hash.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: '1=>100|2=>200|3=>300|'
    def new3
        hash = Hash.new
        hash[1] = 100
        hash[2] = 200
        hash[3] = 300
        hash.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: '1=>100|2=>200|3=>300|'
    def new4
        hash = Hash.new
        hash = {1=>100, 2=>200, 3=>300}
        hash.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: true
    def equals
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash2 = Hash.new
        hash2[1] = 100
        hash2[2] = 200
        hash2[3] = 300
        if hash1 == hash2
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true
    def copy
        hash1 = Hash[1, 100, 2, 200, 3, 300]
        hash2 = hash1
        if hash1 == hash2
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true
    def clear
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.clear()
        if hash.empty?()
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: nil
    def default1
        hash = Hash[1, 100, 2, 200, 3, 300]
        puts hash.default(key=nil)
    end

    # Expected value: 200
    def default2
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.default = 200
        puts hash.default
    end

    # Expected value: '1=>100|3=>300|'
    def delete
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.delete(2)
        hash.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: '1=>100|3=>300|'
    def deleteIf
        hash = Hash[1, 100, 2, 200, 3, 300]
        hash.delete_if {|key, value| key == 2}
        hash.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: '300'
    def fetch
        hash = Hash[1, 100, 2, 200, 3, 300]
        puts hash.fetch(3)
    end

    # Expected value: true if key = a..c
    def hasKey(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.has_key?(key)
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true if key = a..c
    def include(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.include?(key)
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true if key = a..c
    def member(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.member?(key)
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true if key = a..c
    def key(key)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.key?(key)
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: true if value = "abc", "def", or "ghi"
    def hasValue(value)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        if hash.has_value?(value)
            puts "true"
        else
            puts "false"
        end
    end

    # Expected value: "a" if value = "abc", "b" if value "def",
    #                 or "c" is value = "ghi", otherwise nil
    def index(value)
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        puts hash.index(value)
    end

    # Expected value: x=>xyz|y=>uvw|z=>rts
    def replace
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        hash1.replace(hash2)
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: x=>xyz|y=>uvw|z=>rts
    def inspect
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        puts hash.inspect()
    end

    # Expected value: z=>rts|y=>uvw|x=>xyz
    def invert
        hash1 = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst']
        hash2 = hash1.invert()
        hash2.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: 4
    def length
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst', 'd', 'deq']
        puts hash.length()
    end

    # Expected value: 5
    def size
        hash = Hash['x', 'xyz', 'y', 'uvw', 'z', 'rst', 'd', 'deq', 'e', 'fbi']
        puts hash.length()
    end

    # Expected value: a=>abc|b=>def|y=>uvw|z=>xyz
    def merge1
        hash1 = Hash['a', 'abc', 'b', 'def']
        hash2 = Hash['y', 'uvw', 'z', 'xyz']
        hash3 = hash1.merge(hash2)
        hash3.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: a=>abc|b=>def|c=>jkl|z=>xyz
    def merge2
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash3 = hash1.merge(hash2)
        hash3.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: a=>abc|b=>def|c=>jkl|z=>xyz
    def merge!
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash1.merge!(hash2)
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: a=>abc|b=>def|c=>jkl|z=>xyz
    def update
        hash1 = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        hash2 = Hash['c', 'jkl', 'z', 'xyz']
        hash1.update(hash2)
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: a=>abc|b=>def|c=>ghi|
    def sort
        hash1 = Hash['a', 'abc', 'c', 'ghi', 'b', 'def']
        hash2 = hash1.sort()
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: a=>abc|b=>def|c=>ghi|
    def toHash
        hash1 = Hash['a', 'abc', 'c', 'ghi', 'b', 'def']
        hash2 = hash1.to_hash()
        hash1.each_pair {|key, value| print "#{key}=>#{value}|"}
        puts
    end

    # Expected value: aabcbdefcghi
    def toString
        hash = Hash['a', 'abc', 'b', 'def', 'c', 'ghi']
        puts hash.to_s
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

# expectvalue '1=>100|2=>200|3=>300|'
HashTest.new.new1()

# expectvalue '1=>100|2=>200|3=>300|'
HashTest.new.new2()

# expectvalue '1=>100|2=>200|3=>300|'
HashTest.new.new3()

# expectvalue '1=>100|2=>200|3=>300|'
HashTest.new.new4()

# expectvalue true
HashTest.new.equals()

# expectvalue true
HashTest.new.copy()

# expectvalue true
HashTest.new.clear()

# expectvalue nil
HashTest.new.default1()

# expectvalue 2
HashTest.new.default2()

# expectvalue '1=>100|3=>300|'
HashTest.new.delete()

# expectvalue '1=>100|3=>300|'
HashTest.new.deleteIf()

# expectvalue 300
HashTest.new.fetch()

# expectvalue true
HashTest.new.hasKey('a')

# expectvalue false
HashTest.new.hasKey('d')

# expectvalue true
HashTest.new.include('a')

# expectvalue false
HashTest.new.include('d')

# expectvalue true
HashTest.new.member('a')

# expectvalue false
HashTest.new.member('d')

# expectvalue true
HashTest.new.key('a')

# expectvalue false
HashTest.new.key('d')

# expectvalue true
HashTest.new.hasValue('abc')

# expectvalue false
HashTest.new.hasValue('xyz')

# expectvalue true
HashTest.new.index('def')

# expectvalue nil
HashTest.new.index('xyz')

# expectvalue 'x=>xyz|y=>uvw|z=>rst|'
HashTest.new.replace()

# expectvalue '{'x'=>'xyz', 'y'=>'uvw', 'z'=>'rst'}
HashTest.new.inspect()

# expectvalue 'uvw=>y|xyz=>x|rst=>z|'
HashTest.new.invert()

# expectvalue 4
HashTest.new.length()

# expectvalue 5
HashTest.new.size()

# expectvalue 'a=>abc|b=>def|y=>uvw|z=>xyz|'
HashTest.new.merge1()

# expectvalue 'a=>abc|b=>def|c=>jkl|z=>xyz|'
HashTest.new.merge2()

# expectvalue 'a=>abc|b=>def|c=>jkl|z=>xyz|'
HashTest.new.merge!()

# expectvalue 'a=>abc|b=>def|c=>jkl|z=>xyz|'
HashTest.new.update()

# expectvalue 'a=>abc|b=>def|c=>ghi|'
HashTest.new.sort()

# expectvalue 'a=>abc|b=>def|c=>ghi|'
HashTest.new.toHash()

# expectvalue 'aabcbdefcghi'
HashTest.new.toString()
