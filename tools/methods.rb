
class Methods
  def initialize(h_of_h = nil)
    @methods = Hash.new { |h,k| h[k] = Hash.new(0) }
    unless h_of_h.nil?
      h_of_h.each do |klass, methods|
        @methods[klass] = methods.dup
      end
    end
  end

  def add(klass, method, count = 1)
    @methods[klass][method] += count.to_i
  end

  def [](k)
    @methods[k]
  end

  def has_entry?(klass, method)
    @methods.has_key?(klass) &&
      @methods[klass].has_key?(method)
  end

  def -(other)
    raise TypeError unless other.is_a?(Methods)
    diff = Methods.new
    @methods.keys.each do |k|
      if other[k].nil?
        diff[k] = self[k].dup
      else
        @methods[k].keys.each do |v|
          diff.add(k,v) unless other.has_entry?(k,v)
        end
      end
    end
    diff
  end

  # Pretty print the list of class method pairs.
  def pprint
    start_col = 17
    width = 80 - start_col

    @methods.sort.each do |k,v|
      printf "%-15s", k
      cur_col = start_col

      v.keys.sort.each do |k|
        needed = k.length + 1
        if cur_col + needed > width
          printf "\n%s", " " * 15
          cur_col = start_col
        end
        printf "%s ", k
        cur_col += needed
      end
      printf "\n\n"
    end
  end

  def inspect
    @methods.inspect
  end
end

if __FILE__ == $0
  a = Methods.new
  b = Methods.new
  a.add('a', 'a1')
  a.add('a', 'a2')
  a.add('a', 'a3')

  puts
  puts "a.has_entry?(a, a1): #{a.has_entry?('a', 'a1')}"
  puts "a.has_entry?(a, a10): #{a.has_entry?('a', 'a10')}"
  puts "a.has_entry?(b, a1): #{a.has_entry?('b', 'a1')}"
  puts

  b.add('a', 'a1')
  b.add('a', 'a2')
  b.add('a', 'a3')

  a.add('b', 'b1')
  a.add('b', 'b2')

  b.add('b', 'b1')

  b.add('c', 'c1')
  b.add('c', 'c2')

  puts a.inspect
  puts b.inspect
  aMinusB = a - b
  puts aMinusB.inspect
end
