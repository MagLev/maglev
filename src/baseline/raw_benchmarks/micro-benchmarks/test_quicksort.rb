class Array
  def qsort
    return [] if self.empty?
    pivot, *tail = self
    (tail.select {|el| el < pivot }).qsort + [pivot] +
      (tail.select {|el| el >= pivot }).qsort
  end
end

[1].map do |n|
  fname = File.dirname(__FILE__) + "/random.input"
  array = File.read(fname).split(/\n/).map!{|m| m.to_i }
  puts "Quicksort verified." if array.qsort == array.sort
end
