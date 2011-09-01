class Array
  def to_route; extend Route; end
end

module Route; end
class Subclass < Array; end

class Test
  def ma
     ax = nil
     bx = nil
     sx = nil
     [[4,5,6]].each do |a,b,c|
       ax = "#{a},#{b},#{c}"
     end

     [ [4,5,6].to_route].each do |a,b,c|
       bx = "#{a},#{b},#{c}"
     end

     [ Subclass[4,5,6]].each do |a,b,c|
       sx = "#{a},#{b},#{c}"
     end

     raise Exception unless ax == bx
     raise Exception unless ax == sx
  end
end
Test.new.ma
true
