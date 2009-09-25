
class JSON
  attr_reader :count
  def initialize;  @count = 5 end

  def parse
    until eos?()
      case
      when true
        puts "yup"
        break
      else
        puts "else"
      end
    end
  end

  def eos?
    @count -= 1
    @count == 0
  end
end

j = JSON.new
j.parse
raise "FAIL" if j.count == 0
puts "OK"
true
