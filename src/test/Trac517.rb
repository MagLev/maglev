puts "Should print \"Extended called\" on the following line"

module M
  def self.extended(base)
    puts "Extended called"
  end
end

class C
  extend M
end
