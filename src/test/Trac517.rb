puts "Should print \"Extended called\" on the following line"

module M
  @state = nil

  def self.state
    @state
  end
  
  def self.extended(base)
    @state = "Extended called"
  end
end

class C
  extend M
end

raise Exception unless M.state == "Extended called"
