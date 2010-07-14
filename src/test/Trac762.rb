# From Thor
module Base
  def start(a=1, config={ })
    config[:shell] ||= 27
    yield(a)
  rescue Exception => e
    raise e
  end
end

class Group
  def start(a=2, config = { })
    # To further confuse matters, if you uncomment the following
    # assignment, then MRI sees 33 in the block, not 27.
    #
    # config[:shell] = 33
    super do |x|
      raise "Fail: expected 27, actual: #{config[:shell]}" unless config[:shell] == 27
    end
  end

  include Base
end

g = Group.new
g.start
true
