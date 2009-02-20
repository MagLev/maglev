class ClassSupOne
  def call(m)
    puts "One"
    m + 100
  end 
end 
class ClassSupZ
  def call
    puts "Z"
    98
  end 
end 
class ClassSupZChild < ClassSupZ
  def call
    super
  end 
  def testSuper
    x = self.call
  end
end

class ClassSupOneChild < ClassSupOne
  def call(m)
    super(m)
  end
  def testSuper
    self.call(5) 
  end
end
x = ClassSupZChild.new.testSuper()
unless x = 105 ; raise 'err'; end
x = ClassSupOneChild.new.testSuper()
unless x = 98 ; raise 'err'; end
true
