# Distilled from Sinatra
class MyApp
  def configure(*args, &b)
    SinatraApp.configure(*args, &b)
  end
end

class Base
  def self.configure(*args, &b)
    puts args.inspect
    raise "Expecting block" unless block_given?
  end
end


class SinatraApp < Base
  def self.configure(*envs)
    super
  end
end

app = MyApp.new
app.configure do
  puts "In a block"
end

puts "Passed 375"
true
