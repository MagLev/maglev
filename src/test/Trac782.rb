
class K
  def store(key,value)
    10
  end
  alias_method :[]=, :store
end

args = []
k = K.new
ok = false
begin
  p k[*args] = 10
  raise "Fail, expecting argument error"
rescue ArgumentError
  ok = true
rescue Exception => e
  raise e
end
ok
