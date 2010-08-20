
class K
  def store(key,value)
    10
  end
  alias_method :[]=, :store
end

args = []
k = K.new
begin
  p k[*args] = 10
  raise "Fail, expecting argument error"
rescue ArgumentError
  # OK
rescue Exception => e
  raise e
end
