#load 'ostruct'

class OpenStruct
  def initialize(hash=nil)
    @table = {}
    if hash
      for k,v in hash
        @table[k.to_sym] = v
        new_ostruct_member(k)
      end
    end
  end
end
x = OpenStruct.new
