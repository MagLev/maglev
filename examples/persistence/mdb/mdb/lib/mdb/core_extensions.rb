
class Hash
  def symbolize_keys
    keys.each do |k|
      self[k.to_sym] = delete(k)
    end
  end
end
