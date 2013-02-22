
class IdentityHash

  def compare_by_identity?
    true
  end

  def self.from_hash(obj)
    hash = Hash.new
    hash.compare_by_identity
    
    obj.each_pair { |k, v|
      hash[k] = v
    }
    hash
  end

  def default=(value)
    unless value._equal?(nil)
      raise ArgumentError , 'non-nil default value not supported by IdentityHash'
    end
  end

  #--
  # Psych/Yaml support.
  # IdentityHash also registered in lib/ruby/1.8/psych.rb
  #++

  # Psych hook method for dumping to YAML
  def encode_with(coder)
    # serialize as a YAML sequence
    coder.represent_map(self.class.name, self)
  end

  # Psych hook method for reviving from YAML
  def init_with(coder)
    coder.map.each_pair { |k,v| self[k] = v }
  end
end
