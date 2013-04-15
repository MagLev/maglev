class T017
  # instance method
  def t017_i
  end
  # class method
  def self.t017_c
  end
end

T017.maglev_persistable(true)
Maglev.commit_transaction
