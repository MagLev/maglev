module M019_1
end

class << M019_1
  def t019
  end
end

module M019_2
  def self.t019
  end
end

M019_1.maglev_persistable(true)
M019_2.maglev_persistable(true)
Maglev.commit_transaction
