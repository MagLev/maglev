class T024
  def t024_1
  end
end

T024.maglev_persistable(true) do |mod| false end
Maglev.commit_transaction

class T024
  def t024_2
  end
end
T024.maglev_persistable(true) do |mod| false end
Maglev.commit_transaction
