module M018_1
  def t018_1
  end
end
module M018_2
  def t018_2
  end
end

class T018
  extend M018_1
  extend M018_2
end

M018_1.maglev_persistable(true)
M018_2.maglev_persistable(true)
T018.maglev_persistable(true)
Maglev.commit_transaction
