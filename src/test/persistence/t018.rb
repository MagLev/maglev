module M018
  def t018_c
  end
end

class T018
  extend M018
end

M018.maglev_persistable(true)
T018.maglev_persistable(true)
Maglev.commit_transaction
