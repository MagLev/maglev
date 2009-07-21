Maglev.persistent do
  X10 = 123
  X20 = 345
  Maglev.transient do
    X20 = 678
    Maglev.commit_transaction
  end

  Maglev.abort_transaction
end
