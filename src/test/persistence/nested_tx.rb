Maglev.persistent do
  X10 = "10"
  X20 = "ORIGINAL"

  Maglev.transient do
    X20 = "20"
    Maglev.commit_transaction
  end

  Maglev.abort_transaction # doesn't do what you want...
end
p X10
p X20
