class M015
  def m015_1
    puts "m015_1"
  end
end
M015.maglev_persistable(true)
Maglev.commit_transaction
class M015
  def m015_2
    puts "m015_2"
  end
end
M015.maglev_persistable(true)
Maglev.commit_transaction
