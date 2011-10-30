# This should not raise an exception.
Maglev.abort_transaction
$i = 0
Thread.start { callcc {|cc| $cont = cc}; $i += 1 }
Maglev::PERSISTENT_ROOT["cc"] = $cont
Maglev.commit_transaction

begin
  Thread.start { Maglev::PERSISTENT_ROOT["cc"].call }
  raise "resuming persisted continuation didn't work" unless $i == 2
ensure
  Maglev.abort_transaction
  Maglev::PERSISTENT_ROOT.delete("cc")
  Maglev.commit_transaction
end
