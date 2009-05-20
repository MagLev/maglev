if defined? Maglev
  # Whereas persistence is messed up in MagLev right now,
  # and whereas GStore does a commit, which doesn't save the methods,
  # and whereas we want to run this multiple times,
  # be it resolved to use load rather than require for maglev...
  load 'benchmark.rb'
  load 'gstore.rb'
  $db = GStore.new("gstore.db")
else
  require 'benchmark'
  require 'pstore'
  File.delete("pstore.db") if File.exists?("pstore.db")
  $db = PStore.new("pstore.db")
end

def do_work_one_txn(db, count)
  for i in (1..count)
    a = Array.new
    for j in (1..100)
      a << "#{i} #{j} #{Time.now}"
    end
  end
  db.transaction { |ps| ps[i.to_s()] = a }
end

def do_work_n_txns(db, count)
  for i in (1..count)
    a = Array.new
    for j in (1..100)
      a << "#{i} #{j} #{Time.now}"
    end
    db.transaction { |ps| ps[i.to_s()] = a }
  end
end

Benchmark.bm do |x|
  count = 100
#  x.report("do_work_n_txns")  { do_work_n_txns($db, count) }
  x.report("do_work_one_txn") { do_work_one_txn($db, count) }
end
