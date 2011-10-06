def foo(&b)
  raise "fail" unless b.call == 1
  raise "fail" unless yield == 1
  b = proc { "foo" }
  raise "fail" unless b.call == "foo"
  raise "fail" unless yield == 1
  b = proc { yield + 1 }
  raise "fail" unless b.call == 2
  raise "fail" unless yield == 1
end
foo { 1 }

def foo2(&b)
  yield
  raise "fail: should not get here from break"
end
foo2 { break }
