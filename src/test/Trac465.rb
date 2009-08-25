def g
  yield
end

g { break }


# Further test cases: blocks and procs work differently
def m1
  yield
  :m1
end

def m2(pr)
  pr.call
  :m2
end

x = m2(proc { break 37 } )
raise "Expecting :m2 but got #{x} for break in block" unless x == :m2

x = m1 {  break 44 }
raise "Expecting 44  but got #{x} for break in proc"  unless x == 44
