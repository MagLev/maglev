# Note the duplicated '_' in the params list to the sort block.
# MagLev complains about it.

methods = [["b2", "ab", "c2", "e2", "d2"],
           ["b1", "a1", "c1", "d1", "e1"]]

expected = [["b1", "a1", "c1", "d1", "e1"],
            ["b2", "ab", "c2", "e2", "d2"]]

x = methods.sort_by do |_, k, a, _, m|
  [k, a, m].compact
end

raise "Fail" unless x == expected

