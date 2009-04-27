a = %w{ a b c d }
a.insert(2,99)
raise 'Fail 1' unless a == ["a", "b", 99, "c", "d"]

a.insert(-2, 1, 2, 3)
raise 'Fail 2' unless a == ["a", "b", 99, "c", 1, 2, 3, "d"]
