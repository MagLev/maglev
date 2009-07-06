module M
end
expect = [M]
actual = M.ancestors
raise "Fail: expected #{expect.inspect} actual: #{actual.inspect}" unless expect == actual
true
