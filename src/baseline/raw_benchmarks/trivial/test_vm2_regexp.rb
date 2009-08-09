inputs  = [10, 100, 1000, 10_000]
strings = inputs.map { |n| "a" * n + "hoge" + "b" * n }
idx     = -1

inputs.map do |n|
  string = strings[idx+=1]
  1_000_000.times { /hoge/ =~ string }
end
