require File.expand_path('simple', File.dirname(__FILE__))

# Quick test
t1 = Time.now
1_000_000.times do |i|
  x = Object.new
end
t2 = Time.now
diff = t2 - t1

test(diff > 0, true, 'Time has passed...')


# From pickaxe:
delta = 2_592_000
t = Time.now
t2 = t + delta
test(t2 - t,     delta.to_f, "t2 - t")
test(t2 - delta, t,          "t2 - delta")

report

true
