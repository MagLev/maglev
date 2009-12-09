# Just test that the methods are there and don't throw exceptions

ns = Maglev::Gprof.compute_interval(1)
monitor = Maglev::Gprof.create(ns)
monitor.resume_sampling
10_000.times {|i| Array.new(i) }
monitor.suspend_sampling
s = monitor.stop_and_report


