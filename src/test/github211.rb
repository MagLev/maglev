# Passes if no error is raised

threads = [Thread.start {}]
thread[0].join
thread[0].join(10)
threads.map(&:join)
