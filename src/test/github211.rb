# Passes if no error is raised

threads = [Thread.start {}]
threads[0].join
threads[0].join(10)
threads.map(&:join)
