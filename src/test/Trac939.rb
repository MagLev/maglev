parent = Thread.current
child = Thread.start { sleep 1; parent.wakeup; Thread.stop }
now = Time.now
child.join(5)
timeout = Time.now - now
raise "Error: wakeup did not cancel the Thread join" if timeout > 2

true
