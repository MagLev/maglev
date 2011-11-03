# Taken from yard. This should not raise an exception.
cc = callcc {|cc| cc }
cc.call if cc
CONTINUATIONS_SUPPORTED = true
