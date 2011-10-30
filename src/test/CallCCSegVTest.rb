begin
  begin; require 'continuation'; rescue LoadError; end
  cc = callcc {|cc| cc }
  cc.call if cc
  CONTINUATIONS_SUPPORTED = true
rescue Exception
  CONTINUATIONS_SUPPORTED = false
end
