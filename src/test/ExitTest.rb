# A default SystemExit exception will set its return value to 0
# (success).  Exiting by raising a SystemExit should be ok...
require File.expand_path('simple', File.dirname(__FILE__))

["raise SystemExit", "exit 0"].each do |s|
  sys_val = system "maglev-ruby -e '#{s}'"
  exit_val = $?.to_i
  test(sys_val, true,  "[#{s}]: sys_val")
  test(exit_val, 0, "[#{s}]: exit_val")
end

report

