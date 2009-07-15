# A default SystemExit exception will set its return value to 0
# (success).  Exiting by raising a SystemExit should be ok...
require File.expand_path('simple', File.dirname(__FILE__))

def dash_e(cmd, expected_bool, expected_num)
  sys_val = system "maglev-ruby -e '#{cmd}'"
  exit_val = $?.exitstatus
  test(sys_val,  expected_bool, "[#{cmd}] via -e: sys_val")
  test(exit_val, expected_num,  "[#{cmd}] via -e: exit_val")
end

# Test exit 0 via -e
dash_e("raise SystemExit", true, 0)
dash_e("exit 0", true, 0)

# Test non-zero exit status via -e
dash_e("raise SystemExit.new(22)", false, 22)
dash_e("exit 44", false, 44)

report
