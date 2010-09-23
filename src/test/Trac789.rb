# Found doing $MAGLEV_HOME/bin/rake test:units in a rails application.
#
# The files to include are calculated using (ultimately) Dir.glob, and they
# came back in the wrong order.
#
#   Dir.glob('test/unit/**/*_test.rb')
#
# MRI returned:    ["test/unit/helpers/people_helper_test.rb", "test/unit/lint_test.rb"]
# MagLev returned: ["test/unit/lint_test.rb", "test/unit/helpers/people_helper_test.rb"]

Dir.chdir(File.dirname(__FILE__)) do
  files = Dir.glob('lib/test/unit/**/*_test.rb')
  expected = ["lib/test/unit/helpers/people_helper_test.rb", "lib/test/unit/lint_test.rb"]
  raise "Wrong order #{files.inspect}" unless files == expected
end

