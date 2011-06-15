# Test Kernel.exec()

cmd = File.join(ENV['MAGLEV_HOME'], 'bin', 'maglev-ruby')

ENV['MAGLEV_OPTS'] = '' # ensure a known environment for the tests

Dir.chdir File.dirname(__FILE__) do
  result = `#{cmd} execTest1.rb`
  raise "Fail: wrong exitstatus #{$?.exitstatus}" unless $?.exitstatus == 1
  raise "Fail: wrong message #{result.inspect}" unless result =~ /No such file or directory - not_a_command/
end
