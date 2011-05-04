# Ensure Kernel#exec searches for executable on $PATH

lib_dir = File.expand_path File.join(File.dirname(__FILE__), 'lib')
ENV['PATH'] = ENV['PATH'] + ":#{lib_dir}"

# Allow this to be run with either maglev or mri
ruby = (defined?(Maglev) ? File.join(ENV['MAGLEV_HOME'], 'bin', 'maglev-ruby') : 'ruby')

Dir.chdir(File.dirname(__FILE__)) do
  ["trac897.sh", "./lib/trac897.sh"].each do |tc|

    # Fire these off in a child process, since we're doing an exec
    output = `#{ruby} -e 'Kernel.exec("#{tc}")'`
    raise "Fail on test case: #{tc} (#{output})" unless $? == 0

    # Try it with some parameters to the command
    output = `#{ruby} -e 'Kernel.exec("#{tc}", "a", "b")'`
    raise "Fail on test case: #{tc} a b (#{output})" unless $? == 0
  end
end
