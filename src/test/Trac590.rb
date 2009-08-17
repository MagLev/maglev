# From MiniTest
def capture_io
  require 'stringio'

  orig_stdout, orig_stderr         = $stdout, $stderr
  captured_stdout, captured_stderr = StringIO.new, StringIO.new
  $stdout, $stderr                 = captured_stdout, captured_stderr

  yield

  return captured_stdout.string, captured_stderr.string
ensure
  $stdout = orig_stdout
  $stderr = orig_stderr
end


out, err = capture_io do
  puts "Hi"
  warn "bye"
end

raise "Failed on stdout: #{out.inspect}" unless out == "Hi\n"
raise "Failed on stderr: #{err.inspect}" unless err == "bye\n"
