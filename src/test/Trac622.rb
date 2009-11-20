begin
  exit
rescue SystemExit => e
  unless 0 == e.status
    raise "Failed Test case: expecting 0 but got #{e.status}"
  end
end
true


