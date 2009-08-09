def begin_ensure
  begin
    begin
    ensure
    end
  ensure
  end
end

[100_000, 1_000_000, 10_000_000].map do |n|
  n.times { begin_ensure }
end
