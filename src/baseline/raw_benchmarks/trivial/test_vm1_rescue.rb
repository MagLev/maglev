def rescue_loop(n)
  n.times do
    begin
    rescue
    end
  end
end

[1_000_000].map do |n|
  rescue_loop(1_000_000)
end
