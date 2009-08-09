def raise_and_rescue(n)
  n.times do
    begin
      raise
    rescue
    end
  end
end

[300_000].map do |n|
  raise_and_rescue n
end
