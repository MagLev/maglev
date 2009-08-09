def tarai( x, y, z )
  if (x <= y)
    y
  else
    tarai(tarai(x-1, y, z),
          tarai(y-1, z, x),
          tarai(z-1, x, y))
  end
end

[3, 4, 5].map do |n|
  tarai(12, n, 0)
end
