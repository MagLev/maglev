rp, wp = IO.pipe
mesg = "ping "
result = ""

10.times do
  rs, ws, = IO.select([rp], [wp])
  # puts "rs: #{rs.inspect}"
  # puts "ws: #{ws.inspect}"
  if rs && r = rs[0]
    ret = r.read(5)
    result << ret
    case ret
    when /ping/
      mesg = "pong\n"
    when /pong/
      mesg = "ping "
    end
  end
  if ws && w = ws[0]
    w.write(mesg)
  end
end

expected = "ping pong\nping pong\nping pong\nping pong\nping "
raise "IO select did not work" unless result == expected

unless rp.class == IO
  raise "Pipes should pose as instantiating class, so matching in case statements works"
end
