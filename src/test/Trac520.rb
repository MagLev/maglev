# Test time remaining until Christmas 2009. Should report negative days afterwards.
puts "Its now #{Time.now}"
timespan = [86400,3600,60].inject([(Time.local(2009,12,25)-Time.now).to_i]){|a,v|p=a.pop;a+=[p/v,p%v]}
puts "%s days, %s minutes, %s hours, and %s seconds left until Christmas 2009."%timespan
raise Exception unless timespan.first < 0
