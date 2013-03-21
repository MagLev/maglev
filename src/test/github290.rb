# Test the dup-ability of Modules.

Time.now
Time.new
T2 = Time.dup
T2.now
T2.new
Time.now
Time.new
raise "Does not correctly dup Modules" unless T2.ancestors != Time.ancestors

