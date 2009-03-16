raise "Fail 1" unless (/\A\/+\z/ =~ '').nil?
raise "Fail 2" unless (/\A\/*\z/ =~ '') ==  0  # MagLev fails
raise "Fail 3" unless (/\A\/?\z/ =~ '') == 0   # MagLev fails
raise "Fail 4" unless (/\A\/\z/ =~ '').nil?
raise "Fail 5" unless (/\A\\z/ =~ '').nil?
