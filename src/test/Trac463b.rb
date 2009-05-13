# Should be in an infinite loop.
# SEGV in MagLev

while true
    begin
        next
      rescue ZeroDivisionError => err
        p "This never happens"
      end
   end

