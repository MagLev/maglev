$q = ['boo']

while not $q.empty?
    begin
        next if $q.shift
      rescue ZeroDivisionError => err
        p "This never happens"
      end
   end

