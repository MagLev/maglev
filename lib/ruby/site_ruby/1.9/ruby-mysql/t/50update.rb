$my.query("update test set id=0, str='hoge'")
if $my.affected_rows != 2 then raise "update: failed" end
