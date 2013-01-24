res = $my.query("select * from test")
if res.num_rows != 2 then raise "num_rows: failed" end
if res.fetch_row != ["1", "foo"] then raise "fetch_row: failed" end
if res.fetch_hash != {"id"=>"2", "str"=>"bar"} then raise "fetch_hash: failed" end
