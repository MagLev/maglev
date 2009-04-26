h = { }

# Used to raise ERROR 2010, Undefined method `to_hash' for nil
raise 'fail 1' unless  (h == nil) == false

raise 'fail 2' unless h != nil
