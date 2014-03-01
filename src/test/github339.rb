rs, ws = IO.pipe

arrays = IO.select([rs],[ws])

raise "#select should not return nil" if arrays.first.nil?
