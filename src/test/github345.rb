arrays = select nil, nil, nil, 0.01

raise "#select should return nil when no selectors are provided" if arrays._not_equal?(nil)
