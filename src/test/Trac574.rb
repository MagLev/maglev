copy = YAML::load(YAML::dump(:foo))
raise "YAML converted Symbol to #{copy.class}" unless Symbol === copy
