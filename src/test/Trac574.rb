require File.expand_path('simple', File.dirname(__FILE__))
require 'yaml'

# Regular string
test_cases =
  [
   ["foo",  "--- foo\n"],
   [":foo", "--- \":foo\"\n"],
   [:foo,   "--- :foo\n" ]
  ]

test_cases.each do |orig,yaml|
  yaml_obj = YAML::dump(orig)
  test(yaml_obj, yaml, "dumping #{orig.inspect}")

  copy = YAML::load(yaml_obj)
  test(orig, copy, "loading #{orig.inspect}")
  test(orig.class, copy.class, "loading #{orig.inspect} class")
end

report
