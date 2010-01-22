# Distilled from psych: the new YAML lib for Ruby
o = Object.new
ivars = o.respond_to?(:to_yaml_properties) ?
  o.object_id :
  o.instance_variables
p ivars
