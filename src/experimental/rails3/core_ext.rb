# Patches to Core classes

# Bundler assumes to_yaml_properties is defined on Object.
# Psych treats it as an override only.  Patch a default impl
# that works with psych
class Object
  def to_yaml_properties
    instance_variables
  end
  def self.yaml_as(tag)
    Psych.add_tag(tag, self)
  end
end
