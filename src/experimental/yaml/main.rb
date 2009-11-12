require 'libyaml'
require 'maml'

p LibYaml.yaml_get_version_string
yaml = "- Mark McGwire" # a small yaml filef
parser = Maml::Parser.new(yaml)
p parser.parse
