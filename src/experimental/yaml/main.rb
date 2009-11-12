require 'yaml2.rb'

p MAML.yaml_get_version_string
yaml = "- Mark McGwire" # a small yaml filef
parser = MAML::Parser.new(yaml)
p parser.parse
