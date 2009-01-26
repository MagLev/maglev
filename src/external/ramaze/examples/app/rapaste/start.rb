require 'rubygems'
require 'tmpdir'

require 'ramaze'
require 'sequel'
require 'uv'

Ramaze::Log.debug "Initializing UltraViolet..."

Uv.copy_files "xhtml", __DIR__("public")
Uv.init_syntaxes

UV_PRIORITY_NAMES = %w[ ruby plain_text html css javascript yaml diff ]

STYLE = 'iplastic'

Ramaze::Log.debug "done."

DB_FILE = __DIR__("rapaste.sqlite") # for specs
DB = Sequel.connect("sqlite://#{DB_FILE}")

require 'model/paste'
require 'controller/paste'

Ramaze.start :adapter => :mongrel, :port => 9952
