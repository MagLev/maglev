Require 'rdoc/rdoc'

RDOC_DIR = ENV['MAGLEV_HOME'] + '/lib/ruby/1.8/rdoc'

rdoc = RDoc::RDoc.new
rdoc.document(['-o /tmp/foo', RDOC_DIR])
