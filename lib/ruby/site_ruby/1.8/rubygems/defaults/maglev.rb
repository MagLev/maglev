#--
# maglev.rb
#
# Copyright (C) 2009 GemStone Systems, Inc. All rights reserved.
#
#++

# = Overview
#
# File read by RubyGems that allows Ruby implementers to set defaults
# appropriate for their implementation.
#
# For more information on this file, see
# http://www.mail-archive.com/rubygems-developers@rubyforge.org/msg02161.html
#
#
# NOTE: Currently, this isn't working due to a bug in maglev's handling of optparse.
require 'rubygems/config_file'

module Gem
  # Since maglev-ruby is a shell script, we want the shebang line for shell
  # scripts created by RubyGems that invoke maglev-ruby to use "#!/usr/bin/env maglev-ruby"
  ConfigFile::PLATFORM_DEFAULTS['install'] = '--env-shebang --no-rdoc --no-ri'
  ConfigFile::PLATFORM_DEFAULTS['update']  = '--env-shebang --no-rdoc --no-ri'
end
