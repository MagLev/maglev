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

require 'rubygems/config_file'

module Gem
  # Since maglev-ruby is a shell script, we want the shebang line for shell
  # scripts created by RubyGems that invoke maglev-ruby to use "#!/usr/bin/env maglev-ruby"
  ConfigFile::PLATFORM_DEFAULTS['install'] = '--env-shebang'
  ConfigFile::PLATFORM_DEFAULTS['update']  = '--env-shebang'
end

Gem::MAGLEV_POSTFIX = "-maglev-"

class Gem::Dependency
  # MagLev specific gems should match in addition to the standard gems
  def name
    return @name if @name.nil?
    return @__patched_name if @__patched_name && @name == @__patched_name

    @__patched_name = @name.dup
    def @__patched_name.===(other)
      if other.to_s.end_with?(Gem::MAGLEV_POSTFIX)
        super(other.to_s[0...-Gem::MAGLEV_POSTFIX.size])
      else
        super(other)
      end
    end
    @__patched_name
  end
end

class Gem::Specification
  def name
    return @name if @name.nil?
    return @__patched_name if @__patched_name && @name == @__patched_name

    @__patched_name = @name.dup
    def @__patched_name.===(other)
      if other.to_s.end_with?(Gem::MAGLEV_POSTFIX)
        super(other.to_s[0...-Gem::MAGLEV_POSTFIX.size])
      else
        super(other)
      end
    end
    @__patched_name
  end

  def sort_obj
    # Prioritize MagLev specific gems
    [@name.end_with?(Gem::MAGLEV_POSTFIX) ? 1 : 0,
     @name,
     @version,
     @new_platform == Gem::Platform::RUBY ? -1 : 1]
  end
end
