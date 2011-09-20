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
require 'fileutils'

module Gem
  # Since maglev-ruby is a shell script, we want the shebang line for shell
  # scripts created by RubyGems that invoke maglev-ruby to use "#!/usr/bin/env maglev-ruby"
  ConfigFile::PLATFORM_DEFAULTS['install'] = '--env-shebang'
  ConfigFile::PLATFORM_DEFAULTS['update']  = '--env-shebang'
end

# The postfix we are look for to install patched gems released by the MagLev team
Gem::MAGLEV_POSTFIX = "-maglev-"

# A hook so that after installing a MagLev specific gem, the name of
# which differs from the actual gem's name, we create a symlink, so
# Rubygems' require works
Gem.post_install do |installer|
  if installer.spec.maglev_specific_gem?
    spec = installer.spec
    real_gem_dir = installer.gem_dir.reverse.
      sub(spec.maglev_real_name.reverse, spec.name.reverse).reverse
    if File.exist? real_gem_dir
      case File.ftype(real_gem_dir)
      when "directory"
        FileUtils.rm_rf real_gem_dir
      when "link"
        File.delete(real_gem_dir)
      else
        return false
      end
    end
    File.symlink(installer.gem_dir, real_gem_dir)
  end
end

# Override the name accessors of both Gem::Dependency and Gem::Specification.
# This allows us to publish Gems with the MAGLEV_POSTFIX, but have them report
# their names as the names of the gems they are replacing. This means that
# both Bundler and Rubygems should see XYZ-maglev- gems as just XYZ gems.
#
# This patch also modifies the sort-logic for Gems, so that -maglev- gems are
# preferred.

class Gem::Dependency
  # MagLev specific gems should match in addition to the standard gems
  def name
    return @name if @name.nil?
    return @__patched_name if @__patched_name && @name == @__patched_name

    if @name.end_with?(Gem::MAGLEV_POSTFIX)
      @__patched_name = @name[0...-Gem::MAGLEV_POSTFIX.size]
    else
      @__patched_name = @name.dup
    end

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
  def maglev_specific_gem?
    @name != @__patched_name
  end

  def maglev_real_name
    @name.dup
  end

  def name
    return @name if @name.nil?
    return @__patched_name if @__patched_name && @name == @__patched_name

    if @name.end_with?(Gem::MAGLEV_POSTFIX)
      @__patched_name = @name[0...-Gem::MAGLEV_POSTFIX.size]
    else
      @__patched_name = @name.dup
    end

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
