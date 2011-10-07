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
require 'rubygems/installer'
require 'fileutils'

module Gem
  # Since maglev-ruby is a shell script, we want the shebang line for shell
  # scripts created by RubyGems that invoke maglev-ruby to use "#!/usr/bin/env maglev-ruby"
  ConfigFile::PLATFORM_DEFAULTS['install'] = '--env-shebang'
  ConfigFile::PLATFORM_DEFAULTS['update']  = '--env-shebang'
end

###########################################################################################
# From here on in, it's all patching around in Rubygems to get both the maglev-gem command
# and bundler to consider -maglev- patched gems before anything else
###########################################################################################

# Remove our -maglev- gemspecs if they exist, otherwise Rubygems will
# keep thinking the Gem is still installed.
Gem.post_uninstall do |uninstaller|
  s = uninstaller.spec
  f = s.spec_file.sub(s.name, "#{s.name}#{Gem::MAGLEV_POSTFIX}")
  File.unlink f if File.exist? f
end

# The postfix we look for to install patched gems released by the MagLev team
Gem::MAGLEV_POSTFIX = "-maglev-"
maglev_platform = "maglev"
def maglev_platform.=~(other)
  if other._isRegexp
    "maglev" =~ other
  else
    "maglev" =~ /#{other}/
  end
end
def maglev_platform.os; "maglev"; end
def maglev_platform.cpu; "maglev"; end
def maglev_platform.version; "maglev"; end
Gem.platforms = Gem.platforms + [maglev_platform]

class Gem::SpecFetcher
  alias_method :original_list, :list
  alias_method :original_fetch_spec, :fetch_spec

  # Bundler only uses this to fetch a list of spec names and then uses
  # it's own search logic. So we have to modify the list of gems this
  # returns
  def list(*args, &block)
    list = original_list(*args, &block)
    list.values.map do |gems|
      gems.map! do |g|
        if g.first.end_with?(Gem::MAGLEV_POSTFIX)
          [g[0][0...-Gem::MAGLEV_POSTFIX.size], g[1], "maglev"]
        else
          g
        end
      end
    end

    list
  end

  # Fetch spec gets its input from the result of #list above, to
  # download the right spec, we have to change the name back to what
  # it was
  def fetch_spec(spec, uri)
    spec = [spec[0] + Gem::MAGLEV_POSTFIX, spec[1], ""] if spec[2] == "maglev"
    original_fetch_spec(spec, uri)
  end
end

class Gem::Format
  class << self
    alias_method :original_from_file_by_path, :from_file_by_path

    # Bundler uses RemoteSpecifications, that we cannot patch easily.
    # So if we are asked to load a file that we cannot find, just try
    # the maglev specific name, too
    def from_file_by_path(path, security_policy = nil)
      begin
        original_from_file_by_path(path, security_policy)
      rescue Gem::Exception => e
        begin
          maglev_path = path.sub(/(\/.*)-([^-]+)\.gem/,
                                 '\1' + Gem::MAGLEV_POSTFIX + '-\2.gem')
          puts "[NOTE] Was asked to install #{File.basename(path)}, but using #{File.basename(maglev_path)} instead"
          original_from_file_by_path(maglev_path, security_policy)
        rescue Gem::Exception
          raise e
        end
      end
    end
  end
end

class Gem::Platform
  class << self
    alias_method :original_new, :new

    # Rubygems doesn't allow simply adding platforms as we go, so we
    # need to patch here to have 'maglev' go through
    def new(arch)
      return arch if arch == "maglev"
      original_new(arch)
    end
  end
end

class Gem::Specification
  class << self
    alias_method :original__load, :_load

    # This is called by Marshal.load when creating the Spec from the
    # Rubygems data. We make sure the name is set to the package we're
    # faking.
    def _load(str)
      spec = original__load(str)
      spec.check_name
      spec
    end
  end

  alias_method :original_full_name, :full_name
  alias_method :original_gem_dir, :gem_dir

  # Helper
  def maglev_specific_gem?
    self.platform == "maglev"
  end

  # Helper. Checks whether the name looks like a maglev override gem
  # and possibly changes the values of @platform, @original_platform
  # and @name
  def check_name
    if @name.end_with?(Gem::MAGLEV_POSTFIX)
      cache_file # memoize cache file
      instance_variable_set("@original_platform", "maglev")
      instance_variable_set("@platform", "maglev")
      instance_variable_set("@name", @name[0...-Gem::MAGLEV_POSTFIX.size])
    end
  end

  # Make sure we check_name before calling. This is neccessary because
  # Gem::Installer will read the spec metadata from the .gem file, and
  # there is no hook into that.
  def gem_dir
    @gem_dir = nil
    check_name
    original_gem_dir
  end

  # Copy this for the original Rubygems behavior. It is used in
  # Bundler to determine the installation path of the gem.
  def full_gem_path
    check_name
    return @full_gem_path if defined?(@full_gem_path) && @full_gem_path
    @full_gem_path = File.expand_path File.join(gems_dir, original_full_name)
    return @full_gem_path if File.directory? @full_gem_path
    @full_gem_path = File.expand_path File.join(gems_dir, original_name)
  end

  # The full_name methods needs to return the actual name of the gem
  # as Rubygems knows it, because all the URIs are built from that
  def full_name
    if maglev_specific_gem?
      "#{@name}#{Gem::MAGLEV_POSTFIX}-#{@version}"
    else
      original_full_name
    end
  end

  # Prioritize MagLev specific gems
  def sort_obj
    [maglev_specific_gem? ? 1 : 0,
     @name,
     @version,
     @new_platform == Gem::Platform::RUBY ? -1 : 1]
  end
end

class Gem::Installer
  alias_method :original_gem_dir, :gem_dir

  # Always bust the memo, this fixes an issue in Bundler
  def gem_dir
    @gem_dir = nil
    original_gem_dir
  end
end
