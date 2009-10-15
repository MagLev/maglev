require File.join(File.dirname(__FILE__), 'stone')

class GlassStone < Stone

  def run_topaz_commands(*commands)
    result = topaz_commands(["run", "System myUserProfile objectNamed: #MCPlatformSupport", "%"])
    if result.last =~ /\[.* UndefinedObject\] nil/
      super(commands)
    else
      super(commands.unshift("MCPlatformSupport autoCommit: false; autoMigrate: false"))
    end
  end
  
  def seaside_bin_directory
    "#{gemstone_installation_directory}/seaside/bin"
  end
end
