require File.join(File.dirname(__FILE__), 'stone')

class GlassStone < Stone
  def initialize_new_stone
    super
    start
    run_topaz_command("UserGlobals at: #BootStrapSymbolDictionaryName put: #UserGlobals. System commitTransaction")
    topaz_commands(["input #{gemstone_installation_directory}/seaside/topaz/installMonticello.topaz", "commit"])
    run_topaz_command("UserGlobals removeKey: #BootStrapSymbolDictionaryName. System commitTransaction")
  end

  def seaside_bin_directory
    "#{gemstone_installation_directory}/seaside/bin"
  end
end
