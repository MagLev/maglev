require "rbconfig"

maglev_ruby = File.join(RbConfig::CONFIG["bindir"], "maglev-ruby")
extpath = File.join(RbConfig::CONFIG["rubylibdir"], "openssl", "ext")

Dir.chdir(extpath) do
  unless File.exist?("openssl.#{RbConfig::CONFIG["DLEXT"]}")
    puts "Compiling OpenSSL extension ..."
    if not system("#{maglev_ruby} extconf.rb 2>/dev/null >/dev/null")
      raise "compiling OpenSSL extension failed. Make sure you have OpenSSL headers installed"
    end
    if not system("make >/dev/null")
      raise "compiling OpenSSL extension failed. Make sure you have make installed"
    end
  end
end
