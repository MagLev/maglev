require "rbconfig"

extconf_args = ""
if dir = ENV["OPENSSL_DIR"]
  extconf_args << "--with-openssl-dir=#{dir}"
end

maglev_ruby = File.join(RbConfig::CONFIG["bindir"], "maglev-ruby")
extpath = File.join(RbConfig::CONFIG["rubylibdir"], "openssl", "ext")

Dir.chdir(extpath) do
  unless File.exist?("openssl.#{RbConfig::CONFIG["DLEXT"]}")
    puts "Compiling OpenSSL extension ..."
    if not system("#{maglev_ruby} extconf.rb #{extconf_args} 2>/dev/null >/dev/null")
      puts <<-EOF
        Compiling OpenSSL extension failed. Make sure you have OpenSSL headers installed.
        You can set the environment variable OPENSSL_DIR to point to the header directory.
      EOF
      exit 1
    end
    if not system("make >/dev/null")
      puts <<-EOF
        Compiling OpenSSL extension failed. Make sure you have OpenSSL headers and `make' is in you PATH.
        You can set the environment variable OPENSSL_DIR to point to the header directory.
      EOF
      exit 1
    end
  end
end
