# This file is loaded after loading primitives has finished.
# It is used to run code in maglev-ruby (like compiling stdlib C extensions)

load File.expand_path('../extensions/openssl.rb', __FILE__)
