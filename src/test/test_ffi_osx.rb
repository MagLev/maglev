# Misellaneous tests for string (e.g., regressions and random corner cases)
require 'rbconfig'

if RbConfig::CONFIG["host_os"] =~ /darwin/i
  require File.expand_path('simple', File.dirname(__FILE__))
  #     BEGIN TEST CASES
  #
  require 'ffi'
  test(FFI::CLibrary.named('libiconv').__has_symbol('iconv_open'), true,   "finding 'icon_open' in libiconv")
end
