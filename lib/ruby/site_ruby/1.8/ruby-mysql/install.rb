#!/usr/local/bin/ruby
# $Id: install.rb,v 1.1 2003/10/21 09:09:55 tommy Exp $
#
# Copyright (C) 2003 TOMITA Masahiro
# tommy@tmtm.org
#

require "rbconfig"
dest = Config::CONFIG["rubylibdir"]
fname = dest+"/mysql.rb"
File::open(fname, "w") do |f|
  f.write File::open("mysql.rb").read
end
File::chmod 0644, fname
