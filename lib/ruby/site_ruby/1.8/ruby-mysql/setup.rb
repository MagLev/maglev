#!/usr/local/bin/ruby
# $Id: setup.rb,v 1.1 2003/10/21 09:09:55 tommy Exp $
#
# Copyright (C) 2003 TOMITA Masahiro
# tommy@tmtm.org
#

sock = `mysql_config --socket`.chomp
exit 1 if $? != 0

f = File::open("mysql.rb+", "w")
IO::foreach("mysql.rb") do |l|
  f.puts l.gsub(%r|/tmp/mysql\.sock|, sock)
end
f.close
File::rename("mysql.rb", "mysql.rb.org")
File::rename("mysql.rb+", "mysql.rb")
