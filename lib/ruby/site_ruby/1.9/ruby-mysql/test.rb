# This is script to test for mysql-ruby module.
# $Id: test.rb,v 1.1 2003/07/23 00:42:14 tommy Exp $
#
# Execute in mysql-ruby top directory.
# Modify following $host, $user, $passwd and $db if needed.
#  $host:   hostname mysql running
#  $user:   mysql username (not unix login user)
#  $passwd: mysql access passwd for $user
#  $db:     database name for this test. it must not exist before testing.

require "./mysql.rb"

$host = ARGV.shift
$user = ARGV.shift
$passwd = ARGV.shift
$db = ARGV.shift || "rubytest"

begin
  Dir.glob("t/[0-9]*.rb").sort.each do |f|
    f =~ /^t\/\d+(.*)\.rb$/
    print $1 + "."*(20-$1.length)
    $stdout.flush
    load f
    print "ok\n"
  end
ensure
  if $created
    begin
      Mysql.new($host, $user, $passwd).query("drop database #{$db}")
    rescue
    end
  end
end
