# MagLev does not set the __FILE__ name to "xyzzy" for the duration of the
# instance_eval
o = Object.new
o.instance_eval('raise "Fail" unless __FILE__ == "xyzzy"', 'xyzzy', 1)

#################### Trac Info
# ID:         871
# Summary:    Can't bundle Padrino: MagLev does not set __FILE__ for instance_eval
# Changetime: 2011-03-07 19:05:31+00:00
###

#  $> git clone https://github.com/padrino/padrino-framework
#  $> cd padrino-framework
#  $> bundle
#  The path `/home/jesse/projects/examples/padrino-framework/(eval)/padrino` does not exist.
#  error , a RubySystemExit occurred (error 2752),
#               during /home/jesse/.rvm/gems/maglev-25375/bin/bundle
#  a RubySystemExit occurred (error 2752)
#  