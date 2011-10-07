# This works around issue #36
#
# We evaluate rescue clauses too early. In Rails,
# specifically activesupport/lib/active_support/values/time_zone.rb,
# there is a rescue clause referencing TZInfo::InvalidTimezoneIdentifier
# where tzinfo is only lazily loaded after the begin-statement.
# By commiting am empty class to the stone, we work around our
# undefined constant error in that case.

module TZInfo
  class InvalidTimezoneIdentifier < StandardError
  end
end

