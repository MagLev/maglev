# Distilled from Devise + ActiveSupport (const_missing is from
# active_support, devise_for is from Devise).
#
# The key seems to be MagLev doesn't execute the "next" statement in
# devise_for().
#
# This was found in a rails app that mixes in devise:
#
#  $ $MAGLEV_HOME/bin/rake routes
#
$flag = false

class Object
  def self.const_missing(const_name, nesting = nil)
    error = nil

    # If you comment out the .each iteration (and corresponding "end"),
    # then MagLev works fine.
    ["Object"].each do |namespace|
      begin
        xx = self.xyzzy   # Generate a NoMethodError
        return xx
      rescue NoMethodError => l
        raise l
      rescue NameError => e
        error ||= e
      end
    end
    raise error
  end

  def self.load_missing_constant
    self.xyzzy
    22
  end
end

class C

  # This is a slightly more complex environment
  def devise_for
    [:User].each do |sym|
      begin
        xx = self.class.const_get(sym)
      rescue NameError => e
        puts "-- devise_for: NameError: #{e}"
        next
      end
    end
    # MRI comes here via the "next" in the rescue clause.  MagLev does not
    # make it here.
    $flag = true
  end
end

begin
  C.new.devise_for
rescue => e
  puts "FAILED!"
end

puts "Flag is #{$flag}"
raise "Failed to set $flag" unless $flag
