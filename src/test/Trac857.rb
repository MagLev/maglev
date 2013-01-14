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

$ensure_ary = []

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
        begin
          raise l
        ensure
          $ensure_ary << 44
        end
      rescue NameError => e
        error ||= e
      ensure
        puts "EnsureTwo"
        $ensure_ary << 22
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
        begin
          next
        ensure
          $ensure_ary << 33
        end
      ensure
        puts "EnsureOne"
        $ensure_ary << 11
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

puts "ensure_ary #{$ensure_ary.inspect}"
puts "Flag is #{$flag}"
raise "Failed to run all ensure blocks" unless $ensure_ary == [44, 22, 33, 11]
raise "Failed to set $flag" unless $flag
