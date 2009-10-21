# Test lib/ruby/site_ruby/1.8/maglev/ruby_compiler.rb
#
#
require 'maglev/ruby_compiler'
source_string = <<'EOS'
class Who
  def self.knew
    42
  end
end
EOS

RubyCompiler.new.compile(source_string, 'Some Bogus File Name (or other)')
result = Who.knew
raise "Failed: expecting 42 but got: #{result}" unless result == 42


# Now try to compile something with a syntax error:

bad_source = <<EOBS
class Main {
  public static void main(String args[]) {
    System.out.println("Hey...what compiler is this?")   // Syntax error! no semicolon!!
  }
}
EOBS

begin
  RubyCompiler.new.compile(bad_source, 'Some Java File or Other')
rescue SyntaxError
  # OK!
rescue Exception => e
  raise "Failed: Expected SyntaxError on Java code but got #{e.class}: #{e.inspect}"
end

true
