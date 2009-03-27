# First, fire up a maglev sinatra server:
#
#     $ maglev-ruby src/external/sinatratest.rb
#
# Then, run this script with either maglev or MRI.

url = 'http://localhost:4567'

# Hash of URI paths => expected results
get_test_cases = {
  '/'                  => 'Hello World',
  '/names/foobar'      => "The name is: foobar",
  '/say/hello/to/fred' => "Say 'hello' to fred",
  '/goto_home'         => '',  # the redirect itself has no content
  '/not_found'         => 'Custom Not Found',
  '/throw_not_found'   => 'Thrown Not Found',
}

get_test_cases.each do |path, expected|
  full_url = url + path
  result = `curl -s #{full_url}`
  #puts "== Trying #{path} => '#{expected}' actual: '#{result}'"
  unless result == expected
    msg = "Failed '#{full_url}' expected '#{expected}' got: '#{result}'"
    fail msg
  end
end
