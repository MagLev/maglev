# If the we have the data for the US Postal Codes, then we'll use that,
# otherwise we'll generate a tree of random data.

force = ARGV[0] == 'force'
no_data = Maglev::PERSISTENT_ROOT[:kdtree_demo_data].nil?

unless force or no_data
  puts "Data already committed."
  exit 0
end

postal_codes = File.dirname(__FILE__) + '/etc/US.txt'
go_postal = File.exists? postal_codes

Maglev.persistent do
  puts "Committing tree2d.rb"
  load 'tree2d.rb'
  if go_postal
    puts "Comitting postal.rb"
    load 'postal.rb'
  end
end
Maglev.commit_transaction

tree = if go_postal
         puts "Creating tree of US postal zones"
         KDTree::Tree2D.new(PostalCode.parse_file postal_codes)
       else
         puts "Creating tree of #{count} random points"
         MAX_SCALAR = 360.0
         MID_POINT  = 180.0
         points = Array.new(50_000) do |i|
              [rand(MAX_SCALAR) - MID_POINT,
              rand(MAX_SCALAR) - MID_POINT,
              i]
         end
         KDTree::Tree2D.new(points)
       end
Maglev::PERSISTENT_ROOT[:kdtree_demo_data] = tree
Maglev.commit_transaction
puts "Data saved as Maglev::PERSISTENT_ROOT[:kdtree_data]"
