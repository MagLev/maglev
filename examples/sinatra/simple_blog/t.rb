p0 = Post.new(:title => "The Title", :text => "Some text")
p1 = Post.get(p0.__id__)
p p0
p p1
p p0.title
p Post.all


4.times do |i|
  Post.new(:title => "The Title for #{i}", :text => "Some text for #{i}")
end
posts = Post.all
titles = posts.map{ |p| puts "====== class: #{p.class}  #{p.title}" }
puts "=== post titles"
p posts.map{ |p| p.title }

t0 = Tag.new("magleviathon")
t1 = Tag.get(t0.__id__)
p t0
p t1
