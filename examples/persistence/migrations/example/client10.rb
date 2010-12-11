
class BlogClient
  VERSION = "1.0.0"

  def print_blog_entries
    BlogPost.all_posts.each do |p|
      puts "-" * 50
      puts "Title: #{p.title}"
      puts "    #{p.text}"
    end
  end
end

BlogClient.new.print_blog_entries
