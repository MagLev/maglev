
class BlogClient
  VERSION = "1.1.0"

  def print_blog_entries
    BlogPost.all_posts.each do |p|
      puts "-" * 50
      puts "Title: #{p.title}  (published on #{p.date.strftime("%Y-%m-%d")})"
      puts "    #{p.text}"
    end
  end
end

BlogClient.new.print_blog_entries
