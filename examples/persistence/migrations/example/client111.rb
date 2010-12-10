
class BlogClient
  VERSION = "1.1.1"

  def print_blog_entries
    BlogPost.all_posts.each do |p|
      puts "-" * 50
      pub_date = (p.respond_to?(:date) && !p.date.nil?) ? p.date.strftime("%Y-%m-%d") : "Unknown"
      puts "Title: #{p.title}  (published on #{pub_date})"
      puts "    #{p.text}"
    end
  end
end

BlogClient.new.print_blog_entries
