BASE_DIR = "/abaco1/users/bobw/netflix"

class Reviewer
    @@reviewers ||= Hash.new 675000 #Size for anticipated  450K entries

    def self.find(id)
         if @@reviewers.has_key? id
           return @@reviewers[id]
         else
           @@reviewers [id]= Reviewer.new
         end
    end

    attr_reader :reviews

    def initialize
        @reviews = []
    end

    def self.all_reviewers
        @@reviewers
    end

    def self.review_count
      counter = 0
      @@reviewers.each {|id, reviewer| counter += reviewer.reviews.length}
      counter
    end

end

class Movie
    attr_reader :title, :year, :ratings

    @@movies ||= []
    def self.movies
        load_movies if @@movies.length == 0
        @@movies
    end

    def initialize(id, title, year)
        @id = id
        @title = title
        @year = year
    end

    def filename
        base = @id.to_s
        pad = "mv_"
        (7 - (base.length)).times{pad << "0"}
        pad + base + ".txt"
    end

    def load_reviews
        return if @ratings
        puts filename + " loading..."
        @ratings = [[],[],[],[],[]]
        first = true
        file = File.open(BASE_DIR + "/training_set/" + filename, "r")
        lineno = 1
        file.each_line do |line|
            if first
                first = false
            else
                lineno += 1
                reviewer_id, rating, date = line.strip.split(/,/)
                review = Review.new(reviewer_id.to_i, self, date, rating.to_i)
                @ratings[rating.to_i - 1] << review
            end
            if (lineno % 5000) == 0
              if Maglev.commit_transaction
                puts "."
              else
                Maglev.abort_transaction
                puts filename + " ABORTED at line " + lineno
              end
            end
        end
        file.close
        if Maglev.commit_transaction
          puts "loaded."
        else
          Maglev.abort_transaction
          puts filename + " FAILED!"
        end
    end

    def self.load_movies
      puts "Loading Movies..."
      file = File.open(BASE_DIR + "/movie_titles.txt", "r")
      file.each_line do |line|
            id, year, title = line.strip.split(/,/)
            @@movies[id.to_i] = Movie.new(id.to_i, title, year.to_i)
        end
      file.close
      if Maglev.commit_transaction
        puts "Movies loaded."
      else
        puts "Movies load FAILED!"
        Maglev.abort_transaction
        return
      end
    end

    def all_reviews
        reviews = []
        @ratings.each{|r| r.each{|t| reviews << t}}
        reviews
    end
end

class Review
    attr_reader :reviewer, :movie, :date, :rating
    attr_writer :rating

    def initialize(reviewer_id, movie, date, rating)
        @date = date
        @rating = rating
        @movie = movie
        @reviewer = Reviewer.find(reviewer_id)
        @reviewer.reviews << self
    end

    def inspect
        "<Review #{@rating} #{@movie.title}>"
    end
end
