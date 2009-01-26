require "optparse"
require "timeout"
require "net/http"

class String
  def /(o) File.join(self, o.to_s) end
end

class RamazeBenchmark

  class BasicWriter
    def initialize
      @ljust = 24
    end

    def write(key, val)
      if key == "Name"
        puts "====== #{val} ======"
      else
        puts((key.to_s + ":").ljust(@ljust) + val.to_s)
      end
    end

    def flush
      puts
      $stdout.flush
    end

    def close; end
  end

  class CSVWriter
    def initialize
      @keys, @vals = [], []
    end

    def write(key, val)
      @keys << key
      @vals << (val =~ /^\d[\d.]+/ ? $& : val)
    end

    def flush
      unless @header
        puts FasterCSV.generate{|csv| csv << @keys }
        @header = true
      end

      puts FasterCSV.generate{|csv| csv << @vals }
      @keys, @vals = [], []
    end

    def close; end
  end

  class GruffWriter
    RPS = "Requests per second"

    def initialize
      @benchmarks = {}
      @order = []
    end

    def write(key, val)
      @benchmark ||= {}
      @benchmark[key] = (val =~ /^\d[\d.]+/ ? $&.to_f : val)
    end

    def flush
      if @benchmark["Requests per second"]
        name = @benchmark["Name"]
        @benchmarks[name] ||= []
        @benchmarks[name] << @benchmark
        @order << name unless @order.include?(name)
      end
      @benchmark = nil
    end

    def close
      g = Gruff::SideBar.new(800)
      g.title = "Ramaze Benchmark"
      setup_gruff_data(g)
      g.labels = gruff_labels
      g.sort = false
      g.hide_legend = true if graph_type == :simple
      g.x_axis_label = "requests/sec"
      g.minimum_value = 0
      g.write
    end

    private

    def gruff_labels
      table = {}
      @order.each_with_index do |bname, idx|
        table[idx] = bname
      end
      return table
    end

    def adapters
      @benchmarks.to_a[0][1].map{|bm| bm["Adapter"]}.uniq
    end

    def paths
      @benchmarks.to_a[0][1].map{|bm| bm["Path"]}.uniq
    end

    def graph_type
      case [adapters.size > 1, paths.size > 1]
      when [true , true ]; :both
      when [true , false]; :adapter
      when [false, true ]; :path
      else :simple; end
    end

    def setup_gruff_data(g)
      case graph_type
      when :simple
        g.data("", @order.map{|name| @benchmarks[name][0][RPS] }, '#6886B4')
      when :adapter
        adapters.each do |adapter|
          g.data(adapter, @order.map{|name|
                   @benchmarks[name].find{|bm| bm["Adapter"] == adapter}[RPS]
                 })
        end
      when :path
        paths.each do |path|
          g.data(path, @order.map{|name|
                   @benchmarks[name].find{|bm| bm["Path"] == path}[RPS]
                 })
        end
      when :both
        adapters.each do |adapter|
          paths.each do |path|
            g.data("#{adapter}, #{path}", @order.map{|name|
                     @benchmarks[name].find{|bm|
                       bm["Adapter"] == adapter && bm["Path"] == path
                     }[RPS]
                   })
          end
        end
      end
    end
  end

  attr_accessor :requests, :adapters, :port, :log, :display_code, :target
  attr_accessor :concurrent, :paths, :benchmarker, :informer, :sessions
  attr_accessor :show_log, :ignored_tags, :formats

  def initialize()
    @adapters = [:webrick]
    @port = rand(32768-1)+32768
    @requests = 100
    @concurrent = 10
    @signal = 'SIGKILL'
    @host = "127.0.0.1"
    @paths = ["/"]
    @target = /.+/
    @informer = true
    @sessions = true
    @ignored_tags = [:debug, :dev]
    @formats = ["text"]
    @writers = []
    yield self
  end

  def start
    # setup writers
    @formats.each do |format|
      case format
      when "csv"  ; @writers << CSVWriter.new
      when "gruff"; @writers << GruffWriter.new
      when "text" ; @writers << BasicWriter.new
      end
    end

    # benchmarks
    __DIR__ = File.expand_path(File.dirname(__FILE__))
    Dir[__DIR__('suite/*.rb')].each do |filename|
      @adapters.each do |adapter|
        @paths.each do |path|
          benchmark(filename, adapter, path) if @target.match(filename)
        end
      end
    end

    # close writers
    @writers.each do |writer|
      writer.close
    end
  end

  # start to measure
  def benchmark(filename, adapter, path)
    # output informations
    l :Name,       filename.scan(/\/([^\/]+)\.rb/).to_s
    l :Adapter,    adapter
    l :Requests,   @requests
    l :Concurrent, @concurrent
    l :Path,       path
    l :Informer,   @informer
    l :Sessions,   @sessions
    if @display_code
      l :Code, "<code ruby>\n#{File.read(filename)}\n</code>\n\n"
    end

    # real benchmark
    ramaze(filename, adapter) do |pid|
      l "Mem usage before", "#{memsize(pid)}MB"
      ab(path).each do |line|
        l *line.split(/:\s*/)
      end
      l "Mem usage after", "#{memsize(pid)}MB"
    end

    # flush writers
    @writers.each do |writer|
      writer.flush
    end
  end

  private

  # memory usage size
  def memsize(pid)
    (`ps -p #{pid} -o rss=`.strip.to_f/10.24).round/100.0
  end

  # output
  def l(key, val)
    @writers.each do |writer|
      writer.write(key.to_s, val)
    end
  end

  # url of ramaze server
  def url(path)
    "http://#{@host}:#{@port}#{path}"
  end

  # apache benchmark
  def ab(path)
    re = /^(Fail|Req|Time|Total transferred|Document Length|Transfer rate)/
    `ab -c #{@concurrent} -n #{@requests} #{url(path)}`.split("\n").grep(re)
  end

  # startup
  def ramaze(filename, adapter)
    pid = fork do
      begin
        require filename
        Ramaze::Log.ignored_tags = @ignored_tags
        if @informer
          unless @show_log
            Ramaze::Log.loggers = [Ramaze::Logging::Logger::Informer.new("/dev/null")]
          end
        else
          Ramaze::Log.loggers = []
        end
        Ramaze::Global.sessions = @sessions
        Ramaze::Global.sourcereload = false
        Ramaze.start :adapter => adapter, :port => @port
      rescue LoadError => ex; l :Error, ex; end
    end

    yield pid if wait_to_startup

    Process.kill(@signal, pid)
    Process.waitpid2(pid)
  end

  # wait for ramaze to start up
  def wait_to_startup
    begin
      Timeout.timeout(5) do
        loop do
          begin
            sleep 1; Net::HTTP.new(@host, @port).head("/"); return true
          rescue Errno::ECONNREFUSED; end
        end
      end
    rescue Timeout::Error
      l "Error", "failed to start benchmark script"; return false
    end
  end
end

Signal.trap(:INT, proc{exit})

RamazeBenchmark.new do |bm|
  OptionParser.new(false, 24, "  ") do |opt|
    opt.on('-a', '--adapters NAMES', '[webrick] Specify adapters') do |adapters|
      bm.adapters = adapters.split(",")
    end

    opt.on('--formats (text|csv|gruff)', '[text] Specify output formats') do |formats|
      bm.formats = formats.split(",")
      bm.formats.each do |format|
        case format
        when "csv"  ; require "fastercsv"
        when "gruff"; require "gruff"
        end
      end
    end

    opt.on('-n', '--requests NUM', '[100] Number of requests') do |n|
      bm.requests = n
    end

    opt.on('-c', '--concurrent NUM', '[10] Number of multiple requests') do |n|
      bm.concurrent = n
    end

    opt.on('--code', 'Display benchmark code') do |n|
      bm.display_code = true
    end

    opt.on('-p', '--port NUM',
           '[random(32768-65535)] Specify port number') do |n|
      bm.port = n
    end

    opt.on('--paths PATHS', '[/] Specify request paths') do |paths|
      bm.paths = paths.split(",")
    end

    opt.on('--no-informer', 'Disable informer') do
      bm.informer = false
    end

    opt.on('--ignored-tags TAGS',
           '[debug,dev] Specify ignored tags for Ramaze::Log') do |tags|
      bm.ignored_tags = tags.split(",").map{|e| e.to_sym }
    end

    opt.on('--show-log', 'Show log') do
      bm.show_log = true
    end

    opt.on('--no-sessions', 'Disable sessions') do
      bm.sessions = false
    end

    opt.on('--target REGEXP',
           '[/.+/] Specify benchmark scripts to measure') do |name|
      bm.target = Regexp.compile(name)
    end

    opt.on('-h', '--help', 'Show this message') do
      puts opt.help
      exit
    end

    begin
      opt.parse!(ARGV)
    rescue OptionParser::ParseError => ex
      puts "[ERROR] " + ex
      puts opt.help
      exit
    end
  end
end.start
