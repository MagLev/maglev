require 'ramaze/option/dsl'
require 'ramaze/option/holder'

module Ramaze
  Global = Option::Holder.new

  Option::DSL.new Global do
    o "List of aliases for adapters",
      :adapter_aliases, {
        "mongrel"             => "Mongrel",
        "evented_mongrel"     => "Mongrel",
        "fcgi"                => "Fcgi",
        "thin"                => "Thin",
        "ebb"                 => "Ebb",
        "cgi"                 => "Cgi",
        "lsws"                => "LSWS",
        "scgi"                => "SCGI",
        "webrick"             => "WEBrick",
        "swiftiplied_mongrel" => "Mongrel",
      }

    o "Hash of aliases for caches",
      :cache_aliases, {
        "memory"    => "MemoryCache",
        "yaml"      => "YAMLStoreCache",
        "memcache"  => "MemcachedCache",
        "memcached" => "MemcachedCache",
      }

    o "Disable templates without actions",
      :actionless_templates, true, :cli => true

    o "Set adapter Ramaze will run on.",
      :adapter, :webrick, :cli => @config[:adapter_aliases][1].keys, :short => :a

    o "Set the size of Backtrace shown.",
      :backtrace_size, 10, :cli => 10

    o "Turn benchmarking every request on.",
      :benchmarking, false, :short => :b, :cli => false

    o "Do not log about these requests to static files, values as in Global.ignore",
      :boring, ["/favicon.ico"]

    o "Use this for general caching and as base for Cache.new.",
      :cache, :memory, :cli => [:memory, :memcached, :yaml]

    o "Turn on naive caching of all requests.",
      :cache_all, false, :cli => false

    o "Alternative caches",
      :cache_alternative, {}

    o "Don't cache action if session flash data is present for a request.",
      :no_cache_flash, true, :cli => true

    o "Compile Templates",
      :compile, false, :cli => false

    o "Start Ramaze within an IRB session",
      :console, false, :short => :c

    o "Active contribs",
      :contribs, Set.new

    o "Set default Content-Type for responses",
      :content_type, "text/html"

    o "Set default Accept-Charset for responses. No header set if nil.",
      :accept_charset, nil

    o "All subclasses of Controller are collected here.",
      :controllers, Set.new

    o "Instruction for daemonize, only works with bin/ramaze for now",
      :daemonize, '', :cli => [:start, :stop]

    o "Turn on customized error pages. Use Rack::ShowException otherwise.",
      :error_page, true, :cli => true

    o "Cache actions to filesystem, uses String as directory name or Global.public_root if other truism",
      :file_cache, false, :cli => false

    o "Specify what IP Ramaze will respond to - 0.0.0.0 for all",
      :host, "0.0.0.0", :cli => "0.0.0.0"

    o "Ignore requests to these paths if no file in public_root exists, absolute path or regex",
      :ignore, ["/favicon.ico"]

    o "Body set on ignored paths",
      :ignore_body, "File not found"

    o "Status set on ignored paths",
      :ignore_status, 404

    o "Enable directory listing",
      :list_directories, false

    o "Templating engines to load on startup",
      :load_engines, nil

    o "All paths to controllers are mapped here.",
      :mapping, {}

    o "Activate middleware",
      :middleware, true

    o "For your own modes to decide on",
      :mode, :live, :cli => [:live, :dev, :spec]

    o "The place ramaze was started from, useful mostly for debugging",
      :origin, :main

    o "Specify port",
      :port, 7000, :short => :p, :cli => 7000

    o "Specify directory to serve static files",
      :public_root, "public", :cli => "public"

    o "Prefix for incoming and outgoing links",
      :prefix, '/'

    o "Record all Request objects by assigning a filtering Proc to me.",
      :record, false

    o "root directory of your application (former APPDIR)",
      :root, File.dirname(File.expand_path($0))

    o "Don't wait until all adapter-threads are finished or killed.",
      :run_loose, false, :cli => false

    o "The runner of your application (start.rb)",
      :runner, "/home/manveru/c/ramaze/examples/hello.rb"

    o "The running server will be put here from Adapter.",
      :server, nil

    o "Turn on session for all requests.",
      :sessions, true, :cli => true

    o "Turn on BF/DoS protection for error-responses",
      :shield, false, :cli => false

    o "What signal to trap to call Ramaze::shutdown",
      :shutdown_trap, "SIGINT"

    o "Interval in seconds of the Reloader",
      :sourcereload, 3, :cli => 3

    o "Test before start if adapters will be able to connect",
      :test_connections, true, :cli => true

    o "Specify directory to serve dynamic files",
      :view_root, "view", :cli => "view"
  end
end
