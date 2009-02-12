#          Copyright (c) 2008 Michael Fellinger m.fellinger@gmail.com
# All files in this distribution are subject to the terms of the Ruby license.

module Ramaze

  # High performant source reloader
  #
  # This class acts as Rack middleware.
  #
  # It does not depend on Ramaze itself, but you might have to adjust the
  # Reloader::Hooks module or include your own module to override the hooks.
  # You also might have to set the Log constant.
  #
  # Currently, it uses RInotify if available and falls back to using File.stat.
  #
  # Please note that this will not reload files in the background, it does so
  # only when actively called
  # In case of Ramaze it is performing a check/reload cycle at the start of
  # every request, but also respects a cool down time, during which nothing will
  # be done.
  #
  # After every reload the OPTIONS hash will be checked for changed options and
  # assigned to the instance, so you may change options during the lifetime of
  # your application.
  #
  # A number of hooks will be executed during the reload cycle, see
  # Ramaze::ReloaderHooks for more information.

  class Reloader
    OPTIONS = {
      # At most check every n seconds
      # nil/false will never trigger the reload cycle
      # 0 will cycle on every call
      :cooldown => 2,

      # Compiled files cannot be reloaded during runtime
      :ignore => /\.so$/,

      # Run cycle in a Thread.exclusive, by default no threads are used.
      :thread => false,

      # If you assign a block here it will be instance_evaled instead of
      # calling cycle. This allows you to use for example EventMachine for
      # well performing asynchronous cycling.
      :control => nil, # lambda{ cycle },
    }

# GEMSTONE: We don't currently have Gem working, so just force
# use of the File.stat version
#     begin
#       gem 'RInotify', '>=0.9' # is older version ok?
#       require 'rinotify'
#       require 'ramaze/reloader/watch_inotify'
#       Watcher = WatchInotify
#     rescue LoadError
      # stat always available
      require 'ramaze/reloader/watch_stat'
      Watcher = WatchStat
#    end

    def initialize(app)
      @app = app
      @files = {}
      @watcher = Watcher.new
      options_reload
    end

    def options_reload
      @cooldown, @ignore, @control, @thread =
        OPTIONS.values_at(:cooldown, :ignore, :control, :thread)
    end

    def call(env)
      options_reload

      @watcher.call(@cooldown) do
        if @control
          instance_eval(&@control)
        elsif @thread
          Thread.exclusive{ cycle }
        else
          cycle
        end
      end

      @app.call(env)
    end

    def cycle
      before_cycle

      rotation{|file| @watcher.watch(file) }
      @watcher.changed_files{|f| safe_load(f) }

      after_cycle
    end

    # A safe Kernel::load, issuing the hooks depending on the results
    def safe_load(file)
      before_safe_load(file)
      load(file)
      after_safe_load_succeed(file)
    rescue Object => ex
      Log.error(ex)
      after_safe_load_failed(file, ex)
    end

    def rotation
      files = [$0, __FILE__, *$LOADED_FEATURES].uniq
      paths = ['./', *$LOAD_PATH].uniq

      files.each do |file|
        next if file =~ @ignore
        if not @files.has_key?(file) and path = figure_path(file, paths)
          @files[file] = path
          yield path
        end
      end
    end

    def figure_path(file, paths)
      if Pathname.new(file).absolute?
        return File.exist?(file) ? file : nil
      end

      paths.each do |possible_path|
        full_path = File.join(possible_path, file)
        return full_path if File.exist?(full_path)
      end
      nil
    end


    # Holds hooks that are called before and after #cycle and #safe_load
    module Hooks
      # Overwrite to add actions before the reload rotation is started.
      def before_cycle
      end

      # Overwrite to add actions after the reload rotation has ended.
      def after_cycle
      end

      # Overwrite to add actions before a file is Kernel::load-ed
      def before_safe_load(file)
        Log.debug("reload #{file}")
      end

      # Overwrite to add actions after a file is Kernel::load-ed successfully,
      # by default we clean the Cache for compiled templates and resolved actions.
      def after_safe_load_succeed(file)
        Ramaze::Cache.compiled.clear
        Ramaze::Cache.resolved.clear
        Ramaze::Cache.action_methods.clear
        after_safe_load(file)
      end

      # Overwrite to add custom hook in addition to default Cache cleaning
      def after_safe_load(file)
      end

      # Overwrite to add actions after a file is Kernel::load-ed unsuccessfully,
      # by default we output an error-message with the exception.
      def after_safe_load_failed(file, error)
        Log.error(error)
      end
    end

    include Hooks

  end
end
