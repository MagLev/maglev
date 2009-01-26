module Ramaze
  class Reloader
    class WatchStat
      def initialize
        # @files[file_path] = stat
        @files = {}
        @last = Time.now
      end

      def call(cooldown)
        if cooldown and Time.now > @last + cooldown
          yield
          @last = Time.now
        end
      end

      # start watching a file for changes
      # true if succeeded, false if failure
      def watch(file)
        return true if watching?(file) # if already watching
        if stat = safe_stat(file)
          @files[file] = stat
        end
      end

      def watching?(file)
        @files.has_key?(file)
      end

      # stop watching a file for changes
      def remove_watch(file)
        @files.delete(file)
      end

      # no need for cleanup
      def close
      end

      # return files changed since last call
      def changed_files
        @files.each do |file, stat|
          if new_stat = safe_stat(file)
            if new_stat.mtime > stat.mtime
              @files[file] = new_stat
              yield(file)
            end
          end
        end
      end

      def safe_stat(file)
        File.stat(file)
      rescue Errno::ENOENT, Errno::ENOTDIR
        nil
      end
    end
  end
end
