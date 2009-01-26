module Ramaze
  class Reloader
    # TODO:
    #   * There seems to be a problem somewhere that I couldn't identify yet, a
    #     file has to be modified twice initially to make it show up as
    #     modified here, subsequent changes work just fine.
    #     The only workaround I could find right now would be to read/write
    #     every single file, but that would be unexpected, irresponsible, and
    #     error-prone.
    #
    # NOTE:
    #   * I have changed from using a Mutex to using a Queue, which uses a
    #     Mutex internally.

    class WatchInotify
      POLL_INTERVAL = 2 # seconds
      NOTIFY_MASK = RInotify::MODIFY | RInotify::IN_ONESHOT

      def initialize
        @watcher = RInotify.new
        @changed = Queue.new
        @watcher_thread = start_watcher
      end

      def call(cooldown)
        yield
      end

      # TODO: define a finalizer to cleanup? -- reloader never calls #close

      def start_watcher
        Thread.new{ loop{ watcher_cycle }}
      end

      # Not much work here, we just have to empty the event queue and push the
      # descriptors for reloading on next request.
      def watcher_cycle
        return unless @watcher.wait_for_events(POLL_INTERVAL)

        @watcher.each_event do |event|
          @changed.push(event.watch_descriptor)
        end
      end

      def watch(file)
        return if @watcher.watch_descriptors.has_value?(file)
        return unless File.exist?(file)

        @watcher.add_watch(file, NOTIFY_MASK)
      rescue Errno::ENOENT
        retry
      end

      # FIXME:
      #   Seems like this won't work due to some bug in the rinotify library.
      #   Would be cool if someone could make a FFI version.

      def remove_watch(file)
        @watcher.rm_watch(file)
      end

      def close
        @watcher_thread.terminate
        @watcher.close
        true
      end

      # NOTE:
      #   We have to add the changed file again after we got a notification, I
      #   have no idea why, but using IN_ONESHOT should make sure that there is
      #   no memory leak in the C level even if we add a file again.
      #   There is a memory leak however in the watch_descriptors hash, since
      #   rinotify won't synchronize the contents properly and will only add to
      #   the hash, so we have to clean up ourselves.
      def changed_files
        until @changed.empty?
          descriptor = @changed.shift
          file = @watcher.watch_descriptors.delete(descriptor)
          watch(file)
          yield(file)
        end
      end
    end
  end
end
