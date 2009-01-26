require 'tmpdir'

module Ramaze
  def self.daemonize(runner, instruction, options = {})
    puts "Instructing daemonize to: #{instruction}"
    require 'daemons'

    pids = File.join(Dir.tmpdir, 'ramaze.pids')
    FileUtils.mkdir_p(pids)

    # FIXME: there's a much better way, gotta search irc logs.
    unless File.executable?(runner)
      mode = File.stat(runner).mode.to_s(8)
      mode[3,1] = '7'
      File.chmod(mode.to_i(8), runner)
    end

    content = File.readlines(runner)
    content.unshift('#!/usr/bin/env ruby') unless content[0] =~ /#!/
    File.open(runner, 'w+'){|io| io.puts(content) }

    options = {
      :app_name   => File.basename(File.dirname(runner)),
      :ARGV       => [instruction],
      :script     => runner,
      :dir_mode   => :normal,
      :dir        => pids,
      :multiple   => false,
      :ontop      => false,
      :mode       => :exec,
      :backtrace  => true,
      :monitor    => true,
    }.merge(options)

    Daemons.run(runner, options)
  end
end
