def git_branch
  `git branch`[/^\*\s+(.*)/, 1]
end

namespace :git do
  task :anon do
    sh 'git config remote.origin.url git://github.com/manveru/ramaze'
    puts "You're now accessing ramaze anonymous"
  end

  task :committer do
    sh 'git config remote.origin.url git@github.com:manveru/ramaze'
    puts "You're now accessing ramaze as committer"
  end

  desc 'Update from repository'
  task :update do
    puts "Putting your changes on stash"
    sh 'git stash'

    branch = git_branch
    puts "Current branch is #{branch}"

    if switch = branch != 'master'
      puts "Switching to master branch"
      sh 'git checkout master'
    end

    if switch
      puts "Switching to #{branch} branch"
      sh "git checkout '#{branch}'"
    end

    sh 'git stash apply'
  end

  desc 'Create patch files to send'
  task :send do
    sh 'git format-patch ^HEAD'
  end
end
