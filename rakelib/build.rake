# Create a MagLev image from base Smalltalk image
#
# Most uses of this file will simply be for the build:image task.
# That creates a fresh MagLev image from the files under:
#
#    $MAGLEV_HOME/src/smalltalk
#
# The other documented tasks are intended for use when editing the files
# under $MAGLEV_HOME/src/smalltalk.
#
# The steps required to create a new MagLev image are:
#
# 1. Create a temporary directory to work in (FILEIN_DIR)
# 2. Copy a virgin Smalltalk stone to bootstrap (NEW_EXTENT)
# 3. Start a stone, and then:
#      A: load the files in src/smalltalk/ruby
#      B: load the files in src/smalltalk/ruby/mcz
# 4. Do a little bit of maintenance on the new image (audit, shrink it).
# 5. Stop the stone
# 6. Copy the newly created extent0.ruby.dbf to its standard location:
#    $MAGLEV_HOME/bin/extent0.ruby.dbf (the previous extent, if there is
#    one, is saved)
#
# The steps above are run on an initial install of MagLev.  The only other
# time you may want to run these steps is if you change any of the files
# under src/smalltalk.
#
# The steps above do NOT affect any currently running or installed stone
# images.  You'll need to either create a new stone:
#
#    rake stone:create[mynewstone]
#
# or you'll need to stop your current stone and copy a new image (will
# destroy any data in the old stone).
#
# ===================================================
# TODO
# ===================================================
#
# 1. We need a way to upgrade a current stone without destroying user data.
#
# 2. Possibly add allprims loading into this file too (currently, it is done
#    the first time the stone is started with "rake <stonename>:start").

unless defined? MAGLEV_VERSION
  namespace :build do
    require 'erb'
    require 'logger'

    FILEIN_DIR       = File.join(MAGLEV_HOME, "fileintmp")
    NEW_EXTENT       = File.join(FILEIN_DIR, 'extent0.ruby.dbf')

    BUILD_DIR        = File.join(MAGLEV_HOME, 'build')
    BUILD_LOG        = File.join(MAGLEV_HOME, 'build_image.log')
    GEM_BIN          = File.join(GEMSTONE, 'bin')
    GEM_CONF         = File.join(FILEIN_DIR, 'fileingem.ruby.conf')
    SMALLTALK_DIR    = File.join(MAGLEV_HOME, 'src', 'smalltalk')
    PACKAGES_DIR     = File.join(MAGLEV_HOME, 'src', 'packages')
    IMAGE_RUBY_DIR   = File.join(SMALLTALK_DIR, 'ruby')
    KEYFILE          = File.join(MAGLEV_HOME, 'etc', 'maglev.demo.key')
    RUBY_EXTENT      = File.join(MAGLEV_HOME, 'bin', 'extent0.ruby.dbf')
    RUBY_EXTENT_SAVE = File.join(MAGLEV_HOME, 'bin', 'extent0.ruby.dbf.save')
    STONE_CONF       = File.join(FILEIN_DIR, 'filein.ruby.conf')
    STONE_LOG        = File.join(FILEIN_DIR, 'stone.log')
    STONE_NAME       = "fileinrubystone"
    SMALLTALK_EXTENT = File.join(GEMSTONE, 'bin', 'extent0.dbf')
    VERBOSE          = true

    directory FILEIN_DIR

    desc "Create a new MagLev image and install in #{RUBY_EXTENT}. Create new maglev stone if it doesn't exist"
    task :maglev => [:logger] do
      Rake::Task['build:image'].invoke && Rake::Task['stone:create'].invoke('maglev')
    end

    desc "Create a new maglev image and install in #{RUBY_EXTENT}"
    task :image => [:logger] do
      if Rake::Task['build:filein'].invoke && Rake::Task['build:packages'].invoke
        log "maglev", "Build Succeeded"
        if File.exist? RUBY_EXTENT
          log "maglev", "Saving previous extent as #{RUBY_EXTENT_SAVE}"
          mv RUBY_EXTENT, RUBY_EXTENT_SAVE
        end

        log "maglev", "Copying new extent to #{RUBY_EXTENT}"
        cp NEW_EXTENT, RUBY_EXTENT
        chmod 0444, RUBY_EXTENT
      else
        log "maglev", "Build failed see #{BUILD_LOG}"
        abort
      end
    end

    desc "Remove #{FILEIN_DIR} directory"
    task :clean do
      files = FileList['*.log', '*.out', 'gem_*_code.log']
      files << 'tpz_commands'
      files << FILEIN_DIR
      rm_rf files
    end

    desc "call clean, then remove bin/extent0*"
    task :clobber => :clean do
      puts "RM: #{RUBY_EXTENT}"
      puts "RM: #{RUBY_EXTENT_SAVE}"
      rm_f RUBY_EXTENT
      rm_f RUBY_EXTENT_SAVE
    end

    task :logger do
      rm_f BUILD_LOG
      $logger        = Logger.new(BUILD_LOG)
      $logger.level  = Logger::DEBUG
      log "logger", "Logging to: #{BUILD_LOG}"
    end

    task :check_dev_env do
      [MAGLEV_HOME, GEMSTONE, IMAGE_RUBY_DIR, BUILD_DIR, PACKAGES_DIR].each do |var|
        raise "#{var} is not a directory" unless File.directory? var
      end
    end

    file NEW_EXTENT => FILEIN_DIR do
      # These should be done as part of creating FILEIN_DIR, but
      # directory tasks can't have blocks...
      Dir.chdir(FILEIN_DIR) do
        cp SMALLTALK_EXTENT, NEW_EXTENT
        chmod 0770, NEW_EXTENT
        cp_template("#{BUILD_DIR}/filein.ruby.conf.erb", STONE_CONF)
        cp "#{BUILD_DIR}/fileingem.ruby.conf", GEM_CONF
      end
    end

    task :setup_env do
      # upgradeDir is also needed by the filein topaz scripts.  When a
      # customers does a 'upgrade' it will be set to $GEMSTONE/upgrade.  For
      # a filein it will be set to the imageDir.
      #
      # The non-traditional camel case env variable names come from the
      # Smalltalk build.  We do not change them because we want to remain as
      # compatible as possible with the original SVN source.
      ENV["upgradeDir"]             = File.join(GEMSTONE, 'upgrade')
      ENV["imageDir"]               = File.join(MAGLEV_HOME, 'src', 'smalltalk')
      ENV["dbfDir"]                 = FILEIN_DIR
      ENV["imageRubyDir"]           = File.join(MAGLEV_HOME, 'src', 'smalltalk', 'ruby')
      ENV["GS_DEBUG_COMPILE_TRACE"] = "1"
      ENV['STONENAME']              = STONE_NAME
      ENV['GEMSTONE_SYS_CONF']      = STONE_CONF
      ENV["GEMSTONE_EXE_CONF"]      = GEM_CONF
    end

    desc "Load the files in #{SMALLTALK_DIR} into the image (starts and stops stone)"
    task :filein => [:setup_env, NEW_EXTENT, :logger] do
      Dir.chdir FILEIN_DIR do
        begin
          abort unless (startstone and fileinruby)
        ensure
          stopstone
        end
      end
    end

    desc "Load the files in #{PACKAGES_DIR} into the image (starts and stops stone)"
    task :packages => [:setup_env, NEW_EXTENT, :logger] do
      Dir.chdir FILEIN_DIR do
        begin
          abort unless (startstone and load_file_tree_dir)
        ensure
          stopstone
        end
      end
    end

    # Equivalent to the old loading of the MagLev-*.mcz / MCZ_DIR
    def load_file_tree_dir
      # No looping in topaz, so generate a script here
      outfile = "#{FILEIN_DIR}/loadfiletree.out"
      log_run("load_file_tree", outfile) do
        run_topaz("load_file_tree", <<-EOS)
        output push #{outfile} only
        set gemstone #{STONE_NAME} 
        input $imageDir/loadfiletree.topaz
        output pop
        exit
      EOS
      end
    end

    # equivalent to fileinruby.pl, but only supports fast builds
    def fileinruby
      outfile = "#{FILEIN_DIR}/fileinruby.out"
      log_run("fileinruby", outfile) do
        run_topaz("fileinruby", <<-EOS)
        output push #{outfile} only
        set gemstone #{STONE_NAME}
        input $imageDir/fileinruby.topaz
        output pop
        exit
      EOS
      end
    end

    # Run a block wrapped in logging and error checking
    def log_run(step_name, logfile="<no logfile>")
      puts
      log(step_name, "Begin: LOG: #{logfile}")
      res = yield
      if res
        log(step_name, "SUCCESS: $?: #{$?.exitstatus} LOG: #{logfile}")
      else
        log(step_name, "FAILURE: $?: #{$?.exitstatus} LOG: #{logfile}", Logger::ERROR)
      end
      log(step_name, "End: #{step_name}")
      res
    end

    def startstone
      logfile = "#{Dir.pwd}/startstone.log"
      cmd = "#{GEM_BIN}/startstone #{STONE_NAME} -l #{STONE_LOG} -e #{STONE_CONF} -z #{STONE_CONF} > #{logfile} 2>&1"
      if log_run("startstone", logfile) { system cmd }
        cmd = "#{GEM_BIN}/waitstone #{STONE_NAME} >> #{logfile} 2>&1"
        log_run("waitstone", logfile) { system cmd }
      else
        false
      end
    end

    def stopstone
      logfile = "#{Dir.pwd}/stopstone.log"
      cmd = "#{GEM_BIN}/stopstone #{STONE_NAME} DataCurator swordfish > #{logfile} 2>&1"
      log_run("stopstone", logfile) { system cmd }
    end

    def run_topaz(step_name, topaz_commands)
      log("run_topaz: #{step_name}", "Begin")
      cmd_file = File.join(Dir.pwd, 'tpz_commands')
      File.open(cmd_file, 'w') { |f| f.write(topaz_commands) }
      run_topaz_file(cmd_file)
    end

    def run_topaz_file(file)
      topaz_cmd = "#{GEM_BIN}/topaz -l -i -e #{GEM_CONF} -z #{GEM_CONF}"
      system "#{topaz_cmd} < #{file} > #{file}.out 2>&1"
    end

    # Given the name of a ERB template, copy it to the destination dir,
    # running it through erb.
    def cp_template(erb_file, dest_file)
      raise "Can't find erb_file #{erb_file}" unless File.exist? erb_file

      File.open(dest_file, "w") do | dest |
        File.open(erb_file) do |src|
          template = ERB.new(src.read)
          dest.write template.result(binding)
        end
      end
    end

    def log(step, msg, level=Logger::INFO)
      puts "==== #{step}: #{msg}" if VERBOSE
      $logger.log(level, msg, step) if $logger
      true
    end
  end
end

# 3. Create version.txt
# 4. Set build date in Globals.rb
#    e. put the image file somewhere in the package and chmod it
#         `rm topazerrors.log`;
#         `chmod 777 ${target}/gemstone/bin ${target}/gemstone/bin/extent0.ruby.dbf`;
#         `mv ${target}/data/pkg${os}/extent/extent0.ruby.dbf ${target}/gemstone/bin`;
#         `chmod 444 ${target}/gemstone/bin/extent0.ruby.dbf`;
#         `chmod 555 ${target}/gemstone/bin`;
#         `rm -rf ${target}/.hg ${target}/.hgignore`;
#         `rm -rf ${target}/data/* ${target}/log/*`;
#         `zip -r9yq ${target}.zip ${target}`;
#
# for packaging a release image:
#   + remove .git*
#   + keyfile
