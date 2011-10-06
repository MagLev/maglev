# Create a MagLev image from base Smalltalk image
#
# TODO: Get rid of topaz output during rake runs
#

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

namespace :build do
  require 'erb'
  require 'logger'

  BUILD_DIR      = File.join(MAGLEV_HOME, 'build')
  FILEIN_DIR     = File.join(MAGLEV_HOME, "fileintmp") # TODO: add $$ to name
  GEM_BIN        = File.join(GEMSTONE, 'bin')
  GEM_CONF       = File.join(FILEIN_DIR, 'fileingem.ruby.conf')
  IMAGE_RUBY_DIR = File.join(MAGLEV_HOME, 'src', 'smalltalk', 'ruby')
  KEYFILE        = File.join(MAGLEV_HOME, 'etc', 'maglev.demo.key')
  MCZ_DIR        = File.join(MAGLEV_HOME, 'src', 'smalltalk', 'ruby', 'mcz')
  RUBY_EXTENT    = File.join(MAGLEV_HOME, 'bin', 'extent0.ruby.dbf')
  STONE_CONF     = File.join(FILEIN_DIR, 'filein.ruby.conf')
  STONE_LOG      = File.join(FILEIN_DIR, 'stone.log')
  STONE_NAME     = "fileinruby#{$$}stone"
  VERBOSE        = true

  # desc "Create a new extent0.ruby.dbf from virgin Smalltalk extent0.dbf"
  # task :ruby_extent => [:check_dev_env, :save_ruby_extent, :image] do
  #   cp
  # end

  task :check_dev_env do
    [MAGLEV_HOME, GEMSTONE, IMAGE_RUBY_DIR, BUILD_DIR, MCZ_DIR].each do |var|
      raise "#{var} is not a directory" unless File.directory? var
    end
  end

  task :save_ruby_extent do
    if File.exist? RUBY_EXTENT
      puts "Saving copy of #{RUBY_EXTENT}"
      mv RUBY_EXTENT, "#{RUBY_EXTENT}.save"
    else
      puts "No #{RUBY_EXTENT} to save"
    end
  end

  task :temp_dir do
    puts "Creating new FILEIN_DIR #{FILEIN_DIR}"
    rm_rf FILEIN_DIR
    mkdir FILEIN_DIR
  end

  def setup_env
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

  desc "Create a fresh ruby image"
  task :image => [:check_dev_env, :temp_dir] do
    build_log_name = File.join(FILEIN_DIR, 'build_image.log')
    $logger        = Logger.new(build_log_name)
    $logger.level  = Logger::DEBUG

    puts "Logging to: #{build_log_name}"
    log("build:image", "Begin task")

    setup_env
    create_filein_stone_files

    # Pass one includes a shrink stone, so we only run it
    # and logout to ensure image written ok
    Dir.chdir FILEIN_DIR do
      begin
        # Pass one runs the filein of ruby base code, then shrinks the
        # image.  We stop at that point to ensure write of shrunk image
        log("build:image", "Begin initial fileinruby")
        startstone && fileinruby && stopstone &&
          # loadmcz on shrunk image => restart stone
          startstone && load_mcz_dir && puts("===== SUCCESS =====")
      ensure
        stopstone
      end
    end
  end

  def load_mcz_dir
    # No looping in topaz, so generate a script here
    files = Dir["#{MCZ_DIR}/*.gs"].sort_by {|a| a.split('_').last }
    inputs = files.map{ |fn| "input #{fn}\n" }

    outfile = "#{FILEIN_DIR}/loadmczdir.out"
    log_run("load_mcz_dir", outfile) do
      run_topaz("load_mcz_dir", <<-EOS)
        output push #{outfile} only
        iferr 1 exit 3
        set gemstone #{STONE_NAME} user DataCurator pass swordfish
        login
        #{inputs}
        expectvalue true
        commit
        logout
        exit 0
      EOS
    end
  end

  # equivalent to fileinruby.pl, but only supports fast
  #
  # fileinruby creates a MagLev ruby image from a virgin Smalltalk image.
  # This involves:
  #
  #   1. Start a virgin smalltalk image
  #   2. Load the MagLev files from src/smalltalk/ruby
  #   3. Load the code from the MagLev-*.mcz file
  #   4. Save the image file as extent0.ruby.dbf.
  #
  # This step needs to be run anytime you change the files in image/ruby.
  #
  # All work is done in filein_tmp_dir.  Config files and the initial image
  # are copied there.
  #
  # $GEMSTONE should be the VM base directory.
  #
  # We will use $GEMSTONE/bin/extent0.dbf as the base smalltalk image upon
  # which we build the ruby image.
  #
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

  def loadmcz
    log_run("loadmcz") do
      tpz_cmds = "load_mcz.topaz"
      cp_template("#{BUILD_DIR}/load_mcz.topaz.erb", tpz_cmds)
      run_topaz_file(tpz_cmds)
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
    log_run("startstone", logfile) { system cmd }
    cmd = "#{GEM_BIN}/waitstone #{STONE_NAME} >> #{logfile} 2>&1"
    log_run("waitstone", logfile) { system cmd }
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

  def create_filein_stone_files
    log("create_filein_stone_files",  "Begin")
    Dir.chdir(FILEIN_DIR) do

      cp File.join(GEMSTONE, 'bin', 'extent0.dbf'), 'extent0.ruby.dbf'
      chmod 0770,'extent0.ruby.dbf'
      cp_template("#{BUILD_DIR}/filein.ruby.conf.erb", STONE_CONF)
      cp "#{BUILD_DIR}/fileingem.ruby.conf", GEM_CONF

      # TODO: Remove this when smalltalk bug resovled.
      # Workaround for bug in Smalltalk build.  patchMaster30.gs should be shipped
      # with the VM, but currently isn't.  Until bug is fixed, we'll copy the file
      # into place here:
      log("create_filein_stone_files", "WORKAROUND: copy patchMaster30.gs to $upgradeDir", Logger::WARN)
      cp File.join(MAGLEV_HOME, 'src', 'smalltalk', 'patchMaster30.gs'), File.join(GEMSTONE, 'upgrade')
    end
    log("create_filein_stone_files",  "End")
  end

  def log(step, msg, level=Logger::INFO)
    puts "==== #{step}: #{msg}" if VERBOSE
    $logger.log(level, msg, step)
    true
  end
end
