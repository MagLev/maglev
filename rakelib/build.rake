# Create a MagLev image from base Smalltalk image
#
# TODO:
#  1. Get a clean run of fileinruby
#  2. Get output from fileinruby going to a known and reported location
#  3. Report the success/failure of fileinruby
#
#  4. Get clean run of loadmcz
#  5. Get output from loadmcz going to a known and reported location
#  6. Report the success/failure of loadmcz
#
#  7. Get clean run of allprims
#  8. Get output from allprims going to a known and reported location
#  9. Report the success/failure of allprims
#
namespace :build do
  require 'erb'
  require 'logger'

  # Assume MAGLEV_HOME is already set.

  # Note: we only do the fast versions of the build for MagLev (no slow vm
  # available), so the task names do not include "fast" and "slow".

  BASE_DIR       = File.expand_path("..", File.dirname(__FILE__))
  IMAGE_DIR      = File.join(BASE_DIR, 'src', 'smalltalk')
  IMAGE_RUBY_DIR = File.join(IMAGE_DIR, 'ruby')
  BUILD_DIR      = File.join(BASE_DIR, 'build')
  KEYFILE        = File.join(BASE_DIR, 'etc', 'maglev.demo.key')
  ST_IMAGE       = File.join(GEMSTONE, 'bin', 'extent0.dbf')

  VERBOSE        = true

  task :check do
    [BASE_DIR, IMAGE_DIR, IMAGE_RUBY_DIR, BUILD_DIR, KEYFILE, ST_IMAGE].each do |f|
      raise "Can't find #{f}" unless File.exist? f
    end
    raise "GEMSTONE not set" unless defined?(GEMSTONE)
    raise "GEMSTONE '#{GEMSTONE}' is not a directory" unless File.directory? GEMSTONE
  end

  desc "Create a fresh ruby image"
  task :image => :check do
    filein_dir     = File.join(MAGLEV_HOME, "fileintmp") # TODO: add $$ to name

    rm_rf filein_dir
    mkdir filein_dir

    build_log_name = File.join(filein_dir, 'build_image.log')
    $logger        = Logger.new(build_log_name)
    $logger.level  = Logger::DEBUG

    puts "Logging to: #{build_log_name}"
    log("build:image", "Begin task")

    options = {
      :filein_dir  => filein_dir,
      :stone_name  => "fileinruby#{$$}stone",
      :stone_conf  => File.join(filein_dir, 'filein.ruby.conf'),
      :stone_log   => File.join(filein_dir, 'stone.log'),
      :gem_conf    => File.join(filein_dir, 'fileingem.ruby.conf'),
      :gem_bin     => File.join(GEMSTONE, 'bin'),
      :mcz_version => 'TimFelgentreff.1307',  # TODO: Parameterize
    }
    options.each_pair { |k,v| log("build:image", "fileinruby:[info] #{k} = #{v}") }

    # upgradeDir is also needed by the filein topaz scripts.  When a
    # customers does a 'upgrade' it will be set to $GEMSTONE/upgrade.  For
    # a filein it will be set to the imageDir.
    ENV["upgradeDir"]             = IMAGE_RUBY_DIR
    ENV["imageDir"]               = IMAGE_DIR
    ENV["dbfDir"]                 = options[:filein_dir]
    ENV["GS_DEBUG_COMPILE_TRACE"] = "1"
    ENV['STONENAME']              = options[:stone_name]
    ENV['GEMSTONE_SYS_CONF']      = options[:stone_conf]
    ENV["GEMSTONE_EXE_CONF"]      = options[:gem_conf]

    create_filein_stone_files options

    # Pass one includes a shrink stone, so we only run it
    # and logout to ensure image written ok
    success = false
    Dir.chdir(options[:filein_dir]) do
      begin
        startstone(options) &&
          waitstone(options) &&
          (success = fileinruby(options))
      ensure
        stopstone(options)
      end
    end

    # Pass two loads mcz and runs allprims
    Dir.chdir(options[:filein_dir]) do
      begin
        startstone(options) &&
          waitstone( options) &&
          loadmcz(options) &&
          allprims(options)
      ensure
        stopstone(options)
      end
    end if success

    # TODO: clean filein_tmp_dir
    # TODO: Report Success/Failure
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
  def fileinruby(options)
    outfile = "#{options[:filein_dir]}/fileinruby.out"
    safe_run("fileinruby", outfile) do
      run_topaz(<<-EOS, options)
output push #{outfile} only
set gemstone #{options[:stone_name]}
input #{IMAGE_DIR}/fileinruby.topaz
output pop
exit
      EOS
    end
  end

  # Assume stone is running and we PWD is correct
  def loadmcz(options) # todo: parameterize
    #    log "Begin: loadmcz"
    safe_run("loadmcz") do
      tpz_cmds = "load_mcz.topaz"
      cp_template("#{BUILD_DIR}/load_mcz.topaz.erb", tpz_cmds, options)
      run_topaz_file(tpz_cmds, options)
    end
    # log "End: loadmcz"
  end

  # Run a block wrapped in logging and error checking
  def safe_run(step_name, logfile="<no logfile>")
    log(step_name, "Begin: LOG: #{logfile}")
    res = yield
    if res
      log(step_name, "Success: $?: #{$?.exitstatus} LOG: #{logfile}")
    else
      log(step_name, "FAILURE: $?: #{$?.exitstatus} LOG: #{logfile}", Logger::ERROR)
    end
    log(step_name, "End: #{step_name}")
    res
  end

  def startstone(opts)
    #log "Begin: startstone"
    logfile = "#{Dir.pwd}/startstone.log"
    cmd = "#{opts[:gem_bin]}/startstone #{opts[:stone_name]} -l #{opts[:stone_log]} -e #{opts[:stone_conf]} -z #{opts[:stone_conf]} > #{logfile} 2>&1"

    safe_run("startstone", logfile) { system cmd }
    #puts "Starting stone: #{cmd}"
    #v = system cmd
    #log "End: loadmcz"
    #v
  end

  def waitstone(opts)
    logfile = "#{Dir.pwd}/waitstone.log"
    # log "Begin: waitstone"
    cmd = "#{opts[:gem_bin]}/waitstone #{opts[:stone_name]} > #{logfile} 2>&1"
    safe_run("waitstone", logfile) { system cmd }
    # v = system cmd
    # log "End: waitstone"
    # v
  end

  def stopstone(opts)
    #log "Begin: stopstone"
    logfile = "#{Dir.pwd}/waitstone.log"
    cmd = "#{opts[:gem_bin]}/stopstone #{opts[:stone_name]} DataCurator swordfish > #{logfile} 2>&1"
    safe_run("stopstone", logfile) { system cmd }
    #v = system cmd
    #log "End: stopstone"
    #v
  end

  def run_topaz(topaz_commands, opts)
    log("run_topaz", "Begin")
    cmd_file = File.join(Dir.pwd, 'tpz_commands')
    File.open(cmd_file, 'w') { |f| f.write(topaz_commands) }
    run_topaz_file(cmd_file, opts)
  end

  def run_topaz_file(file, opts)
    topaz_cmd = "#{opts[:gem_bin]}/topaz -l -i -e #{opts[:gem_conf]} -z #{opts[:gem_conf]}"
    system "#{topaz_cmd} < #{file}"
#    log "run_topaz_file returns: #{v} ($?: #{$?.exitstatus})"
#    v
  end

  # Given the name of a ERB template, copy it to the destination dir,
  # running it through erb.
  def cp_template(erb_file, dest_file, options)
    raise "Can't find erb_file #{erb_file}" unless File.exist? erb_file

    File.open(dest_file, "w") do | dest |
      File.open(erb_file) do |src|
        template = ERB.new(src.read)
        dest.write template.result(binding)
      end
    end
  end

  # 1. setup and verify $GEMSTONE
  # 2. Ensure we can find a base smalltalk image
  # 3. Create version.txt
  # 4. Set build date in Globals.rb

  # 5. create extent0.ruby.dbf
  # 6. Load mcz into extent0.ruby.dbf
  #    a. start stone
  #    b. load_mcz()
  #    c. allprims()
  #    d. stop stone
  #    e. put the image file somewhere in the package and chmod it
  #         `rm topazerrors.log`;
  #         `chmod 777 ${target}/gemstone/bin ${target}/gemstone/bin/extent0.ruby.dbf`;
  #         `mv ${target}/data/pkg${os}/extent/extent0.ruby.dbf ${target}/gemstone/bin`;
  #         `chmod 444 ${target}/gemstone/bin/extent0.ruby.dbf`;
  #         `chmod 555 ${target}/gemstone/bin`;
  #         `rm -rf ${target}/.hg ${target}/.hgignore`;
  #         `rm -rf ${target}/data/* ${target}/log/*`;
  #         `zip -r9yq ${target}.zip ${target}`;

  # for packaging a release image:
  #   + remove .git*
  #   + keyfile

  def allprims()
    log("allprims",  "Begin")
    #        `$gemstone/bin/topaz -l <<EOF
    #        output append $build_log
    #        set gemstone pkg${os} user DataCurator pass swordfish
    #        input $gemstone/upgrade/ruby/allprims.topaz
    #        exit
    #        EOF`;
    log("allprims", "NOT IMPLEMENTED", Logger::ERROR)
    log("allprims",  "End")
  end

  def create_filein_stone_files(options)
    log("create_filein_stone_files",  "Begin")
    Dir.chdir(options[:filein_dir]) do
      cp ST_IMAGE, 'extent0.ruby.dbf'
      chmod 0770,'extent0.ruby.dbf'

      # filein_tmp_dir is the working directory that we put the starting
      # image in, the conf files and log files.  The only useful thing at the
      # end is the extent0.ruby.dbf file, which is our build product.
      cp_template("#{BUILD_DIR}/filein.ruby.conf.erb", options[:stone_conf], options)
      cp "#{BUILD_DIR}/fileingem.ruby.conf", options[:gem_conf]
    end
    log("create_filein_stone_files",  "End")
  end

  def log(step, msg, level=Logger::INFO)
    puts "==== #{msg}" if VERBOSE
    $logger.log(level, msg)
  end
end
