# Create a MagLev image from base Smalltalk image
namespace :build do
  # Assume MAGLEV_HOME is already set.

  # Note: we only do the fast versions of the build for MagLev (no slow vm
  # available), so the task names do not include "fast" and "slow".

  BASE_DIR  = File.expand_path("..", File.dirname(__FILE__))
  GS_DIR    = File.join(BASE_DIR, 'src', 'smalltalk', 'ruby')
  BUILD_DIR = File.join(BASE_DIR, 'build')
  KEYFILE   = File.join(BASE_DIR, 'etc', 'maglev.demo.key')
  ST_IMAGE  = File.join(GEMSTONE, 'bin', 'extent0.dbf')

  task :check do
    [BASE_DIR, BUILD_DIR, KEYFILE, GS_DIR, ST_IMAGE].each do |f|
      raise "Can't find #{f}" unless File.exist? f
    end
    raise "GEMSTONE not set" unless defined?(GEMSTONE)
    raise "GEMSTONE '#{GEMSTONE}' is not a directory" unless File.directory? GEMSTONE
  end

  desc "Create a fresh ruby image"
  task :image => :check do
    puts "=== task :image"
    fileinruby
    # equivalent to fastrubyimage
    # start stone
    # create filein.ruby.conf
    #
  end

  # equivalent to fileinruby.pl, but only supports fast
  #
  # fileinruby creates a MagLev ruby image from a virgin Smalltalk image.
  # This involves "running" the files in image/ruby on top of a smalltalk
  # image file (extent0.dbf), and saving the file as extent0.ruby.dbf.
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
  #
  def fileinruby
    require 'erb'
    puts "=== fileinruby"

    # filein_tmp_dir is the working directory that we put the starting
    # image in, the conf files and log files.  The only useful thing at the
    # end is the extent0.ruby.dbf file, which is our build product.

    filein_dir = File.join(MAGLEV_HOME, "fileintmp") # TODO: add $$ to name
    options = {
      :filein_dir => filein_dir,
      :stone_name => "fileinruby#{$$}stone",
      :stone_conf => File.join(filein_dir, 'filein.ruby.conf'),
      :stone_log  => File.join(filein_dir, 'stone.log'),
      :gem_conf   => File.join(filein_dir, 'fileingem.ruby.conf'),
      :gem_bin    => File.join(GEMSTONE, 'bin'),
    }
    options.each_pair { |k,v| puts "fileinruby:[info] #{k} = #{v}" }

    rm_rf options[:filein_dir]
    mkdir options[:filein_dir]

    # upgradeDir is also needed by the filein topaz scripts.  When a
    # customers does a 'upgrade' it will be set to $GEMSTONE/upgrade.  For
    # a filein it will be set to the imageDir.
    ENV["upgradeDir"]             = GS_DIR
    ENV["GS_DEBUG_COMPILE_TRACE"] = "1"
    ENV['STONENAME']              = options[:stone_name]
    ENV['GEMSTONE_SYS_CONF']      = options[:stone_conf]

    cp_template("#{BUILD_DIR}/filein.ruby.conf.erb", options[:stone_conf], options)
    cp "#{BUILD_DIR}/fileingem.ruby.conf", options[:gem_conf]

    Dir.chdir(options[:filein_dir]) do
      puts "Now in directory: #{Dir.pwd}"
      cp ST_IMAGE, 'extent0.ruby.dbf'
      chmod 0770,'extent0.ruby.dbf'
      begin
        startstone(options) && waitstone( options)
        #topaz_cmd = "#{options[:gem_bin]}/topaz -l -i -e #{gem_conf} -z #{gem_conf}"
      ensure
        stopstone  options
      end
    end

    # TODO: clean filein_tmp_dir
  end

  def startstone(opts)
    cmd = "#{opts[:gem_bin]}/startstone #{opts[:stone_name]} -l #{opts[:stone_log]} -e #{opts[:stone_conf]} -z #{opts[:stone_conf]}"
    puts "Starting stone: #{cmd}"
    system cmd
  end

  def waitstone(opts)
    cmd = "#{opts[:gem_bin]}/waitstone #{opts[:stone_name]}"
    puts "Waiting for stone: #{cmd}"
    system cmd
  end

  def stopstone(opts)
    cmd = "#{opts[:gem_bin]}/stopstone #{opts[:stone_name]} DataCurator swordfish"
    puts "Stopping stone: #{cmd}"
    system cmd
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


  def load_mcz() # todo: parameterize
    # a. startstone
    # b. run this:
    #        `$gemstone/bin/topaz -l
    build_log = "./build.log"
    stone_name = "buildstone"
    maglev_version = 'todo'           # todo: get the mcz version...

    topaz_cmds =<<-EOS
      set gemstone #{stone_name} user DataCurator pass swordfish
      login
      output append #{build_log}
      display resultcheck
      run
      | httpRepo fileRepo useRepo aBase |
      httpRepo := MCHttpRepository location:'http://seaside.gemstone.com/ss/megaVL'
          user: 'mb' password:'xxxxx'.
      fileRepo := MCDirectoryRepository new directory:
        (FileDirectory on: '#{MAGLEV_HOME}/image/ruby').
      useRepo := fileRepo .

      {
          'MagLev-#{maglev_version}.mcz'

      } do:[ :aName | | ver |
          ver := useRepo loadVersionFromFileNamed: aName .
          ver class == MCVersion ifFalse:[ aName error:'not found in repos' ].
          GsFile gciLogServer: ver printString .
          ver load .
          ver workingCopy repositoryGroup addRepository: httpRepo ;
                              addRepository: fileRepo .
          GsFile gciLogServer: 'load done'
      ].
      ^ true
      %
      commit
      errorCount
      logout
      output pop
      exit
    EOS
  end


  def allprims()
    #        `$gemstone/bin/topaz -l <<EOF
    #        output append $build_log
    #        set gemstone pkg${os} user DataCurator pass swordfish
    #        input $gemstone/upgrade/ruby/allprims.topaz
    #        exit
    #        EOF`;
  end

end
