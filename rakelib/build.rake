# Create a MagLev image from base Smalltalk image
namespace :build do
  # Assume MAGLEV_HOME is already set.

  # Note: we only do the fast versions of the build for MagLev (no slow vm
  # available), so the task names do not include "fast" and "slow".

  BASE_DIR  = File.expand_path("..", File.dirname(__FILE__))
  GS_DIR    = File.join(BASE_DIR, 'src', 'smalltalk', 'ruby')
  BUILD_DIR = File.join(BASE_DIR, 'build')
  KEYFILE   = File.join(BASE_DIR, 'etc', 'maglev.demo.key')

  task :check do
    [BASE_DIR, BUILD_DIR, KEYFILE, GS_DIR].each do |f|
      raise "Can't find #{f}" unless File.exist? f
    end
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

  task :clean do # equivalent to fastrubyclean
  end

  task :private do # equivalent to fastrubyprivate

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
    filein_tmp_dir = File.join(MAGLEV_HOME, "fileintmp#{$$}")
    rm_rf filein_tmp_dir
    mkdir filein_tmp_dir

    # TODO: remove aliasing after first pass
    $out_dir = filein_tmp_dir
    $dbf_dir = filein_tmp_dir # or perhaps filein_tmp_dir/rubyfilein

    image_dir = GS_DIR # TODO: elimianate
    raise "Can't find #{image_dir}" unless File.directory? image_dir

    out_dir = 'NOT IMPLEMENTED'
    keyfile = 'path/to/keyfile'  # todo

    puts "fileinruby:[info] GEMSTONE       = #{ENV['GEMSTONE']}"
    puts "fileinruby:[info] image_dir      = #{image_dir}"
    puts "fileinruby:[info] filein_tmp_dir = #{filein_tmp_dir}"

    ENV["GS_DEBUG_COMPILE_TRACE"] = "1"

    # upgradeDir is also needed by the filein topaz scripts.  When a
    # customers does a 'upgrade' it will be set to $GEMSTONE/upgrade.  For
    # a filein it will be set to the imageDir.
    ENV["upgradeDir"] = image_dir

    ENV['STONENAME'] = "fileinruby#{$$}stone"

    stn_conf = ENV['GEMSTONE_SYS_CONF'] = "#{filein_tmp_dir}/filein.ruby.conf"
    cp_template "#{BUILD_DIR}/filein.ruby.conf.erb", stn_conf


#    gem_conf = "#{filein_tmp_dir}/fileingem.ruby.conf"





    # # cp_template("fileingem.ruby.conf", filein_tmp_dir)
    # $gem_conf =
    # raise "Can't find #{$gem_conf}" unless File.exist? $gem_conf


    # raise "Can't find #{$stn_conf}" unless File.exist? $stn_conf
    # TODO may need to cp templ stn_conf and chmod 770

    # require 'erb'
    # File.open(system_config_filename, "w") do | file |
    #   file.write(ERB.new(config_file_template).result(binding))
    # end
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
