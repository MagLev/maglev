# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = 'ubuntu/precise64'

  # Silence the 'stdin: is not a tty' error on Ubuntu - see https://github.com/mitchellh/vagrant/issues/1673
  config.vm.provision :shell,
    inline: "sed -i 's/^mesg n$/tty -s \\&\\& mesg n/g' /root/.profile"

  config.vm.provision :shell,
    inline: <<-SHELL
      DEBIAN_FRONTEND=noninteractive apt-get -y install git libicu-dev
      chown -R vagrant. /opt
      su - vagrant -c '
        git clone https://github.com/maglev/maglev /opt/maglev && \
        cd /opt/maglev && \
        git checkout allen/libmagparse && \
        ./install.sh'
    SHELL

  config.vm.provider :virtualbox do |vb|
    vb.customize [
      'modifyvm', :id,
      '--cpus', '2',
      '--memory', '2048'
    ]
  end

  config.vm.network :forwarded_port,  guest: 22, host: 2222, id: 'ssh'
end
