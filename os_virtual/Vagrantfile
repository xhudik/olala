
# -*- mode: ruby -*-
# vi: set ft=ruby :

VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "boxcutter-VAGRANTSLASH-debian8-i386"
  config.ssh.insert_key = false

  config.vm.provider :virtualbox do |v|
    v.name = "webscraping"
    v.memory = 4096
    v.cpus = 4
    v.customize "pre-boot", [
       "storageattach", :id,
       "--storagectl", "IDE Controller",
        "--port", "1",
       "--device", "0",
       "--type", "dvddrive",
       "--medium", "emptydrive",
       ]
    v.customize ["modifyvm", :id, "--boot1", "disk", "--boot2", "dvd"]
  end

  config.vm.hostname = "webscraping"
#  config.vm.network "forwarded_port", guest: 8000, host: 8000
#  config.vm.network "forwarded_port", guest: 8081, host: 8081

  # Set the name of the VM in virtualbox. 
  # See: http://stackoverflow.com/a/17864388/100134
  config.vm.define :webscraping do |webscraping|
  end

  # Ansible provisioner.
  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "provisioning/webscraping_playbook.yml"
    ansible.verbose = "v"
    ansible.sudo = true
  end

end
