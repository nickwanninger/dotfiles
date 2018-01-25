function mntremote -a remote
  mkdir -p ~/mnt/$remote
  sshfs -o reconnect,ServerAliveInterval=15,ServerAliveCountMax=3,follow_symlinks $remote: ~/mnt/$remote
  echo "Mounted $argv[1] to ~/mnt/$remote"
end
