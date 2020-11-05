function naut -d "builds nautilus"
	make isoimage -j
	qemu-system-x86_64 -curses nautilus.iso
end
