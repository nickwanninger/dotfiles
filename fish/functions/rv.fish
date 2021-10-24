# call a riscv64 toolchain tool. `rv gcc` will call `riscv64-elf-gcc` instead
function rv
	~/chariot/toolchain/local/bin/riscv64-elf-$argv[1] $argv[2..-1]
end
