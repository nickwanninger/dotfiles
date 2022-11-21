function funcdis
	if test (count $argv) -lt 2
		echo "usage: funcdis <binary> <function>"
	else
		gdb -batch -ex "disassemble $argv[2]" "$argv[1]"
	end
end
