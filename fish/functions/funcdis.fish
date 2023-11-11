function funcdis
	if test (count $argv) -lt 2
		echo "usage: funcdis <binary> <function>"
	else

		if type -q r2
      echo -e "aa; s sym.$argv[2]; pdf" | r2 $argv[1]
      # r2 -c "aa; s sym.$argv[2]; VVV" $argv[1]
		else
			gdb -batch -ex "disassemble $argv[2]" "$argv[1]"
		end
	end
end
