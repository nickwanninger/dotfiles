function plc

	# Check if the user wants to reset the file
	switch "$argv[1]"
		case new
			printf "Clearing the playground\n"
			createplaygroundfile

		case edit
			# check if the playground file exists or not
			# and create it if not
			if [ ! -f /tmp/playground.c ];
				printf "Playground file missing, creating...\n"
				createplaygroundfile
			end
			# open the file in an editor
			eval $EDITOR /tmp/playground.c

		case ir
			clang -S -emit-llvm -O3 -o /tmp/playground.ll /tmp/playground.c
			eval $EDITOR /tmp/playground.ll

		case run
			# compile and run
			gcc -o /tmp/playground /tmp/playground.c
			/tmp/playground
		

		case asm
			# build the asm and show it to the user
			gcc -S -o /tmp/playground.s -masm=intel /tmp/playground.c
			eval $EDITOR /tmp/playground.s
	end
		
end


function createplaygroundfile
	cat ~/dotfiles/.cplaygroundtemplate.c > /tmp/playground.c
end
