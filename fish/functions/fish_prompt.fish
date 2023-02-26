function fish_prompt -d Hydro

	# set_color green
	# echo -n (whoami)
	# set_color reset
	# echo -n "@"
	# set_color blue
	# echo -n (hostname)
	# set_color brblack
	# echo -n ":"
	# set_color green
	# echo -n (prompt_pwd)
	
	# set_color green
	# printf '┌ '


	set_color reset
	if [ $SSH_TTY ];
		echo -n (hostname)
		echo -n ' '
	end

	set_color blue

	echo -n (prompt_pwd)


	echo -en '\\n'

	# set_color green
	# printf '└ '
	set_color reset
	set_color brblack
	echo -n '$ '
	set_color reset

end
