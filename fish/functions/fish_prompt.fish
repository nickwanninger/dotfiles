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

	# set -g __fish_git_prompt_show_informative_status 1

	set_color reset
	if [ $SSH_TTY ];
		echo -n (hostname)
		echo -n ' '
	end

	set_color blue

	echo -n (prompt_pwd)

	set_color brblack
	echo -n (fish_git_prompt)


	echo -en '\\n'

	# set_color green
	# printf '└ '
	set_color brblack
	echo -n '$ '
	set_color reset

end
