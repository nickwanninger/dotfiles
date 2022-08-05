function fish_prompt -d Hydro

	set_color green
	echo -n (whoami)
	set_color reset
	echo -n "@"
	set_color blue
	echo -n (hostname)
	set_color brblack
	echo -n ":"
	set_color green
	echo -n (prompt_pwd)

	set_color brblack
	echo -n '$ '
	set_color reset

	# set_color reset


end
