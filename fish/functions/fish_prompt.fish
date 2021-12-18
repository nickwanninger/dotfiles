function fish_prompt -d Hydro

	set_color brblack
	echo -n '['

	set_color blue
	echo -n (whoami)

	set_color brblack
	echo -n '@'


	set_color yellow
	echo -n (hostname)

	echo -n " "

	set_color green
	echo -n (prompt_pwd)

	set_color brblack
	echo -n ']'

	set_color -o brred

	echo -n ' µ '

	set_color reset



end
