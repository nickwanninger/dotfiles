function fish_prompt

	set -l exit_status $status

	set_color green
	printf (whoami)

	set_color black
	printf " on "

	set_color blue
	printf (prompt_hostname)

	set_color black
	printf " in "

	set_color yellow
	prompt_pwd | sed "s|$HOME|~|g" | tr -d '\n'

	set_color white


	# add a new line at the end
	printf "\n"


	set prompt_symbol "*"
	if [ $exit_status -ne 0 ]
		set_color --bold red
		printf "$exit_status "
		set prompt_symbol "!"
	else

	end

	# print the little arrow thing
	for c in 444 777 fff
		set_color --bold $c
		printf $prompt_symbol
	end

	printf ' '
end

