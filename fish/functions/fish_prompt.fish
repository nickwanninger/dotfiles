function fish_prompt

	set -l exit_status $status

	set path (basename (pwd))

	if [ (whoami) = $path ]
		set path '~'
	end

	set_color --bold F95858
	printf "%s" (whoami)

	printf " "

	set_color --bold FBBB4D
	printf "%s" $path


	printf " "

	if test $exit_status -ne 0
		set_color --bold purple
		printf "%s " $exit_status
		set_color normal
	end


	set_color 3DC550
	printf '$'

	printf " "



end
