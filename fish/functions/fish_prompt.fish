
set __fish_git_prompt_show_informative_status

function fish_prompt

	set -l exit_status $status

	if [ $exit_status -ne 0 ]
		printf "["
		printf "%d" $exit_status
		printf "] "
	end

	if [ (prompt_pwd) != "~" ]
		set_color green
		printf "%s" (prompt_pwd)

		printf "\n"
	end

	set_color yellow
	printf "%s" (prompt_hostname | tr [a-z] [A-Z])
	set_color normal
	printf ": "
end
