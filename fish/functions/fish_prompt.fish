
set __fish_git_prompt_show_informative_status

function __fish_prompt
	set -l exit_status $status

	if [ $exit_status -ne 0 ]
		set_color -r red
		printf "["
		printf "%d" $exit_status
		printf "]"
		set_color normal
		printf " "
	end

	set_color (printf "#%.6s" (echo (prompt_hostname) | md5) | tail -c 6)
	printf (prompt_pwd)

	set_color normal
	printf " "
	printf "\$ "
end

function fish_prompt
	set_color green
	printf "%s\n" (prompt_pwd)
	set_color yellow
	printf "%s" (prompt_hostname)
	set_color normal
	printf ": "
end
