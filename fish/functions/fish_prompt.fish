
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

		set_color normal
		# show git file changes
		set -l file_changes (git diff --name-only 2> /dev/null | wc -l)
		if [ $file_changes -ne 0 ]
			printf " <%d>" $file_changes
		end

		set -l branch (git rev-parse --abbrev-ref HEAD 2> /dev/null)
		if [ "$branch" != "" ]
			printf " (%s)" "$branch"
		end


		printf "\n"
	end

	set_color yellow
	printf "%s" (prompt_hostname | tr [a-z] [A-Z])
	set_color normal
	printf ": "
end
