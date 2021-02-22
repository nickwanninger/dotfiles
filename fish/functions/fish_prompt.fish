
function fish_prompt

		set "__fish_git_prompt_show_informative_status" 1
		set "__fish_git_prompt_showdirtystate" 1

    set -l status_copy $status

		# set_color '#333333'
		set_color brblack
		# # string repeat -n (tput cols) '-'


		# set_color brblue
		printf "%s" (prompt_pwd)

    if test "$status_copy" -ne 0
				set_color red
				echo -n " $status_copy"
    end

		set_color brblue
		printf " # "
		# printf " > "
		# printf "\$ "

end
