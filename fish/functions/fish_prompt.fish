
function fish_prompt
    set -l status_copy $status

		set_color yellow
		echo (hostname) (prompt_pwd)

    if test "$status_copy" -ne 0
				set_color red
				echo -n "$status_copy "
    end

		set_color reset
		printf "\$ "

end
