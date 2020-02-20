function fish_prompt
    set -l status_copy $status

		switch (uname)
			case Darwin
				printf " "
		end


		set_color brblack
		printf "%s " (prompt_pwd)

    if test "$status_copy" -ne 0
				set_color red
				printf "[%d] " $status_copy
		else
			#set_color green
			#printf "| "
    end

		set_color normal

end
