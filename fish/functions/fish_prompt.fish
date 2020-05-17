function fish_prompt
    set -l status_copy $status

		# switch (uname)
		#   case Darwin
		#      printf "[mac] "
		# end


		echo -n "["
		set_color brblack

		set_color red
		echo -n (whoami)
		set_color brblack
		echo -n "@"

		set_color red
		echo -n (hostname)

		set_color brblack
		echo -n "::"

		set_color cyan
		echo -n (prompt_pwd)

		set_color reset
		echo -n "] "

    if test "$status_copy" -ne 0
				set_color red
				printf "[%d] " $status_copy
		else
			set_color green
			#printf "| "
    end

		printf "\$ "

		set_color normal

end
