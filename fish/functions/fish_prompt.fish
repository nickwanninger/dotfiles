function fish_prompt
    set -l status_copy $status

		# switch (uname)
		#   case Darwin
		#      printf "[mac] "
		# end

		set_color green
		echo -n "["
		echo -n (hostname)
		echo -n " "
		echo -n (prompt_pwd)

		echo -n "]"

		set_color reset

    if test "$status_copy" -ne 0
				set_color red
				printf "[%d] " $status_copy
		else
			# set_color 8a8a8a
			#printf "| "
    end

		printf "\$ "

		# set_color normal

end
