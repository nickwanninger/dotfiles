function fish_prompt
    set -l status_copy $status

		# switch (uname)
		#   case Darwin
		#      printf "[mac] "
		# end

		echo -n "["

		# set_color 8a8a8a
		# echo -n (whoami)

		# set_color white
		# echo -n "@"

		set_color 5ad7ff
		echo -n (hostname)

		set_color 8a8a8a
		echo -n " "

		set_color 5fd7af
		# echo -n (basename (prompt_pwd))
		echo -n (prompt_pwd)

		set_color reset
		echo -n "]"

    if test "$status_copy" -ne 0
				set_color red
				printf "[%d] " $status_copy
		else
			set_color 8a8a8a
			#printf "| "
    end

		printf "\$ "

		set_color normal

end
