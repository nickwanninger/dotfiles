function fish_prompt

	set -l exit_status $status



	set_color "#444444"
	printf (prompt_hostname)
	printf " "
	printf (prompt_pwd)
	printf "\n"

	if [ $exit_status -ne 0 ]
		set_color red
	else
		set_color green
	end

	printf "λ"
	set_color "#555555"
	printf " :: "

end

