function fish_prompt
	set -l exit_status $status

	if [ $exit_status -ne 0 ]
		set_color -r red
		printf "["
		printf "%d" $exit_status
		printf "]"
		set_color reset
		printf " "
	end

	set_color (printf "#%.6s" (echo (prompt_hostname) | md5))
	printf (prompt_pwd)

	set_color "#ffffff"
	printf " # "
end

