function fish_prompt
	set prompt_symbol ''
	set -l sudoactive (sudo -n uptime 2>&1 | grep "load" | wc -l)

	if echo $sudoactive | grep 1 >/dev/null
		set_color red
		printf "! "
	end


	switch "$USER"
		case root toor
			set prompt_symbol '#'
		case '*'
			set prompt_symbol '$'
	end

	set_color -o 8BCFF7
	echo -n (basename $PWD)
	set_color normal
	printf ' %s ' $prompt_symbol
end
