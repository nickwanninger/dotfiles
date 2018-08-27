function fish_prompt
	echo -n (whoami)
	set_color $fish_color_cwd
	echo -n ' ' # some space
	echo -n (basename $PWD)
	set_color normal
	echo -n ' = '
end
