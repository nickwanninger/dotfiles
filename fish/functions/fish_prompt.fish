function fish_prompt
  set last_status $status

  if [ "$TERM" = "dumb" ]
    printf "> "
    return
  end

	
	# set_color green
	# printf '┌ '

	set -g __fish_git_prompt_show_informative_status 1


  if [ $last_status -ne "0" ]
    set_color red
    echo -n '■ '
  end

  set_color 14ff6e
  echo -n (hostname)
  echo -n ' '
  set_color brblack

  echo -n (prompt_pwd)
  # echo -n (path basename $PWD)

  set_color reset
  echo -n ' '

end
