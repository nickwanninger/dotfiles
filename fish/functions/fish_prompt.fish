function fish_prompt
  set last_status $status

  if [ "$TERM" = "dumb" ]
    printf "> "
    return
  end

	
	# set_color green
	# printf '┌ '

	set -g __fish_git_prompt_show_informative_status 1

  set hostname_color "brblack"
  set status_color "brgreen"
  if [ $last_status -ne "0" ]
    set hostname_color yellow
    set status_color red
  end


  set_color $hostname_color
  echo -n (hostname)
  echo -n ' '
  set_color reset


	set_color $status_color
	echo -n (prompt_pwd)

  set_color reset
  echo -n ' '

end
