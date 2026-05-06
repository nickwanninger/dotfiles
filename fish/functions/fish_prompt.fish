function fish_prompt
  set last_status $status

  if [ "$TERM" = "dumb" ]
    printf "> "
    return
  end

	
	# set_color green
	# printf '┌ '

	set -g __fish_git_prompt_show_informative_status 1


  set main_color "brblack"
  set dot '●'

  if [ $last_status -ne "0" ]
    set main_color red
    set dot '■'
  end
  set_color $main_color

  echo -n "$dot "

  set_color brblack
  # echo -n (hostname)
  # echo -n ' '

  # set_color $main_color
  echo -n (prompt_pwd)
  # echo -n (path basename $PWD)

  set_color reset
  echo -n ' '

end
