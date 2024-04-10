function fish_prompt

  if [ "$TERM" = "dumb" ]
    printf "> "
    return
  end

  set last_status $status
	
	# set_color green
	# printf '┌ '

	set -g __fish_git_prompt_show_informative_status 1

  set status_color "red"

  if [ $last_status -eq "0" ]
    set status_color "blue"
  end

  set_color $status_color
  echo -n '╭╴'

  if [ ! $last_status -eq "0" ]
    echo -n " $last_status "
  end

  set_color reset
  set_color -i
  echo -n (hostname)
  echo -n ' '
  set_color reset

	set_color blue

	echo -n (prompt_pwd)

  if test -n "$NIX_STORE"
    set_color red
    echo -n ' (nix'

    if test -n "$NDEV_DIR"
	    set_color brblack
      echo -n " in $(basename $NDEV_DIR)"
      set_color red
    end


    echo -n ')'
    # false
  end


	# set_color brblack
	# echo -n (fish_git_prompt)


	echo -en '\\n'

  set_color $status_color
  echo -n '╰╼ '

	set_color reset

end
