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

function fish_right_prompt -d "Write out the right prompt"
  set_color brblack

  set -l duration (math -s0 "$CMD_DURATION / 1000")

  if test "$duration" -gt 2
    if test "$duration" -ge 3600
      printf "%dh %dm %ds" (math -s0 "$duration / 3600") (math -s0 "($duration % 3600) / 60") (math -s0 "$duration % 60")
    else if test "$duration" -ge 60
      printf "%dm %ds" (math -s0 "$duration / 60") (math -s0 "$duration % 60")
    else
      printf "%ds" $duration
    end
  end
  set_color reset


  if test -n "$NIX_STORE"
    set_color -i brblack

    if test -n "$DIRENV_FILE"
      echo -n 'nix::'
      echo -n (basename (dirname $DIRENV_FILE))
    else
      echo -n 'nix'
    end
	  set_color reset
  end

end
