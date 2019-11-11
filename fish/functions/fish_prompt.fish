function last_job_id
    jobs $argv | command awk '/^[0-9]+\t/ { print status = $1 } END { exit !status }'
end

function fish_prompt
    set -l status_copy $status
    set -l pwd_info (pwd_info "/")
    set -l dir
    set -l base
    set -l color (set_color white)
    set -l color2 (set_color normal)
    set -l color3 (set_color $fish_color_command)
    set -l color_error (set_color $fish_color_error)
    set -l color_normal "$color2"


		switch (uname)
			case Darwin
				printf " "
		end

    if test "$status_copy" -ne 0
        set color "$color_error"
        set color2 "$color_error"
        set color3 "$color_error"
    end

    set -l glyph " $color2\$$color_normal"

    if test 0 -eq (id -u "$USER")
        echo -sn "$color_error# $color_normal"
    end

    if test ! -z "$SSH_CLIENT"
        set -l color "$color2"

        if test 0 -eq (id -u "$USER")
            set color "$color_error"
        end

        echo -sn "$color"(host_info "host ")"$color_normal"
    end

    if test "$PWD" = ~
        set base "$color3~"
        set glyph

    else if pwd_is_home
        set dir

    else
        if test "$PWD" = "/"
            set glyph
        else
            set dir "/"
        end

        set base "$color_error/"
    end


		set -l paths
		if set -l git_dir (git rev-parse --show-toplevel 2> /dev/null)
			set git_dir (string replace $HOME '~' $git_dir)
			for x in (string split '/' $git_dir)[1..-2]
				set -a paths (set_color brblack)$x(set_color normal)
			end
			set -a paths (set_color green)(string split '/' $git_dir)[-1](set_color normal)
			set -l git_loc (git rev-parse --show-prefix 2> /dev/null)
			for x in (string split '/' $git_loc)
				set -a paths (set_color brblack)$x(set_color normal)
		end
		else
			set -l dir (string replace $HOME '~' $PWD)
			for x in (string split '/' $dir)
				set -a paths (set_color brblack)$x(set_color normal)
			end
		end
		set -l path (string join '/' $paths)

		echo -sn "$path"

		set_color "#FFA779"
		echo -sn " \$$color_normal "
end
