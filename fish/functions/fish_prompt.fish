function fish_prompt -d Hydro
    string unescape "$_hydro_color_pwd$hostname\x1b[35m : $_hydro_color_pwd$_hydro_pwd\x1b[0m $_hydro_color_git$$_hydro_git\x1b[0m$_hydro_color_duration$_hydro_cmd_duration\n\x1b[0m$_hydro_prompt\x1b[0m "
end
