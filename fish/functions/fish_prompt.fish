function fish_prompt -d Hydro
    string unescape "$_hydro_color_pwd$hostname $_hydro_color_pwd$_hydro_pwd\x1b[0m $_hydro_prompt\x1b[0m "
end
