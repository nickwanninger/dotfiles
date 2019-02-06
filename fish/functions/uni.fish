function uni
	echo $argv[1..-1] | python3 -c 'import unicodedata; print(unicodedata.lookup(input()), end="")' | pbcopy
end
