function onchange
	switch (uname -s)
		case Darwin
			fswatch -o $argv[1] | xargs -n1 -I{} $argv[2..-1]
		case '*'
			echo "dunno how. implement"
	end
end
