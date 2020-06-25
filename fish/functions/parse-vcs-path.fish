# Defined in /tmp/fish.O3zwfi/parse-vcs-path.fish @ line 2
function parse-vcs-path --description 'echo the path given by remote' --argument vcs remote
	set remote	(echo $remote | sed -E -e 's$^(https|git)?://$$' -e 's|.git$||')
	set parts	(string split -m 1 '/' $remote)  # split on the first / to get the host and path
	set host	$parts[1]

	if test (count $parts) = 1
		# user did not supply a path
		echo $host
		return 0
	end

	set path	$parts[2]
	set host_paths (string split '.' $host)
	set host_paths $host_paths[-1..1]

	set final_path ~/code/vcs/$vcs/(string join '/' $host_paths '@' $path)
	echo $final_path
end
