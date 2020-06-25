# Defined in /tmp/fish.H08ti1/gcl.fish @ line 2
function gcl --argument repo
	set -l path (parse-vcs-path git $repo)
	if test ! -d $path
		vcd $repo
		git clone $argv
	end
	vcd $repo
end
