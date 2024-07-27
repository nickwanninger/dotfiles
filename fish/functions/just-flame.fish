function just-flame
	sudo perf record -g $argv
	sudo perf script | inferno-collapse-perf | inferno-flamegraph > flamegraph.svg
end
