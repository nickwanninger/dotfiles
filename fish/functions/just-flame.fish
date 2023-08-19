function just-flame
	perf record -g $argv
	perf script | inferno-collapse-perf | inferno-flamegraph > flamegraph.svg
end
