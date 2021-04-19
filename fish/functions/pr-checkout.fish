function pr-checkout
  set -l PR_NUM (
    gh api 'repos/:owner/:repo/pulls' |
    jq --raw-output '.[] | "#\(.number) \(.title)"' |
    fzf |
    sed 's/^#\([0-9]\+\).*/\1/'
  )

  if test -n "$PR_NUM"
    gh pr checkout "$PR_NUM"
	end
end
