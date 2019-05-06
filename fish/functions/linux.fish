function linux
	docker run -v /Users/nick/.linux:/mount -w /mount --name debian-(openssl rand -hex 4) --rm -i -t debian bash
end
