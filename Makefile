fmt:
	./scripts/format.bash all .

fmt-since-origin-master:
	./scripts/format.bash origin/master .

clean:
	rm -f vk pk C x w i pf CC
