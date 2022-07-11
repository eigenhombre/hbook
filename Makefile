.PHONY: test

test:
	./test.sh

docker:
	docker build -t hbook .
