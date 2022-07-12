.PHONY: test

test:
	./test.sh

test-ecl:
	./test-ecl.sh

docker:
	docker build -t hbook .
