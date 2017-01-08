all:

clean:
	find tests -type f -name '*.ll' -delete
	find tests -type f -name '*.bc' -delete
	find tests -type f -name '*.out' -delete
