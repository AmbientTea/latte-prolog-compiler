all:

clean:
	find . -type f -name '*.ll' -delete
	find . -type f -name '*.bc' -delete
	find . -type f -name '*.out' -delete
