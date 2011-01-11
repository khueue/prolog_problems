all: trim run_tests

trim:
	# Remove trailing whitespace and such. Not important.
	- trim_and_clean *.md *.pl **/*.pl

run_tests:
	clear
	swipl -g "[load, '../crisp/lib/crisp'], crisp, halt"
