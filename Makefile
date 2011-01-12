# Flags: skip welcome, optimize, run goal (expects arg)
PROLOG = swipl --quiet -O -g

all: trim run_tests

trim:
	# Remove trailing whitespace and such. Not vital.
	- trim_and_clean *.md *.pl **/*.pl

run_tests:
	clear
	$(PROLOG) "[load], ['../crisp/lib/crisp'], crisp, halt"
