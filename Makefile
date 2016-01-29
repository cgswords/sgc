#----------------------------------------------------------------------
# File Makefile	
# Written by Chris Frisz	
# 	
# Created 10 Jan 2012	
# Last modified 10 Jan 2012	
# 	
# This Makefile is intended for use with CSCI-P423 and runs the the
# load_and_test.ss file. It may be extended to do other things as you
# may need.
#----------------------------------------------------------------------

#-- Variables --#
#SC=scheme
SC=scheme
TEST_FILE=load_and_test.ss

#-- Rules --#

# The main point of this file is to run the tests
all : test

# Run the testing on the compiler
test : $(TEST_FILE)
	$(SC) $(TEST_FILE)

rt2 : 
	gcc -D_XOPEN_SOURCE=700 t.s runtime2/cheney.c runtime2/runtime.c runtime2/printer.c

rt1 :
	gcc -D_GNU_SOURCE -D_XOPEN_SOURCE=700 t.s runtime/runtime.c runtime/printer.c runtime/collector.c runtime/copygc.c runtime/metrics.c
