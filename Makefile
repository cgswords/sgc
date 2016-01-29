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
RUNTIME=runtime
#-- Rules --#

# The main point of this file is to run the tests
all : test

# Run the testing on the compiler
test : $(TEST_FILE)
	$(SC) $(TEST_FILE)

copygc : 
	gcc t.s $(RUNTIME)/copygc.c $(RUNTIME)/runtime.c $(RUNTIME)/printer.c $(RUNTIME)/metrics.c

cheney : 
	gcc t.s $(RUNTIME)/cheney.c $(RUNTIME)/runtime.c $(RUNTIME)/printer.c
