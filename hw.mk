# Makefile for homework testing infrastructure, based on stack and gradescope

# This file prepares a Haskell project for release. It supports the following features:
#
#   literate Haskell explanation of homework problems
#   processing of input via stubbing, formatting and hlint
#   submission time testing, via gradescope and `stack test`
#
# a homework project should have the following form:
#
#  hwXX/
#    hwXX.cabal
#    stack.yaml
#    LICENSE
#    .hlint.yaml
#    src/          -- all homework problems, in lhs
#                  -- as well as helper modules in hs
#    dat/          -- (optional) data files
#    test/         -- grading tests
#    submit.sh     -- submission zip command for students 
#    Makefile  
#
# From this source, the makefile generates two complete versions of the
# project:
#
#    STUB_DIR/hwXX    -- answers removed
#    EXPORT_DIR/hwXX  -- answers removed, test cases removed, lhs->hs, formatted
#
# as well as a zip file to upload to gradescope 
#
#    hwXX_grader.zip -- autograder
#
# The first can be used to make sure that the stubs work with the tests and to 
# use pandoc to generate webpages that describe the homework.
#
# The second is what students should edit to do the assignment. (NOTE: many vs code 
# lenses and formatters do not support lhs, so we give the students hs instead.)
#
# The third uses the test suite to grade the correctness of the assignment.

##################################################################################

# This file defines the following make targets:

#   stub    (generates stub directory in STUB_DIR/hwXX)
#   run     (compiles and runs executable on stub, repo and solution)
#   test    (compiles and runs test cases on stubs and solution)
#   zipfile (generates gradescope tester)
#   export_files  (generates export directory in EXPORT_DIR/hwXX)
#   clean   (removes all generated files)

##################################################################################

# This makefile is configurable. 

# The Makefiles in the homework project should reference this file, and may define 
# any of the following.

# name of the directory and cabal project, e.g. hw01 etc.
HWNAME       ?= hw     

# where to put the (lhs) stubs
STUB_DIR     ?= ../../../hw-stubs

# where to put the (hs) repos for the students
EXPORT_DIR   ?= ../../../hw-repos

# where to find this, and other files relative to hwXX source directory
TOOLS_DIR    ?= ../../../haskelltester

# all lhs source files, with stubbing
# These are files that the students should edit and that contain instructions
# for the homework problems that will be put on the website.
SOURCES      ?= $(wildcard src/*.lhs) 

# any markdown files, not distributed to students but used to generate webpages
MARKDOWN     ?= $(wildcard markdown/*.md)

# files to distribute to students, no stubbing needed
EXTRA        ?= $(wildcard src/*.hs) $(wildcard dat/*)  

CABAL_EXTRA  ?= LICENSE cabal.project CHANGELOG.md stack.yaml .hlint.yaml hie.yaml

# files needed for grading, not distributed to students
TESTSRC      ?= $(wildcard test/*.hs)  

# files that students need to submit, by default, the stubbed lhs files
SUBMIT       ?= $(SOURCES:.lhs=.hs)

# location of repo to make webpage & test
STUB         ?= $(STUB_DIR)/$(HWNAME)

# location of repo to give to students
REPO         ?= $(EXPORT_DIR)/$(HWNAME)

# all files necessary to build the project
PROJECT      ?= $(SOURCES) $(EXTRA) $(CABAL_EXTRA)

STUBFILES    ?= $(addprefix $(STUB)/,$(PROJECT)) $(addprefix $(STUB)/,$(TESTSRC))

EXPORTFILES  ?= $(addprefix $(REPO)/,$(PROJECT:.lhs=.hs))

GS_TESTER_SRC  ?= setup.sh run_autograder ReportError.hs

GRADESCOPE_ZIP ?= $(HWNAME)_grader.zip

echo:  
	@echo STUB_DIR= $(STUB_DIR)
	@echo TESTSRC= $(TESTSRC)
	@echo STUB= $(STUB)
	@echo PROJECT= $(PROJECT)
	@echo STUBFILES= $(STUBFILES)

##################################################################################

# TOOLS
# where is this repo locally?

# BASE_DIR       := $(realpath $(abspath $(dir $(abspath $(lastword $(MAKEFILE_LIST))))../..))
BASE_DIR       := $(realpath $(abspath $(abspath ../..)))
MYDIR          := $(dir $(abspath $(lastword $(MAKEFILE_LIST))))
TOOLS          := $(BASE_DIR)
# TOOLS_DIR      := $(BASE_DIR)

# Aux tools for stubbing out the answers
HSTUB  :=$(TOOLS_DIR)/bin/hstub
HSOLN  :=$(TOOLS_DIR)/bin/hsoln
CSTUB  :=$(TOOLS_DIR)/bin/cstub
YSTUB  :=$(TOOLS_DIR)/bin/ystub
LHS2HS :=$(TOOLS_DIR)/bin/lhs2hs

########## General ############################################################

.PHONY: all clean test 

all: run test export_files zipfile

clean:
	-rm -rf .stack-work $(GRADESCOPE_ZIP)
	-rm -rf $(STUB)
	-rm -rf $(REPO)/*
	-find . -name '*~' -o -name '#.*#' | xargs rm -rf


########## stub ###########################################
# 
# Note: the stub files are still .lhs files so that we can run pandoc on them
# for the website. They also include the testing code so that we can 
# make sure that the tests work for the stubbed code.

$(STUB): 
	@echo ========== Creating $(STUB) ==========
	mkdir -p $(STUB)
	mkdir -p $(STUB)/src
	mkdir -p $(STUB)/test
	mkdir -p $(STUB)/markdown

$(STUB)/src/%.lhs: src/%.lhs
	$(HSTUB) < src/$*.lhs > $(STUB)/src/$*.lhs

stub : $(STUB) $(PROJECT) $(TESTSRC) $(addprefix $(STUB)/,$(SOURCES)) 
	@echo ========== Creating stub version ==========
	if [ -n "$(EXTRA)" ]; then rsync -R $(EXTRA) $(STUB) ; fi
	cp $(HWNAME).cabal $(STUB)
	rsync -R $(CABAL_EXTRA) $(STUB)
	rsync -R $(TESTSRC) $(STUB)
	rsync -R $(MARKDOWN) $(STUB)

########## run ###########################################

run: stub export_files
	@echo ================= running lhs stub ==============
	(cd $(STUB) ; stack run)	
	@echo ================= running hs stub ==============
	(cd $(REPO) ; stack run)	
	@echo ================= running soln ==================
	stack run


########## test ###########################################

test: stub
	@echo ================= testing stub ==================
	(cd $(STUB) ; stack test)	
	@echo ================= testing soln ==================
	stack test


########## export ###########################################
#
# Prepares the assignment for distribution via github private repositories.
#
# Key vars include:
#    SOURCES = Haskell .lhs source files to process
#    EXTRA = non .lhs files to distribute in repo  (.hs files, data, etc.)
#    SUBMIT = list of .hs files that students need to submit
#
# This process converts .lhs in the sources to .hs files and runs the formatter on the 
# output. At the same time, any Main.lhs in the cabal file is updated to Main.hs 
# in the github repo.
#
# This script also creates a script "submit.sh" to use to create a zipfile for submission.


$(REPO) :
	@echo ========== Making student github repo ========
	mkdir -p $(REPO)
	rm -rf $(REPO)/*


$(REPO)/src/%.hs : $(REPO) $(STUB)/src/%.lhs
	@echo ========== Converting literate files ========
	mkdir -p $(REPO)/src
	$(LHS2HS) $(STUB)/src/$*.lhs > $@
	ormolu -m inplace $@

$(REPO)/$(HWNAME).cabal : $(HWNAME).cabal
	$(CSTUB) < $(HWNAME).cabal > $@
	sed -i "" 's/lhs/hs/g' $(REPO)/*.cabal

$(REPO)/submit.sh: 
	echo "!#/bin/bash" > $(REPO)/submit.sh
	echo "zip submit.zip $(SUBMIT)" >& $(REPO)/submit.sh
	chmod u+x $(REPO)/submit.sh

export_files: $(REPO) $(PROJECT) $(REPO)/$(HWNAME).cabal $(REPO)/submit.sh $(addprefix $(REPO)/,$(SOURCES:.lhs=.hs))
	@echo ========== Copying files to student github repo ========
	if [ -n "$(EXTRA)" ]; then rsync -R $(EXTRA) $(REPO) ; fi
	cp $(CABAL_EXTRA) $(REPO)
	$(YSTUB) < stack.yaml > $(REPO)/stack.yaml


########## Gradescope Zipfile ############################################################

# Recipe to construct a zipfile appropriate for testing this assignment via gradescope
# this does not include any of the files that should be submitted by the students

zipfile: $(addprefix $(TOOLS_DIR)/,$(GS_TESTER_SRC)) $(TESTSRC) 
	rm -rf $(GRADESCOPE_ZIP)
	zip -j $(GRADESCOPE_ZIP) $(addprefix $(TOOLS_DIR)/,$(GS_TESTER_SRC))
	zip $(GRADESCOPE_ZIP) $(TESTSRC) $(CABAL_EXTRA) $(HWNAME).cabal

