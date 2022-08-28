This repository contains simple scripts and utilities for creating and automatically grading Haskell assignments. 

It is tailored for use with the gradescope tool: https://www.gradescope.com/ but could be adapted for other platforms.

## Creating an assignment for autograding

1. Create an assignment as normally, marking student answers with `{-# SOLN #-}
   yyy {-# STUBWITH xxx #-}`. The `hstub` utitlity will create a version of the
   file that replaces `yyy` with `xxx`.
   
1a. Add the following to your stack.yaml file:


2. Create a `test/Main.hs` module that imports `Tester` from the `gradescope`
   library. Use this library to define the `Problem`s for the assignment.

```haskell
problem0 :: Problem
problem0 = 
  Problem { name = "higher-order operation on lists",
          tests = [(t0a, 3), (t0b, 3), (t0c, 3),
                   (t0d, 3), (t0e, 3)],
          style = 5,
          design = 0,
          requirements = 0,
          testcases = 0
        }
```


   The main method of this module should look something like this:

```haskell
main :: IO ()
main = testerMain [problem0, problem1, problem2, problem3, problem4]
```

3. Give the stubbed versions of the source files to students, but keep your
   tests private. When they submit, compile their versions using your
   `test/Main`.

## Setting up assignments in gradescope

1. "Create programming assignment" in gradescope. If you are lucky you can just
   duplicate an existing assignment from the previous year.

2. Set up the outline with the point values for each problem. 
   (TODO: figure out how to automatically do this via TestMain)

3. Zip up the assignment's `TestMain.hs`, gradescope library, and any other files 
   needed for testing (see `$(TESTSRC)`) using a `make` recipe like this one.

```
# Recipe to construct a zipfile appropriate for testing this assignment via gradescope
# 
HW_DIR         ?= ../hw/$(HWNAME)
TESTER_DIR     ?= ../../haskelltester
GRADESCOPE_SRC ?= gradescope/src/*.hs gradescope/*.cabal gradescope/LICENSE gradescope/CHANGELOG.md
TESTER_SRC     ?= setup.sh run_autograder $(GRADESCOPE_SRC) 
GRADESCOPE_ZIP ?= $(HWNAME)_grader.zip

# Create the zipfile, replacing testerMain (works locally) with gradeScopeMain in the (works on GS) in TestMain module
#
zipfile: $(addprefix $(TESTER_DIR),$(TESTER_SRC)) $(TESTSRC) 
	if [ -a src/TestMain.hs ] ; then sed s/testerMain/gradeScopeMain/ src/TestMain.hs | sed s/TestMain/GradeScopeMain/ > src/GradeScopeMain.hs ; fi
	rm -rf $(GRADESCOPE_ZIP)
	zip $(GRADESCOPE_ZIP) $(TESTSRC) src/GradeScopeMain.hs
	(cd $(TESTER_DIR); zip $(HW_DIR)/$(GRADESCOPE_ZIP) $(TESTER_SRC))
	rm -rf src/GradeScopeMain.hs
```

	 
4. Under "configure autograder" upload the `$(HWNAME)_grader.zip` file
  (Or "Update autograder")
  
  It takes a long time to set up the autograder as it needs to install
  GHC. (TODO: figure out a better way to set up gradescope so that we don't
  have to reinstall GHC with every tester update.)


