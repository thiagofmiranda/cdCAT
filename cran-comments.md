## Resubmission

This is a resubmission. In response to the previous review I have:

* Explained every acronym on first use in the Description field
  (CD-CAT, DINA, DINO, GDINA, KL, PWKL, MPWKL, SHE, MLE, MAP, EAP).
* Added references describing the implemented methods to the Description
  field in the `authors (year) <doi:...>` form:
  - Cheng (2009) <doi:10.1007/s11336-009-9123-2>
  - de la Torre (2011) <doi:10.1007/s11336-011-9207-7>

## R CMD check results

0 errors | 0 warnings | 1 note (on win-builder R-devel)

The single NOTE is from the "CRAN incoming feasibility" check and contains:

* "New submission" -- expected for a first submission to CRAN.

* "Possibly misspelled words in DESCRIPTION": the flagged tokens are
  method acronyms that are now spelled out on first use in the Description
  (EAP, GDINA, MPWKL, PWKL) and author surnames cited in the references
  (Cheng, Kullback, Leibler, de la Torre). All are spelled correctly.

## Test environments

* Local: Windows 11, R 4.5.1 -- 0 errors | 0 warnings | 3 notes (environmental)
* mac-builder (macOS, R-release) -- Status: OK (0 errors | 0 warnings | 0 notes)
* win-builder (R-devel) -- Status: 1 NOTE (new submission; see above)
* win-builder (R-release, R 4.6.0) -- Status: 1 NOTE (new submission; see above)

## Downstream dependencies

There are currently no downstream dependencies for this package.
