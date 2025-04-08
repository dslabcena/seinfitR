## Resubmission
This is a resubmission. In this version I have:

* Expanded the Description field to provide more details about the package functionality and implemented methods.
  (Modified: DESCRIPTION)

* Added references in the Description field in the correct format, including the Seinhorst (1986) reference with DOI.
  (Modified: DESCRIPTION)

* Added \value tags to all exported methods in the documentation, explaining the structure and meaning of the output.
  (Modified: R/methods.R)

* Replaced direct console output (print/cat) with message()/warning() or verbose options where appropriate.
  (Modified: R/seinfitR.R)

* Removed options(scipen=3) to avoid changing user options without proper reset.
  (Modified: R/plot.seinfitR.R)

* Enhanced code clarity and documentation throughout the package.

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
