## Submission Summary

This minor release (v0.2.0) includes several updates to the existing `summarytabl` package. Minor bugs were resolved, and documentation was improved to clarify function descriptions and argument explanations, minimizing the risk of output misinterpretation. Main and internal functions were restructured to enhance code clarity and maintainability, as well as to improve input validation and strengthen error handling. These updates improve the robustness and usability of the package without altering its core functionality.

### Changes from v0.1.0

* Reorganized main and internal functions by separating helper and utility functions, and adding specific input validation routines for each table-specific function.
* Updated and expanded documentation, including clearer function and argument descriptions, improved examples, and a revised vignette.
* Introduced additional unit tests to improve test coverage.
* Two new datasets were added:
	- sdoh: A subset of the 2020 Social Determinants of Health Database.
	- gss: A subset of the 2022 General Social Survey.
* A new function argument called `margins` was added to the `select_group_tbl()` function. It allows users to choose how percentages are calculated.
* A new function argument called `margins` has been added to the `cat_group_tbl()` function. It allows users to choose how percentages are calculated.

## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments
* local macOS arm64, R 4.4.2
* win-builder (devel and release)
