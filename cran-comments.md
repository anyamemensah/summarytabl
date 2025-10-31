## Submission Summary

This minor release (v0.2.0) includes several updates to the existing `summarytabl` package. Minor bugs were resolved, and documentation was improved to clarify function descriptions and argument explanations, minimizing the risk of output misinterpretation. Main and internal functions were restructured to enhance code clarity and maintainability, as well as to improve input validation and strengthen error handling. These updates improve the robustness and usability of the package without altering its core functionality.

### Changes from v0.1.0

* Reorganized internal structure by separating helper and utility functions.
* Added input validation for each variable-type-specific function.
* Expanded documentation and examples, revised vignette, and added unit tests.
* Dropped support for `haven_labelled` columns in `cat_` and `select_` functions.
* New or updated features: 
  - datasets
    * `sdoh`: A subset of the 2020 Social Determinants of Health Database.
  - `cat_group_tbl()`
    * New arguments 
      - `margins`: Allows users to specify how percentages are calculated in the output table. This provides greater flexibility in defining whether percentages are based on row totals, column totals, or overall totals.
  - `select_tbl()`
    * New arguments:
      - `var_input`: Allows users to specify whether the values supplied to `var_stem` should be treated as variable stems (`stem`) or as complete 
variable names (`name`). Default is `stem`.
      - `force_pivot`: Allows users to force the output table to pivot to the 'wider' format even when variables have inconsistent value sets.
    * Updated arguments:
      - `var_stem` now accepts
        * A single or multiple value(s) 
        * Either variable stems (e.g., `"dep_"`) or full variable names (e.g., `c("dep_1", "dep_2")`)
      - `escape_stem` has been renamed to `regex_stem`. This argument now explicitly reflects its role in enabling Perl-compatible regular expression matching for variable stems/names.
      - `ignore` now requires a named vector or list to specify which values should be excluded for each variable, or for variables associated with a given stem.
        * Previous usage
          - `select_tbl(data = depressive, var_stem = "dep", ignore = 3)` 
        * Updated usage
          - `select_tbl(data = depressive, var_stem = "dep", ignore = c(dep = 3))`
  - `select_group_tbl()`
    * New arguments:
      - `var_input`: Allows users to specify whether the values supplied to `var_stem` should be treated as variable stems (`stem`) or as complete 
variable names (`name`). Default is `stem`.
      - `margins`: Allows users to specify how percentages are calculated in the output table. This provides greater flexibility in defining whether percentages are based on row totals, column totals, or overall variable totals.
      - `force_pivot`: Allows users to force the output table to pivot to the 'wider' format even when variables have inconsistent value sets.
    * Updated arguments:
      - `var_stem` now accepts
        * A single or multiple value(s) 
        * Either variable stems (e.g., `"dep_"`) or full variable names (e.g., `c("dep_1", "dep_2")`)
      - `escape_stem` has been renamed to `regex_stem`. This argument now explicitly reflects its role in enabling Perl-compatible regular expression matching for variable stems/names.
      - `escape_group` has been renamed to `regex_group`. This argument now explicitly reflects its role in enabling Perl-compatible regular expression matching for grouping variable names/patterns.
  - `mean_tbl()`
    * New arguments:
      - `var_input`: Allows users to specify whether the values supplied to `var_stem` should be treated as variable stems (`stem`) or as complete 
variable names (`name`). Default is `stem`.
    * Updated arguments:
      - `var_stem` now accepts
        * A single or multiple value(s) 
        * Either variable stems (e.g., `"ACS_PCT_AGE"`) or full variable names (e.g., `c("ACS_PCT_AGE_5_9", "ACS_PCT_AGE_10_14")`)
      - `escape_stem` has been renamed to `regex_stem`. This argument now explicitly reflects its role in enabling Perl-compatible regular expression matching for variable stems/names.
      - `ignore` now requires a named vector or list to specify which values should be excluded for each variable, or for variables associated with a given stem.
        * Previous usage
          - `mean_tbl(data = sdoh, var_stem = "ACS_PCT_AGE", ignore = 0)` 
        * Updated usage
          - `mean_tbl(data = sdoh, var_stem = "ACS_PCT_AGE", ignore = c(ACS_PCT_AGE = 0))`
  - `mean_group_tbl()`
    * New arguments:
      - `var_input`: Allows users to specify whether the values supplied to `var_stem` should be treated as variable stems (`stem`) or as complete 
variable names (`name`). Default is `stem`.
    * Updated arguments:
      - `var_stem` now accepts
        * A single or multiple value(s) 
        * Either variable stems (e.g., `"ACS_PCT_AGE"`) or full variable names (e.g., `c("ACS_PCT_AGE_5_9", "ACS_PCT_AGE_10_14")`)
      - `escape_stem` has been renamed to `regex_stem`. This argument now explicitly reflects its role in enabling Perl-compatible regular expression matching for variable stems/names.
      - `escape_group` has been renamed to `regex_group`. This argument now explicitly reflects its role in enabling Perl-compatible regular expression matching for grouping variable names/patterns.
  
## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments
* local macOS arm64, R 4.4.2
* win-builder (devel and release)
