# summarytabl 0.2.1.9000 (development version)

## Minor improvements

* Revised function argument documentation to correct errors and improve clarity of descriptions.
* In `mean_tbl()`, if a user supplies variable labels and a match is not found (when `var_input` is `name`), the label now defaults to the original variable name instead of `NA`.
* Both `mean_tbl` and `mean_group_tbl` now include the median as a summary statistic.

# summarytabl 0.2.1

* Corrected package dependencies to resolve installation and compatibility issues. This update does not introduce new features or code changes.

# summarytabl 0.2.0

## Breaking Changes

* Functions prefixed with `cat_` and `select_` no longer support columns of the `haven_labelled` type. Users must convert these columns to factors before using these functions. To convert `haven_labelled` columns to factors, consider using `haven::as_factor()` or `labelled::unlabelled()`.

## Minor improvements

### New datasets

* `sdoh`: A subset of the 2020 Social Determinants of Health Database.

### Updates by function

#### `cat_group_tbl()`

Users can now specify how percentages are calculated in the output table using the `margins` argument. This provides greater flexibility in defining whether percentages are based on row totals, column totals, or overall totals.

```r
nlsy_sub <- 
  nlsy |>
  dplyr::mutate(
    gender = ifelse(gender == 1, "male", "female"))


# Default: All
cat_group_tbl(data = nlsy_sub,
              row_var = "gender",
              col_var = "race",
              pivot = "wider",
              only = "percent")

# gender percent_race_Black percent_race_Hispanic `percent_race_Non-Black,Non-Hispanic`
# <chr>               <dbl>                 <dbl>                                 <dbl>
# 1 female            0.146                 0.102                                 0.243
# 2 male              0.146                 0.110                                 0.253

# Margins: Columnwise
cat_group_tbl(data = nlsy_sub,
              row_var = "gender",
              col_var = "race",
              pivot = "wider",
              margins = "columns",
              only = "percent")

# gender   percent_race_Black percent_race_Hispanic `percent_race_Non-Black,Non-Hispanic`
# <chr>                 <dbl>                 <dbl>                                 <dbl>
# 1 female                0.5                 0.483                                 0.490
# 2 male                  0.5                 0.517                                 0.510

# Margins: Rowwise
cat_group_tbl(data = nlsy_sub,
              row_var = "gender",
              col_var = "race",
              pivot = "wider",
              margins = "rows",
              only = "percent")

# gender percent_race_Black percent_race_Hispanic `percent_race_Non-Black,Non-Hispanic`
# <chr>               <dbl>                 <dbl>                                 <dbl>
# 1 female            0.297                 0.208                                 0.495
# 2 male              0.287                 0.215                                 0.498
```

#### `select_tbl()`

`select_tbl()` now supports selecting variables either by stem or by full variable name. Both single and multiple values are accepted. 

Set `var_input = "stem"` (default) when searching for variables using stems. 

```r
select_tbl(data = depressive, var_stem = "dep")

# # A tibble: 24 × 4
# variable values count percent
# <chr>     <int> <int>   <dbl>
# 1 dep_1       1   109  0.0678
# 2 dep_1       2   689  0.429 
# 3 dep_1       3   809  0.503 
# 4 dep_2       1   144  0.0896
# 5 dep_2       2   746  0.464 
# 6 dep_2       3   717  0.446 
```

Set `var_input = "name"` when searching for variables using their name.

```r
select_tbl(data = depressive, 
           var_stem = c("dep_1", "dep_4", "dep_6"),
           var_input = "name")

# # A tibble: 9 × 4
# variable values count percent
# <chr>     <int> <int>   <dbl>
# 1 dep_1       1   117  0.0714
# 2 dep_1       2   703  0.429 
# 3 dep_1       3   818  0.499 
# 4 dep_4       1   608  0.371 
# 5 dep_4       2   854  0.521 
# 6 dep_4       3   176  0.107 
# 7 dep_6       1   398  0.243 
# 8 dep_6       2   872  0.532 
# 9 dep_6       3   368  0.225 
```

Users are now required to use a named vector or list to indicate which values to exclude for each variable or for variables associated with a specific stem.

Previous usage: value `3` is not excluded from analysis
```r
select_tbl(data = depressive,
           var_stem = "dep",
           ignore = 3)

# # A tibble: 24 × 4
# variable values count percent
# <chr>     <int> <int>   <dbl>
# 1 dep_1       1   109  0.0678
# 2 dep_1       2   689  0.429 
# 3 dep_1       3   809  0.503 
# 4 dep_2       1   144  0.0896
# 5 dep_2       2   746  0.464 
# 6 dep_2       3   717  0.446 
# 7 dep_3       1  1162  0.723 
# 8 dep_3       2   392  0.244 
# 9 dep_3       3    53  0.0330
# 10 dep_4      1   601  0.374 
```

Updated usage: value `3` is successfully excluded from analysis

```r
select_tbl(data = depressive,
           var_stem = "dep",
           ignore = c(dep = 3))

# # A tibble: 16 × 4
# variable values count percent
# <chr>     <int> <int>   <dbl>
# 1 dep_1       1    37   0.167
# 2 dep_1       2   185   0.833
# 3 dep_2       1    47   0.212
# 4 dep_2       2   175   0.788
# 5 dep_3       1   133   0.599
# 6 dep_3       2    89   0.401
# 7 dep_4       1   108   0.486
# 8 dep_4       2   114   0.514
```

#### `select_group_tbl()`

`select_group_tbl()` now supports selecting variables either by stem or by full variable name. Both single and multiple values are accepted. 

Set `var_input = "stem"` (default) when searching for variables using stems. 

```r
select_group_tbl(data = depressive,
                 var_stem = "dep",
                 group = "sex")

# # A tibble: 48 × 5
#   variable   sex values count percent
#   <chr>    <int>  <int> <int>   <dbl>
# 1 dep_1        1      1    55  0.0342
# 2 dep_1        1      2   325  0.202 
# 3 dep_1        1      3   440  0.274 
# 4 dep_1        2      1    54  0.0336
# 5 dep_1        2      2   364  0.227 
# 6 dep_1        2      3   369  0.230 
# 7 dep_2        1      1    82  0.0510
```

Set `var_input = "name"` when searching for variables using their name.

```r
select_group_tbl(data = depressive, 
                 var_stem = c("dep_1", "dep_4", "dep_6"),
                 var_input = "name",
                 group = "sex")

# # A tibble: 18 × 5
# variable   sex values count percent
# <chr>    <int>  <int> <int>   <dbl>
# 1 dep_1        1      1    58  0.0354
# 2 dep_1        1      2   332  0.203 
# 3 dep_1        1      3   443  0.270 
# 4 dep_1        2      1    59  0.0360
# 5 dep_1        2      2   371  0.226 
# 6 dep_1        2      3   375  0.229 
# 7 dep_4        1      1   300  0.183 
# 8 dep_4        1      2   436  0.266 
# 9 dep_4        1      3    97  0.0592
# 10 dep_4       2      1   308  0.188 
# 11 dep_4       2      2   418  0.255 
# 12 dep_4       2      3    79  0.0482
```

Users can now specify how percentages are calculated in the output table using the `margins` argument. This provides greater flexibility in defining whether percentages are based on row totals, column totals, or overall variable totals.

```r
tas_recoded <-
  tas |>
  dplyr::mutate(sex = dplyr::case_when(
    sex == 1 ~ "female",
    sex == 2 ~ "male",
    TRUE ~ NA)) |>
  dplyr::mutate(dplyr::across(
    .cols = dplyr::starts_with("involved_"),
    .fns = ~ dplyr::case_when(
      .x == 1 ~ "selected",
      .x == 0 ~ "unselected",
      TRUE ~ NA)
  ))

# Default: All
select_group_tbl(data = tas_recoded,
                 var_stem = "involved_",
                 group = "sex",
                 na_removal = "pairwise",
                 pivot = "wider",
                 only = "percent")

# variable                  values     percent_sex_female percent_sex_male
# <chr>                     <chr>                   <dbl>            <dbl>
# 1 involved_arts           selected               0.0839           0.0740
# 2 involved_arts           unselected             0.395            0.447 

# Margins: Columnwise
select_group_tbl(data = tas_recoded,
                 var_stem = "involved_",
                 group = "sex",
                 na_removal = "pairwise",
                 pivot = "wider",
                 margins = "columns",
                 only = "percent")

# variable                  values     percent_sex_female percent_sex_male
# <chr>                     <chr>                   <dbl>            <dbl>
# 1 involved_arts           selected               0.175            0.142 
# 2 involved_arts           unselected             0.825            0.858 

# Margins: Rowwise
select_group_tbl(data = tas_recoded,
                 var_stem = "involved_",
                 group = "sex",
                 na_removal = "pairwise",
                 pivot = "wider",
                 margins = "rows",
                 only = "percent")

# variable                  values     percent_sex_female percent_sex_male
# <chr>                     <chr>                   <dbl>            <dbl>
# 1 involved_arts           selected                0.531            0.469
# 2 involved_arts           unselected              0.469            0.531
```

#### `mean_tbl()`

`mean_tbl()` now supports selecting variables either by stem or by full variable name. Both single and multiple values are accepted. 

Set `var_input = "stem"` (default) when searching for variables using stems. 

```r
mean_tbl(data = sdoh, var_stem = "HHC_PCT")

# # A tibble: 6 × 6
# variable                    mean    sd   min   max  nobs
# <chr>                      <dbl> <dbl> <dbl> <dbl> <int>
# 1 HHC_PCT_HHA_NURSING       58.2  49.3     0   100  3227
# 2 HHC_PCT_HHA_PHYS_THERAPY  56.7  48.8     0   100  3227
# 3 HHC_PCT_HHA_OCC_THERAPY   52.4  48.3     0   100  3227
# 4 HHC_PCT_HHA_SPEECH        49.1  47.6     0   100  3227
# 5 HHC_PCT_HHA_MEDICAL       42.2  46.2     0   100  3227
# 6 HHC_PCT_HHA_AIDE          55.1  48.6     0   100  3227
```

Set `var_input = "name"` when searching for variables using their name.

```r
mean_tbl(data = sdoh, 
         var_stem = c("HHC_PCT_HHA_NURSING", "HHC_PCT_HHA_AIDE"),
         var_input = "name")

# # A tibble: 2 × 6
# variable              mean    sd   min   max  nobs
# <chr>                 <dbl> <dbl> <dbl> <dbl> <int>
# 1 HHC_PCT_HHA_NURSING  58.2  49.3     0   100  3227
# 2 HHC_PCT_HHA_AIDE     55.1  48.6     0   100  3227
```
Users are now required to use a named vector or list to indicate which values to exclude for each variable or for variables associated with a specific stem.

Previous usage: value `0` is not excluded from analysis

```r
mean_tbl(data = sdoh,
         var_stem = "HHC_PCT",
         ignore = 0)

# # A tibble: 6 × 6
# variable                    mean    sd   min   max  nobs
# <chr>                      <dbl> <dbl> <dbl> <dbl> <int>
# 1 HHC_PCT_HHA_NURSING       58.2  49.3     0   100  3227
# 2 HHC_PCT_HHA_PHYS_THERAPY  56.7  48.8     0   100  3227
# 3 HHC_PCT_HHA_OCC_THERAPY   52.4  48.3     0   100  3227
# 4 HHC_PCT_HHA_SPEECH        49.1  47.6     0   100  3227
# 5 HHC_PCT_HHA_MEDICAL       42.2  46.2     0   100  3227
# 6 HHC_PCT_HHA_AIDE          55.1  48.6     0   100  3227 
```

Updated usage: value `0` is successfully excluded from analysis

```r
mean_tbl(data = sdoh,
         var_stem = "HHC_PCT",
         ignore = c(HHC_PCT = 0))

# # A tibble: 6 × 6
# variable                  mean    sd    min   max  nobs
# <chr>                    <dbl> <dbl>  <dbl> <dbl> <int>
# 1 HHC_PCT_HHA_NURSING      100    0    100      100  1454
# 2 HHC_PCT_HHA_PHYS_THERAPY  98.0  7.52  25      100  1454
# 3 HHC_PCT_HHA_OCC_THERAPY   94.9 12.7   25      100  1454
# 4 HHC_PCT_HHA_SPEECH        91.9 16.6   20      100  1454
# 5 HHC_PCT_HHA_MEDICAL       87.7 20.2    9.09   100  1454
# 6 HHC_PCT_HHA_AIDE          96.6  9.75  42.9    100  1454
```

#### `mean_group_tbl()`

`mean_group_tbl()` now supports selecting variables either by stem or by full variable name. Both single and multiple values are accepted. 

Set `var_input = "stem"` (default) when searching for variables using stems. 

```r
mean_group_tbl(data = sdoh,
               var_stem = "HHC_PCT",
               group = "REGION")

# # A tibble: 24 × 7
# variable                  REGION     mean    sd   min   max  nobs
# <chr>                     <chr>     <dbl> <dbl> <dbl> <dbl> <int>
# 1 HHC_PCT_HHA_NURSING      Midwest    57.4  49.5     0   100  1055
# 2 HHC_PCT_HHA_NURSING      Northeast  74.2  43.9     0   100   217
# 3 HHC_PCT_HHA_NURSING      South      58.8  49.2     0   100  1422
# 4 HHC_PCT_HHA_NURSING      West       56    49.7     0   100   450
# 5 HHC_PCT_HHA_PHYS_THERAPY Midwest    55.2  48.9     0   100  1055
# 6 HHC_PCT_HHA_PHYS_THERAPY Northeast  68.0  43.1     0   100   217
# 7 HHC_PCT_HHA_PHYS_THERAPY South      58.4  49.0     0   100  1422
# 8 HHC_PCT_HHA_PHYS_THERAPY West       54.5  49.0     0   100   450
# 9 HHC_PCT_HHA_OCC_THERAPY  Midwest    52.9  48.7     0   100  1055
# 10 HHC_PCT_HHA_OCC_THERAPY Northeast  64.8  42.8     0   100   217
```

Set `var_input = "name"` when searching for variables using their name.

```r
mean_group_tbl(data = sdoh, 
               var_stem = c("ACS_PCT_AGE_0_4", "HHC_PCT_HHA_MEDICAL"), 
               var_input = "name",
               group = "REGION")

# # A tibble: 8 × 7
# variable              REGION     mean     sd   min    max  nobs
# <chr>                 <chr>     <dbl>  <dbl> <dbl>  <dbl> <int>
# 1 ACS_PCT_AGE_0_4     Midwest    5.90  1.13   2.4   12.0   1055
# 2 ACS_PCT_AGE_0_4     Northeast  5.04  0.829  0.95   8.12   217
# 3 ACS_PCT_AGE_0_4     South      5.76  1.26   0.98  18.4   1422
# 4 ACS_PCT_AGE_0_4     West       5.80  1.67   0.23  13.8    449
# 5 HHC_PCT_HHA_MEDICAL Midwest   33.0  43.3    0    100     1055
# 6 HHC_PCT_HHA_MEDICAL Northeast 62.2  42.6    0    100      217
# 7 HHC_PCT_HHA_MEDICAL South     45.7  46.8    0    100     1422
# 8 HHC_PCT_HHA_MEDICAL West      46.1  47.6    0    100      449
```

# summarytabl 0.1.0

* Developmental release of summarytabl
