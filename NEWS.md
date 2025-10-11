# summarytabl 0.2.0

## Minor improvements

1. `cat_group_tbl()` now includes a new `margins` argument. It allows users to choose how percentages are calculated: by `rows`, `columns`, or the entire table (`all`, which is the default).

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
# 1 female              0.146               0.102                                 0.243
# 2 male                0.146               0.110                                 0.253

# Margins: Columnwise
cat_group_tbl(data = nlsy_sub,
              row_var = "gender",
              col_var = "race",
              pivot = "wider",
              margins = "columns",
              only = "percent")
# gender percent_race_Black percent_race_Hispanic `percent_race_Non-Black,Non-Hispanic`
# <chr>               <dbl>                 <dbl>                                 <dbl>
# 1 female             0.5                  0.483                                 0.490
# 2 male               0.5                  0.517                                 0.510

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

2. `select_group_tbl()` now includes a new `margins` argument. It allows users to choose how percentages are calculated: by `rows`, `columns`, or the entire variable (`all`, which is the default).

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
                 group_type = "variable",
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
                 group_type = "variable",
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
                 group_type = "variable",
                 na_removal = "pairwise",
                 pivot = "wider",
                 margins = "rows",
                 only = "percent")
# variable                  values     percent_sex_female percent_sex_male
# <chr>                     <chr>                   <dbl>            <dbl>
# 1 involved_arts           selected                0.531            0.469
# 2 involved_arts           unselected              0.469            0.531
```

## New features

1. Two new datasets were added to the package.
	- `sdoh`: A subset of the `2020 Social Determinants of Health Database`.
	- `gss`: A subset of the `2022 General Social Survey`.
2. A new hex sticker logo was created for the package.

# summarytabl 0.1.0

* Developmental release of summarytabl
