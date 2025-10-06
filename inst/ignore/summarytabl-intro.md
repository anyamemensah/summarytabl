Welcome to the `summarytabl` package! This package makes it easy to
create simple tables for summarizing continuous, ordinal, and
categorical data. This document introduces you to some of
`summarytabl`’s most frequently used functions, along with examples of
how to apply them to your data frames.

To begin, load `summarytabl`

    library(summarytabl)

## Types of functions

This package has three types of functions to help you summarize your
data. Those for:

1.  Categorical data, such as binary (e.g., Unselected/Selected) or
    nominal (e.g., woman/man/non-binary) variables
2.  Multiple response data, including binary (e.g.,
    Unselected/Selected), ordinal (e.g., never, sometimes, often), or
    Likert and Likert-type (e.g., strongly disagree to strongly agree)
    variables
3.  Continuous data, like interval (e.g., test scores) and ratio-level
    (e.g., age) variables

Functions for categorical data start with `cat_`, those for multiple
response data start with `select_`, and functions for continuous data
start with `mean_`.

To learn more about how these functions work, read the next few
sections.

## Categorical variables

### Summarize a categorical variable

The `cat_tbl()` function can be used to generate a frequency table for a
categorical variable.

    cat_tbl(data = nlsy, var = "race")

    ## # A tibble: 3 × 3
    ##   race                   count percent
    ##   <chr>                  <int>   <dbl>
    ## 1 Black                    868   0.292
    ## 2 Hispanic                 631   0.212
    ## 3 Non-Black,Non-Hispanic  1477   0.496

You can exclude certain values and eliminate missing values from the
data using the `ignore` and `na.rm` arguments, respectively.

    cat_tbl(data = nlsy, 
            var = "race",
            ignore = "Black",
            na.rm = TRUE)

    ## # A tibble: 2 × 3
    ##   race                   count percent
    ##   <chr>                  <int>   <dbl>
    ## 1 Hispanic                 631   0.299
    ## 2 Non-Black,Non-Hispanic  1477   0.701

Finally, you can choose what information to return using the `only`
argument.

    # Default: counts and percentages
    cat_tbl(data = nlsy, 
            var = "race",
            ignore = "Black",
            na.rm = TRUE)

    ## # A tibble: 2 × 3
    ##   race                   count percent
    ##   <chr>                  <int>   <dbl>
    ## 1 Hispanic                 631   0.299
    ## 2 Non-Black,Non-Hispanic  1477   0.701

    # Counts only
    cat_tbl(data = nlsy, 
            var = "race",
            ignore = "Black",
            na.rm = TRUE,
            only = "count")

    ## # A tibble: 2 × 2
    ##   race                   count
    ##   <chr>                  <int>
    ## 1 Hispanic                 631
    ## 2 Non-Black,Non-Hispanic  1477

    # Percents only
    cat_tbl(data = nlsy, 
            var = "race",
            ignore = "Black",
            na.rm = TRUE,
            only = "percent")

    ## # A tibble: 2 × 2
    ##   race                   percent
    ##   <chr>                    <dbl>
    ## 1 Hispanic                 0.299
    ## 2 Non-Black,Non-Hispanic   0.701

### Comparing two categorical variables

To create a grouped frequency table for two categorical variables (i.e.,
a contingency table), use the `cat_group_tbl()` function.

    cat_group_tbl(data = nlsy,
                  row_var = "gender",
                  col_var = "bthwht")

    ## # A tibble: 4 × 4
    ##   gender bthwht count percent
    ##    <dbl>  <dbl> <int>   <dbl>
    ## 1      0      0  1340  0.450 
    ## 2      0      1   123  0.0413
    ## 3      1      0  1409  0.473 
    ## 4      1      1   104  0.0349

Like `cat_tbl()`, you have the option to exclude certain values and omit
missing values (by row and/or column).

    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  na.rm.row_var = TRUE,
                  ignore = c(race = "Non-Black,Non-Hispanic"),
                  pivot = "wider")

    ## # A tibble: 2 × 5
    ##   race     count_bthwht_0 count_bthwht_1 percent_bthwht_0 percent_bthwht_1
    ##   <chr>             <int>          <int>            <dbl>            <dbl>
    ## 1 Black               766            102            0.511           0.0680
    ## 2 Hispanic            589             42            0.393           0.0280

If you want to ignore more than one value per row or column, provide
them in a named list:

    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  na.rm.row_var = TRUE,
                  ignore = list(race = c("Non-Black,Non-Hispanic", "Hispanic")),
                  pivot = "wider")

    ## # A tibble: 1 × 5
    ##   race  count_bthwht_0 count_bthwht_1 percent_bthwht_0 percent_bthwht_1
    ##   <chr>          <int>          <int>            <dbl>            <dbl>
    ## 1 Black            766            102            0.882            0.118

Use the `margins` argument to define how percentages are calculated and
shown in the resulting table:

    # Default: percentages sum to one across the entire table
    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  pivot = "wider")

    ## # A tibble: 3 × 5
    ##   race           count_bthwht_0 count_bthwht_1 percent_bthwht_0 percent_bthwht_1
    ##   <chr>                   <int>          <int>            <dbl>            <dbl>
    ## 1 Black                     766            102            0.257           0.0343
    ## 2 Hispanic                  589             42            0.198           0.0141
    ## 3 Non-Black,Non…           1394             83            0.468           0.0279

    # Rowwise: percentages sum to one across rows
    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  margins = "rows",
                  pivot = "wider")

    ## # A tibble: 3 × 5
    ##   race           count_bthwht_0 count_bthwht_1 percent_bthwht_0 percent_bthwht_1
    ##   <chr>                   <int>          <int>            <dbl>            <dbl>
    ## 1 Black                     766            102            0.882           0.118 
    ## 2 Hispanic                  589             42            0.933           0.0666
    ## 3 Non-Black,Non…           1394             83            0.944           0.0562

    # Columnwise: percentages sum to one across columns
    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  margins = "columns",
                  pivot = "wider")

    ## # A tibble: 3 × 5
    ##   race           count_bthwht_0 count_bthwht_1 percent_bthwht_0 percent_bthwht_1
    ##   <chr>                   <int>          <int>            <dbl>            <dbl>
    ## 1 Black                     766            102            0.279            0.449
    ## 2 Hispanic                  589             42            0.214            0.185
    ## 3 Non-Black,Non…           1394             83            0.507            0.366

Finally, you can choose what information to return using the `only`
argument.

    # Default: counts and percentages
    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  na.rm.row_var = TRUE)

    ## # A tibble: 6 × 4
    ##   race                   bthwht count percent
    ##   <chr>                   <dbl> <int>   <dbl>
    ## 1 Black                       0   766  0.257 
    ## 2 Black                       1   102  0.0343
    ## 3 Hispanic                    0   589  0.198 
    ## 4 Hispanic                    1    42  0.0141
    ## 5 Non-Black,Non-Hispanic      0  1394  0.468 
    ## 6 Non-Black,Non-Hispanic      1    83  0.0279

    # Counts only
    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  na.rm.row_var = TRUE,
                  only = "count")

    ## # A tibble: 6 × 3
    ##   race                   bthwht count
    ##   <chr>                   <dbl> <int>
    ## 1 Black                       0   766
    ## 2 Black                       1   102
    ## 3 Hispanic                    0   589
    ## 4 Hispanic                    1    42
    ## 5 Non-Black,Non-Hispanic      0  1394
    ## 6 Non-Black,Non-Hispanic      1    83

    # Percents only
    cat_group_tbl(data = nlsy,
                  row_var = "race",
                  col_var = "bthwht",
                  na.rm.row_var = TRUE,
                  only = "percent")

    ## # A tibble: 6 × 3
    ##   race                   bthwht percent
    ##   <chr>                   <dbl>   <dbl>
    ## 1 Black                       0  0.257 
    ## 2 Black                       1  0.0343
    ## 3 Hispanic                    0  0.198 
    ## 4 Hispanic                    1  0.0141
    ## 5 Non-Black,Non-Hispanic      0  0.468 
    ## 6 Non-Black,Non-Hispanic      1  0.0279

## Multiple response variables

### Summarize multiple response variables

With `select_tbl()`, you can produce a summary table for multiple
response variables with the same variable stem. A variable stem is a
common prefix found in related variable names, often corresponding to
similar survey items, that represents a shared concept before unique
identifiers.

For example, the `depressive` dataset contains eight variables that
share the same variable stem: `dep`, with each one representing a
different item (such as a statement, question, or indicator) used to
measure depression:

    names(depressive)

    ##  [1] "cid"   "race"  "sex"   "yob"   "dep_1" "dep_2" "dep_3" "dep_4" "dep_5"
    ## [10] "dep_6" "dep_7" "dep_8"

With the `select_tbl()` function, you can summarize these responses to
see how many respondents selected each answer option for every variable.

    select_tbl(data = depressive, var_stem = "dep")

    ## # A tibble: 24 × 4
    ##    variable values count percent
    ##    <chr>     <int> <int>   <dbl>
    ##  1 dep_1         1   109  0.0678
    ##  2 dep_1         2   689  0.429 
    ##  3 dep_1         3   809  0.503 
    ##  4 dep_2         1   144  0.0896
    ##  5 dep_2         2   746  0.464 
    ##  6 dep_2         3   717  0.446 
    ##  7 dep_3         1  1162  0.723 
    ##  8 dep_3         2   392  0.244 
    ##  9 dep_3         3    53  0.0330
    ## 10 dep_4         1   601  0.374 
    ## # ℹ 14 more rows

You can also use the `ignore` and `na_removal` arguments to exclude
values from the data and specify how missing values should be handled.
By default, missing values are removed `listwise`, but you can set
`na_removal` to `pairwise` for pairwise removal instead:

    # Default listwise removal, value '3' removed from data
    select_tbl(data = depressive, 
               var_stem = "dep", 
               ignore = 3)

    ## # A tibble: 16 × 4
    ##    variable values count percent
    ##    <chr>     <int> <int>   <dbl>
    ##  1 dep_1         1    37   0.167
    ##  2 dep_1         2   185   0.833
    ##  3 dep_2         1    47   0.212
    ##  4 dep_2         2   175   0.788
    ##  5 dep_3         1   133   0.599
    ##  6 dep_3         2    89   0.401
    ##  7 dep_4         1   108   0.486
    ##  8 dep_4         2   114   0.514
    ##  9 dep_5         1    61   0.275
    ## 10 dep_5         2   161   0.725
    ## 11 dep_6         1    86   0.387
    ## 12 dep_6         2   136   0.613
    ## 13 dep_7         1   126   0.568
    ## 14 dep_7         2    96   0.432
    ## 15 dep_8         1    81   0.365
    ## 16 dep_8         2   141   0.635

    # Pairwise removal, value '3' removed from data
    select_tbl(data = depressive, 
               var_stem = "dep", 
               ignore = 3,
               na_removal = "pairwise")

    ## # A tibble: 16 × 4
    ##    variable values count percent
    ##    <chr>     <int> <int>   <dbl>
    ##  1 dep_1         1   120   0.145
    ##  2 dep_1         2   709   0.855
    ##  3 dep_2         1   151   0.165
    ##  4 dep_2         2   762   0.835
    ##  5 dep_3         1  1192   0.746
    ##  6 dep_3         2   406   0.254
    ##  7 dep_4         1   611   0.416
    ##  8 dep_4         2   856   0.584
    ##  9 dep_5         1   206   0.264
    ## 10 dep_5         2   574   0.736
    ## 11 dep_6         1   399   0.312
    ## 12 dep_6         2   879   0.688
    ## 13 dep_7         1  1046   0.674
    ## 14 dep_7         2   507   0.326
    ## 15 dep_8         1   323   0.287
    ## 16 dep_8         2   801   0.713

Set the `pivot` argument to `wider` to reshape the resulting table into
the wide format. By default, the summary table is presented in the long
format.

    # Default longer format
    select_tbl(data = depressive, 
               var_stem = "dep")

    ## # A tibble: 24 × 4
    ##    variable values count percent
    ##    <chr>     <int> <int>   <dbl>
    ##  1 dep_1         1   109  0.0678
    ##  2 dep_1         2   689  0.429 
    ##  3 dep_1         3   809  0.503 
    ##  4 dep_2         1   144  0.0896
    ##  5 dep_2         2   746  0.464 
    ##  6 dep_2         3   717  0.446 
    ##  7 dep_3         1  1162  0.723 
    ##  8 dep_3         2   392  0.244 
    ##  9 dep_3         3    53  0.0330
    ## 10 dep_4         1   601  0.374 
    ## # ℹ 14 more rows

    # Wider format
    select_tbl(data = depressive, 
               var_stem = "dep",
               pivot = "wider")

    ## # A tibble: 8 × 7
    ##   variable count_value_1 count_value_2 count_value_3 percent_value_1
    ##   <chr>            <int>         <int>         <int>           <dbl>
    ## 1 dep_1              109           689           809          0.0678
    ## 2 dep_2              144           746           717          0.0896
    ## 3 dep_3             1162           392            53          0.723 
    ## 4 dep_4              601           836           170          0.374 
    ## 5 dep_5              194           556           857          0.121 
    ## 6 dep_6              387           860           360          0.241 
    ## 7 dep_7             1028           490            89          0.640 
    ## 8 dep_8              317           784           506          0.197 
    ## # ℹ 2 more variables: percent_value_2 <dbl>, percent_value_3 <dbl>

You can use the `var_labels` argument to include variable labels in your
summary table to make the variable names easier to interpret:

    select_tbl(data = depressive, 
               var_stem = "dep",
               pivot = "wider",
               var_labels = c(
                 dep_1="how often child feels sad and blue",
                 dep_2="how often child feels nervous, tense, or on edge",
                 dep_3="how often child feels happy",
                 dep_4="how often child feels bored",
                 dep_5="how often child feels lonely",
                 dep_6="how often child feels tired or worn out",
                 dep_7="how often child feels excited about something",
                 dep_8="how often child feels too busy to get everything"
               )
    )

    ## # A tibble: 8 × 8
    ##   variable variable_label              count_value_1 count_value_2 count_value_3
    ##   <chr>    <chr>                               <int>         <int>         <int>
    ## 1 dep_1    how often child feels sad …           109           689           809
    ## 2 dep_2    how often child feels nerv…           144           746           717
    ## 3 dep_3    how often child feels happy          1162           392            53
    ## 4 dep_4    how often child feels bored           601           836           170
    ## 5 dep_5    how often child feels lone…           194           556           857
    ## 6 dep_6    how often child feels tire…           387           860           360
    ## 7 dep_7    how often child feels exci…          1028           490            89
    ## 8 dep_8    how often child feels too …           317           784           506
    ## # ℹ 3 more variables: percent_value_1 <dbl>, percent_value_2 <dbl>,
    ## #   percent_value_3 <dbl>

Finally, you can choose what information to return using the `only`
argument.

    # Default: counts and percentages
    select_tbl(data = depressive, 
               var_stem = "dep",
               pivot = "wider")

    ## # A tibble: 8 × 7
    ##   variable count_value_1 count_value_2 count_value_3 percent_value_1
    ##   <chr>            <int>         <int>         <int>           <dbl>
    ## 1 dep_1              109           689           809          0.0678
    ## 2 dep_2              144           746           717          0.0896
    ## 3 dep_3             1162           392            53          0.723 
    ## 4 dep_4              601           836           170          0.374 
    ## 5 dep_5              194           556           857          0.121 
    ## 6 dep_6              387           860           360          0.241 
    ## 7 dep_7             1028           490            89          0.640 
    ## 8 dep_8              317           784           506          0.197 
    ## # ℹ 2 more variables: percent_value_2 <dbl>, percent_value_3 <dbl>

    # Counts only
    select_tbl(data = depressive, 
               var_stem = "dep",
               pivot = "wider",
               only = "count")

    ## # A tibble: 8 × 4
    ##   variable count_value_1 count_value_2 count_value_3
    ##   <chr>            <int>         <int>         <int>
    ## 1 dep_1              109           689           809
    ## 2 dep_2              144           746           717
    ## 3 dep_3             1162           392            53
    ## 4 dep_4              601           836           170
    ## 5 dep_5              194           556           857
    ## 6 dep_6              387           860           360
    ## 7 dep_7             1028           490            89
    ## 8 dep_8              317           784           506

    # Percents only
    select_tbl(data = depressive, 
               var_stem = "dep",
               pivot = "wider",
               only = "percent")

    ## # A tibble: 8 × 4
    ##   variable percent_value_1 percent_value_2 percent_value_3
    ##   <chr>              <dbl>           <dbl>           <dbl>
    ## 1 dep_1             0.0678           0.429          0.503 
    ## 2 dep_2             0.0896           0.464          0.446 
    ## 3 dep_3             0.723            0.244          0.0330
    ## 4 dep_4             0.374            0.520          0.106 
    ## 5 dep_5             0.121            0.346          0.533 
    ## 6 dep_6             0.241            0.535          0.224 
    ## 7 dep_7             0.640            0.305          0.0554
    ## 8 dep_8             0.197            0.488          0.315

### Summarize multiple response variables by group or pattern

With `select_group_tbl()`, you can create a summary table for multiple
response variables with the same variable stem, grouped either by
another variable in your dataset or by matching a pattern in the
variable names.

For example, we often want to summarize survey responses by demographic
variables like gender, age, or race:

    dep_recoded <- 
      depressive |>
      dplyr::mutate(
        race = dplyr::case_match(.x = race,
                                 1 ~ "Hispanic", 
                                 2 ~ "Black", 
                                 3 ~ "Non-Black/Non-Hispanic",
                                 .default = NA)
      ) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::starts_with("dep"),
          .fns = ~ dplyr::case_when(.x == 1 ~ "often", 
                                    .x == 2 ~ "sometimes", 
                                    .x == 3 ~ "hardly ever")
        )
      ) |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::starts_with("dep"),
          .fns = ~ factor(., levels = c("often", "sometimes", "hardly ever"),
          labels = c("often", "sometimes", "hardly ever"))
        )
      )

    # longer format
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer")

    ## # A tibble: 72 × 5
    ##    variable race                   values      count percent
    ##    <chr>    <chr>                  <fct>       <int>   <dbl>
    ##  1 dep_1    Black                  often          45  0.0280
    ##  2 dep_1    Black                  sometimes     194  0.121 
    ##  3 dep_1    Black                  hardly ever   248  0.154 
    ##  4 dep_1    Hispanic               often          28  0.0174
    ##  5 dep_1    Hispanic               sometimes     155  0.0965
    ##  6 dep_1    Hispanic               hardly ever   187  0.116 
    ##  7 dep_1    Non-Black/Non-Hispanic often          36  0.0224
    ##  8 dep_1    Non-Black/Non-Hispanic sometimes     340  0.212 
    ##  9 dep_1    Non-Black/Non-Hispanic hardly ever   374  0.233 
    ## 10 dep_2    Black                  often          49  0.0305
    ## # ℹ 62 more rows

    # wider format
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "wider")

    ## # A tibble: 24 × 8
    ##    variable values   count_race_Black count_race_Hispanic count_race_Non-Black…¹
    ##    <chr>    <fct>               <int>               <int>                  <int>
    ##  1 dep_1    often                  45                  28                     36
    ##  2 dep_1    sometim…              194                 155                    340
    ##  3 dep_1    hardly …              248                 187                    374
    ##  4 dep_2    often                  49                  36                     59
    ##  5 dep_2    sometim…              204                 163                    379
    ##  6 dep_2    hardly …              234                 171                    312
    ##  7 dep_3    often                 330                 246                    586
    ##  8 dep_3    sometim…              138                 104                    150
    ##  9 dep_3    hardly …               19                  20                     14
    ## 10 dep_4    often                 183                 131                    287
    ## # ℹ 14 more rows
    ## # ℹ abbreviated name: ¹​`count_race_Non-Black/Non-Hispanic`
    ## # ℹ 3 more variables: percent_race_Black <dbl>, percent_race_Hispanic <dbl>,
    ## #   `percent_race_Non-Black/Non-Hispanic` <dbl>

As with `cat_group_tbl()`, you can define values to exclude and how to
handle missing data. When excluding values, use the `var_stem` argument
to specify which values to omit across variables sharing that stem.

    # Default listwise removal: 'often' value removed from all
    # dep_ variables, and 'Non-Black/Non-Hispanic' value removed
    # from race variable
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer",
                     ignore = c(dep = "often", race = "Non-Black/Non-Hispanic"))

    ## # A tibble: 31 × 5
    ##    variable race     values count percent
    ##    <chr>    <chr>     <int> <int>   <dbl>
    ##  1 dep_1    Black         2    16   0.254
    ##  2 dep_1    Black         3    18   0.286
    ##  3 dep_1    Hispanic      2    20   0.317
    ##  4 dep_1    Hispanic      3     9   0.143
    ##  5 dep_2    Black         2    16   0.254
    ##  6 dep_2    Black         3    18   0.286
    ##  7 dep_2    Hispanic      2    16   0.254
    ##  8 dep_2    Hispanic      3    13   0.206
    ##  9 dep_3    Black         2    34   0.540
    ## 10 dep_3    Hispanic      2    21   0.333
    ## # ℹ 21 more rows

    # Pairwise removal: 'often' value removed from all
    # dep_ variables, and 'Non-Black/Non-Hispanic' value removed
    # from race variable
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer",
                     ignore = c(dep = "often", race = "Non-Black/Non-Hispanic"),
                     na_removal = "pairwise")

    ## # A tibble: 32 × 5
    ##    variable race     values count percent
    ##    <chr>    <chr>     <int> <int>   <dbl>
    ##  1 dep_1    Black         2   203  0.251 
    ##  2 dep_1    Black         3   256  0.317 
    ##  3 dep_1    Hispanic      2   159  0.197 
    ##  4 dep_1    Hispanic      3   190  0.235 
    ##  5 dep_2    Black         2   213  0.269 
    ##  6 dep_2    Black         3   241  0.305 
    ##  7 dep_2    Hispanic      2   165  0.209 
    ##  8 dep_2    Hispanic      3   172  0.217 
    ##  9 dep_3    Black         2   149  0.507 
    ## 10 dep_3    Black         3    20  0.0680
    ## # ℹ 22 more rows

Use a list if you want to exclude several values from the same
`var_stem` or `group` variable:

    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer",
                     ignore = list(race = c("Hispanic", "Non-Black/Non-Hispanic")))

    ## # A tibble: 24 × 5
    ##    variable race  values      count percent
    ##    <chr>    <chr> <fct>       <int>   <dbl>
    ##  1 dep_1    Black often          45  0.0924
    ##  2 dep_1    Black sometimes     194  0.398 
    ##  3 dep_1    Black hardly ever   248  0.509 
    ##  4 dep_2    Black often          49  0.101 
    ##  5 dep_2    Black sometimes     204  0.419 
    ##  6 dep_2    Black hardly ever   234  0.480 
    ##  7 dep_3    Black often         330  0.678 
    ##  8 dep_3    Black sometimes     138  0.283 
    ##  9 dep_3    Black hardly ever    19  0.0390
    ## 10 dep_4    Black often         183  0.376 
    ## # ℹ 14 more rows

When `group_type` is `variable` you can use the `margins` argument to
define how percentages are calculated and shown in the resulting table:

    # Default: percentages for each variable sum to one.
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer",
                     only = "percent")

    ## # A tibble: 72 × 4
    ##    variable race                   values      percent
    ##    <chr>    <chr>                  <fct>         <dbl>
    ##  1 dep_1    Black                  often        0.0280
    ##  2 dep_1    Black                  sometimes    0.121 
    ##  3 dep_1    Black                  hardly ever  0.154 
    ##  4 dep_1    Hispanic               often        0.0174
    ##  5 dep_1    Hispanic               sometimes    0.0965
    ##  6 dep_1    Hispanic               hardly ever  0.116 
    ##  7 dep_1    Non-Black/Non-Hispanic often        0.0224
    ##  8 dep_1    Non-Black/Non-Hispanic sometimes    0.212 
    ##  9 dep_1    Non-Black/Non-Hispanic hardly ever  0.233 
    ## 10 dep_2    Black                  often        0.0305
    ## # ℹ 62 more rows

    # Rowwise: for each value of the variable, the percentages 
    # for all levels of the grouping variable sum to one
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     margins = "rows",
                     pivot = "longer",
                     only = "percent")

    ## # A tibble: 72 × 4
    ##    variable race                   values      percent
    ##    <chr>    <chr>                  <fct>         <dbl>
    ##  1 dep_1    Black                  often         0.413
    ##  2 dep_1    Hispanic               often         0.257
    ##  3 dep_1    Non-Black/Non-Hispanic often         0.330
    ##  4 dep_1    Black                  sometimes     0.282
    ##  5 dep_1    Hispanic               sometimes     0.225
    ##  6 dep_1    Non-Black/Non-Hispanic sometimes     0.493
    ##  7 dep_1    Black                  hardly ever   0.307
    ##  8 dep_1    Hispanic               hardly ever   0.231
    ##  9 dep_1    Non-Black/Non-Hispanic hardly ever   0.462
    ## 10 dep_2    Black                  often         0.340
    ## # ℹ 62 more rows

    # Columnwise: for each level of the grouping variable, 
    # the percentages for all values of the main variable 
    # sum to one
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     margins = "columns",
                     pivot = "longer",
                     only = "percent")

    ## # A tibble: 72 × 4
    ##    variable race                   values      percent
    ##    <chr>    <chr>                  <fct>         <dbl>
    ##  1 dep_1    Black                  often        0.0924
    ##  2 dep_1    Black                  sometimes    0.398 
    ##  3 dep_1    Black                  hardly ever  0.509 
    ##  4 dep_1    Hispanic               often        0.0757
    ##  5 dep_1    Hispanic               sometimes    0.419 
    ##  6 dep_1    Hispanic               hardly ever  0.505 
    ##  7 dep_1    Non-Black/Non-Hispanic often        0.048 
    ##  8 dep_1    Non-Black/Non-Hispanic sometimes    0.453 
    ##  9 dep_1    Non-Black/Non-Hispanic hardly ever  0.499 
    ## 10 dep_2    Black                  often        0.101 
    ## # ℹ 62 more rows

Another application of `select_group_tbl` is summarizing responses based
on a matching pattern, such as survey time points (e.g., waves). To use
this feature, set `group_type` to `pattern` and enter the pattern to
search for in the `group` argument.

For example, the `stem_social_psych` dataset includes a set of variables
responded to by students at two different time points (“w1” and “w2”).
You can summarize the responses for these variables using the following
approach:

    select_group_tbl(data = stem_social_psych, 
                     var_stem = "belong_belong",
                     group = "_w\\d",
                     group_type = "pattern",
                     pivot = "longer")

    ## # A tibble: 10 × 5
    ##    variable             group values count percent
    ##    <chr>                <chr>  <dbl> <int>   <dbl>
    ##  1 belong_belongStem_w1 w1         1     5  0.0185
    ##  2 belong_belongStem_w1 w1         2    20  0.0741
    ##  3 belong_belongStem_w1 w1         3    59  0.219 
    ##  4 belong_belongStem_w1 w1         4   107  0.396 
    ##  5 belong_belongStem_w1 w1         5    79  0.293 
    ##  6 belong_belongStem_w2 w2         1    11  0.0407
    ##  7 belong_belongStem_w2 w2         2    11  0.0407
    ##  8 belong_belongStem_w2 w2         3    44  0.163 
    ##  9 belong_belongStem_w2 w2         4   113  0.419 
    ## 10 belong_belongStem_w2 w2         5    91  0.337

Use the `group_name` argument to assign a descriptive name to the column
containing the matched pattern values.

    select_group_tbl(data = stem_social_psych, 
                     var_stem = "belong_belong",
                     group = "_w\\d",
                     group_type = "pattern",
                     group_name = "wave",
                     pivot = "longer")

    ## # A tibble: 10 × 5
    ##    variable             wave  values count percent
    ##    <chr>                <chr>  <dbl> <int>   <dbl>
    ##  1 belong_belongStem_w1 w1         1     5  0.0185
    ##  2 belong_belongStem_w1 w1         2    20  0.0741
    ##  3 belong_belongStem_w1 w1         3    59  0.219 
    ##  4 belong_belongStem_w1 w1         4   107  0.396 
    ##  5 belong_belongStem_w1 w1         5    79  0.293 
    ##  6 belong_belongStem_w2 w2         1    11  0.0407
    ##  7 belong_belongStem_w2 w2         2    11  0.0407
    ##  8 belong_belongStem_w2 w2         3    44  0.163 
    ##  9 belong_belongStem_w2 w2         4   113  0.419 
    ## 10 belong_belongStem_w2 w2         5    91  0.337

You can use the `var_labels` argument to include variable labels in your
summary table to make the variable names easier to interpret:

    select_group_tbl(data = stem_social_psych, 
                     var_stem = "belong_belong",
                     group = "_w\\d",
                     group_type = "pattern",
                     group_name = "wave",
                     pivot = "longer",
                     var_labels = c(
                       belong_belongStem_w1 = "I feel like I belong in STEM (wave 1)",
                       belong_belongStem_w2 = "I feel like I belong in STEM (wave 2)"
                     ))

    ## # A tibble: 10 × 6
    ##    variable             variable_label                wave  values count percent
    ##    <chr>                <chr>                         <chr>  <dbl> <int>   <dbl>
    ##  1 belong_belongStem_w1 I feel like I belong in STEM… w1         1     5  0.0185
    ##  2 belong_belongStem_w1 I feel like I belong in STEM… w1         2    20  0.0741
    ##  3 belong_belongStem_w1 I feel like I belong in STEM… w1         3    59  0.219 
    ##  4 belong_belongStem_w1 I feel like I belong in STEM… w1         4   107  0.396 
    ##  5 belong_belongStem_w1 I feel like I belong in STEM… w1         5    79  0.293 
    ##  6 belong_belongStem_w2 I feel like I belong in STEM… w2         1    11  0.0407
    ##  7 belong_belongStem_w2 I feel like I belong in STEM… w2         2    11  0.0407
    ##  8 belong_belongStem_w2 I feel like I belong in STEM… w2         3    44  0.163 
    ##  9 belong_belongStem_w2 I feel like I belong in STEM… w2         4   113  0.419 
    ## 10 belong_belongStem_w2 I feel like I belong in STEM… w2         5    91  0.337

Finally, you can choose what information to return using the `only`
argument.

    # Default: counts and percentages
    select_group_tbl(data = stem_social_psych, 
                     var_stem = "belong_belong",
                     group = "_w\\d",
                     group_type = "pattern",
                     group_name = "wave",
                     pivot = "longer",
                     only = "count")

    ## # A tibble: 10 × 4
    ##    variable             wave  values count
    ##    <chr>                <chr>  <dbl> <int>
    ##  1 belong_belongStem_w1 w1         1     5
    ##  2 belong_belongStem_w1 w1         2    20
    ##  3 belong_belongStem_w1 w1         3    59
    ##  4 belong_belongStem_w1 w1         4   107
    ##  5 belong_belongStem_w1 w1         5    79
    ##  6 belong_belongStem_w2 w2         1    11
    ##  7 belong_belongStem_w2 w2         2    11
    ##  8 belong_belongStem_w2 w2         3    44
    ##  9 belong_belongStem_w2 w2         4   113
    ## 10 belong_belongStem_w2 w2         5    91

    # Counts only
    select_group_tbl(data = stem_social_psych, 
                     var_stem = "belong_belong",
                     group = "_w\\d",
                     group_type = "pattern",
                     group_name = "wave",
                     pivot = "longer",
                     only = "count")

    ## # A tibble: 10 × 4
    ##    variable             wave  values count
    ##    <chr>                <chr>  <dbl> <int>
    ##  1 belong_belongStem_w1 w1         1     5
    ##  2 belong_belongStem_w1 w1         2    20
    ##  3 belong_belongStem_w1 w1         3    59
    ##  4 belong_belongStem_w1 w1         4   107
    ##  5 belong_belongStem_w1 w1         5    79
    ##  6 belong_belongStem_w2 w2         1    11
    ##  7 belong_belongStem_w2 w2         2    11
    ##  8 belong_belongStem_w2 w2         3    44
    ##  9 belong_belongStem_w2 w2         4   113
    ## 10 belong_belongStem_w2 w2         5    91

    # Percents only
    select_group_tbl(data = stem_social_psych, 
                     var_stem = "belong_belong",
                     group = "_w\\d",
                     group_type = "pattern",
                     group_name = "wave",
                     pivot = "longer",
                     only = "percent")

    ## # A tibble: 10 × 4
    ##    variable             wave  values percent
    ##    <chr>                <chr>  <dbl>   <dbl>
    ##  1 belong_belongStem_w1 w1         1  0.0185
    ##  2 belong_belongStem_w1 w1         2  0.0741
    ##  3 belong_belongStem_w1 w1         3  0.219 
    ##  4 belong_belongStem_w1 w1         4  0.396 
    ##  5 belong_belongStem_w1 w1         5  0.293 
    ##  6 belong_belongStem_w2 w2         1  0.0407
    ##  7 belong_belongStem_w2 w2         2  0.0407
    ##  8 belong_belongStem_w2 w2         3  0.163 
    ##  9 belong_belongStem_w2 w2         4  0.419 
    ## 10 belong_belongStem_w2 w2         5  0.337

## Continuous variables

### Summarize continuous variables

With the `mean_tbl()` function, you can summarize a series of continuous
variables that share the same variable stem. The resulting table
provides descriptive statistics for each variable, including the mean,
standard deviation, minimum, maximum, and the number of non-missing
values.

    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT")

    ## # A tibble: 6 × 6
    ##   variable                  mean    sd   min   max  nobs
    ##   <chr>                    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING       58.2  49.3     0   100  3227
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  56.7  48.8     0   100  3227
    ## 3 HHC_PCT_HHA_OCC_THERAPY   52.4  48.3     0   100  3227
    ## 4 HHC_PCT_HHA_SPEECH        49.1  47.6     0   100  3227
    ## 5 HHC_PCT_HHA_MEDICAL       42.2  46.2     0   100  3227
    ## 6 HHC_PCT_HHA_AIDE          55.1  48.6     0   100  3227

Like the other functions in this package, you can use the `ignore`
argument to specify which values to exclude from all variables
associated with the provided variable stem.

    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             ignore = 0)

    ## # A tibble: 6 × 6
    ##   variable                  mean    sd    min   max  nobs
    ##   <chr>                    <dbl> <dbl>  <dbl> <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING      100    0    100      100  1454
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  98.0  7.52  25      100  1454
    ## 3 HHC_PCT_HHA_OCC_THERAPY   94.9 12.7   25      100  1454
    ## 4 HHC_PCT_HHA_SPEECH        91.9 16.6   20      100  1454
    ## 5 HHC_PCT_HHA_MEDICAL       87.7 20.2    9.09   100  1454
    ## 6 HHC_PCT_HHA_AIDE          96.6  9.75  42.9    100  1454

You can also specify how missing values are removed:

    # Default listwise removal
    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             ignore = 0)

    ## # A tibble: 6 × 6
    ##   variable                  mean    sd    min   max  nobs
    ##   <chr>                    <dbl> <dbl>  <dbl> <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING      100    0    100      100  1454
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  98.0  7.52  25      100  1454
    ## 3 HHC_PCT_HHA_OCC_THERAPY   94.9 12.7   25      100  1454
    ## 4 HHC_PCT_HHA_SPEECH        91.9 16.6   20      100  1454
    ## 5 HHC_PCT_HHA_MEDICAL       87.7 20.2    9.09   100  1454
    ## 6 HHC_PCT_HHA_AIDE          96.6  9.75  42.9    100  1454

    # Pairwise removal
    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             na_removal = "pairwise",
             ignore = 0)

    ## # A tibble: 6 × 6
    ##   variable                  mean    sd    min   max  nobs
    ##   <chr>                    <dbl> <dbl>  <dbl> <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING      100    0    100      100  1879
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  98.1  7.84  25      100  1865
    ## 3 HHC_PCT_HHA_OCC_THERAPY   95.2 12.8   25      100  1776
    ## 4 HHC_PCT_HHA_SPEECH        92.4 16.4   20      100  1714
    ## 5 HHC_PCT_HHA_MEDICAL       88.1 20.1    9.09   100  1547
    ## 6 HHC_PCT_HHA_AIDE          97.1  9.26  33.3    100  1833

Including variable labels in your summary table can help make the
variable names easier to interpret.

    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             na_removal = "pairwise",
             var_labels = c(
               HHC_PCT_HHA_NURSING="% of agencies offering nursing care services",
               HHC_PCT_HHA_PHYS_THERAPY="% of agencies offering physical therapy services",
               HHC_PCT_HHA_OCC_THERAPY="% of agencies offering occupational therapy services",
               HHC_PCT_HHA_SPEECH="% of agencies offering speech pathology services",
               HHC_PCT_HHA_MEDICAL="% of agencies offering medical social services",
               HHC_PCT_HHA_AIDE="% of agencies offering home health aide services"
             ))

    ## # A tibble: 6 × 7
    ##   variable                 variable_label           mean    sd   min   max  nobs
    ##   <chr>                    <chr>                   <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING      % of agencies offering…  58.2  49.3     0   100  3227
    ## 2 HHC_PCT_HHA_PHYS_THERAPY % of agencies offering…  56.7  48.8     0   100  3227
    ## 3 HHC_PCT_HHA_OCC_THERAPY  % of agencies offering…  52.4  48.3     0   100  3227
    ## 4 HHC_PCT_HHA_SPEECH       % of agencies offering…  49.1  47.6     0   100  3227
    ## 5 HHC_PCT_HHA_MEDICAL      % of agencies offering…  42.2  46.2     0   100  3227
    ## 6 HHC_PCT_HHA_AIDE         % of agencies offering…  55.1  48.6     0   100  3227

Finally, you can choose what information to return using the `only`
argument.

    # Default: all summary statistics returned
    # (mean, sd, min, max, nobs)
    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             na_removal = "pairwise")

    ## # A tibble: 6 × 6
    ##   variable                  mean    sd   min   max  nobs
    ##   <chr>                    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING       58.2  49.3     0   100  3227
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  56.7  48.8     0   100  3227
    ## 3 HHC_PCT_HHA_OCC_THERAPY   52.4  48.3     0   100  3227
    ## 4 HHC_PCT_HHA_SPEECH        49.1  47.6     0   100  3227
    ## 5 HHC_PCT_HHA_MEDICAL       42.2  46.2     0   100  3227
    ## 6 HHC_PCT_HHA_AIDE          55.1  48.6     0   100  3227

    # Means and non-missing observations returned
    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             na_removal = "pairwise",
             only = c("mean", "nobs"))

    ## # A tibble: 6 × 3
    ##   variable                  mean  nobs
    ##   <chr>                    <dbl> <int>
    ## 1 HHC_PCT_HHA_NURSING       58.2  3227
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  56.7  3227
    ## 3 HHC_PCT_HHA_OCC_THERAPY   52.4  3227
    ## 4 HHC_PCT_HHA_SPEECH        49.1  3227
    ## 5 HHC_PCT_HHA_MEDICAL       42.2  3227
    ## 6 HHC_PCT_HHA_AIDE          55.1  3227

    # Means and standard deviations  returned
    mean_tbl(data = sdoh, 
             var_stem = "HHC_PCT",
             na_removal = "pairwise",
             only = c("mean", "sd"))

    ## # A tibble: 6 × 3
    ##   variable                  mean    sd
    ##   <chr>                    <dbl> <dbl>
    ## 1 HHC_PCT_HHA_NURSING       58.2  49.3
    ## 2 HHC_PCT_HHA_PHYS_THERAPY  56.7  48.8
    ## 3 HHC_PCT_HHA_OCC_THERAPY   52.4  48.3
    ## 4 HHC_PCT_HHA_SPEECH        49.1  47.6
    ## 5 HHC_PCT_HHA_MEDICAL       42.2  46.2
    ## 6 HHC_PCT_HHA_AIDE          55.1  48.6

### Summarize continuous variables by group or pattern

With the `mean_group_tbl()` function, you can produce a summary table
for a series of continuous variables sharing the same variable stem,
grouped either by another variable in your dataset or by matching a
pattern in the variable names.

For example, we often want to present summary statistics for responses
by demographic variables like region, gender, age, or race:

    mean_group_tbl(data = sdoh, 
                   var_stem = "HHC_PCT",
                   group = "REGION",
                   group_type = "variable")

    ## # A tibble: 24 × 7
    ##    variable                 REGION     mean    sd   min   max  nobs
    ##    <chr>                    <chr>     <dbl> <dbl> <dbl> <dbl> <int>
    ##  1 HHC_PCT_HHA_NURSING      Midwest    57.4  49.5     0   100  1055
    ##  2 HHC_PCT_HHA_NURSING      Northeast  74.2  43.9     0   100   217
    ##  3 HHC_PCT_HHA_NURSING      South      58.8  49.2     0   100  1422
    ##  4 HHC_PCT_HHA_NURSING      West       56    49.7     0   100   450
    ##  5 HHC_PCT_HHA_PHYS_THERAPY Midwest    55.2  48.9     0   100  1055
    ##  6 HHC_PCT_HHA_PHYS_THERAPY Northeast  68.0  43.1     0   100   217
    ##  7 HHC_PCT_HHA_PHYS_THERAPY South      58.4  49.0     0   100  1422
    ##  8 HHC_PCT_HHA_PHYS_THERAPY West       54.5  49.0     0   100   450
    ##  9 HHC_PCT_HHA_OCC_THERAPY  Midwest    52.9  48.7     0   100  1055
    ## 10 HHC_PCT_HHA_OCC_THERAPY  Northeast  64.8  42.8     0   100   217
    ## # ℹ 14 more rows

As with `select_group_tbl()`, you can specify which values to exclude
and whether to omit missing values. Be sure to use the `var_stem`
argument you provide to determine which values to exclude for variables
sharing the same stem.

    # Default listwise removal
    mean_group_tbl(data = sdoh, 
                   var_stem = "HHC_PCT",
                   group = "REGION",
                   ignore = c(HHC_PCT = 0, REGION = "Northeast"))

    ## # A tibble: 18 × 7
    ##    variable                 REGION   mean    sd    min   max  nobs
    ##    <chr>                    <chr>   <dbl> <dbl>  <dbl> <dbl> <int>
    ##  1 HHC_PCT_HHA_NURSING      Midwest 100    0    100      100   403
    ##  2 HHC_PCT_HHA_NURSING      South   100    0    100      100   681
    ##  3 HHC_PCT_HHA_NURSING      West    100    0    100      100   200
    ##  4 HHC_PCT_HHA_PHYS_THERAPY Midwest  97.7  7.15  50      100   403
    ##  5 HHC_PCT_HHA_PHYS_THERAPY South    99.2  4.78  50      100   681
    ##  6 HHC_PCT_HHA_PHYS_THERAPY West     98.3  5.31  60      100   200
    ##  7 HHC_PCT_HHA_OCC_THERAPY  Midwest  96.3 10.4   33.3    100   403
    ##  8 HHC_PCT_HHA_OCC_THERAPY  South    95.5 12.4   28.6    100   681
    ##  9 HHC_PCT_HHA_OCC_THERAPY  West     94.8 12.2   25      100   200
    ## 10 HHC_PCT_HHA_SPEECH       Midwest  91.9 16.2   33.3    100   403
    ## 11 HHC_PCT_HHA_SPEECH       South    93.4 15.3   25      100   681
    ## 12 HHC_PCT_HHA_SPEECH       West     91.0 17.2   20      100   200
    ## 13 HHC_PCT_HHA_MEDICAL      Midwest  82.4 23.8    9.09   100   403
    ## 14 HHC_PCT_HHA_MEDICAL      South    89.4 18.6   16.7    100   681
    ## 15 HHC_PCT_HHA_MEDICAL      West     92.6 15.3   33.3    100   200
    ## 16 HHC_PCT_HHA_AIDE         Midwest  97.3  8.97  50      100   403
    ## 17 HHC_PCT_HHA_AIDE         South    96.1 10.3   42.9    100   681
    ## 18 HHC_PCT_HHA_AIDE         West     96.4  9.96  50      100   200

    # Pairwise removal
    mean_group_tbl(data = sdoh, 
                   var_stem = "HHC_PCT",
                   group = "REGION",
                   na_removal = "pairwise",
                   ignore = c(HHC_PCT = 0, REGION = "Northeast"))

    ## # A tibble: 18 × 7
    ##    variable                 REGION   mean    sd    min   max  nobs
    ##    <chr>                    <chr>   <dbl> <dbl>  <dbl> <dbl> <int>
    ##  1 HHC_PCT_HHA_NURSING      Midwest 100    0    100      100   606
    ##  2 HHC_PCT_HHA_NURSING      South   100    0    100      100   836
    ##  3 HHC_PCT_HHA_NURSING      West    100    0    100      100   252
    ##  4 HHC_PCT_HHA_PHYS_THERAPY Midwest  97.8  8.36  25      100   595
    ##  5 HHC_PCT_HHA_PHYS_THERAPY South    99.4  4.32  50      100   836
    ##  6 HHC_PCT_HHA_PHYS_THERAPY West     97.7  8.14  33.3    100   251
    ##  7 HHC_PCT_HHA_OCC_THERAPY  Midwest  96.3 11.5   25      100   579
    ##  8 HHC_PCT_HHA_OCC_THERAPY  South    95.8 12.2   28.6    100   787
    ##  9 HHC_PCT_HHA_OCC_THERAPY  West     94.5 13.0   25      100   232
    ## 10 HHC_PCT_HHA_SPEECH       Midwest  92.6 16.1   25      100   552
    ## 11 HHC_PCT_HHA_SPEECH       South    93.7 15.2   25      100   769
    ## 12 HHC_PCT_HHA_SPEECH       West     91.3 17.0   20      100   221
    ## 13 HHC_PCT_HHA_MEDICAL      Midwest  83.0 23.6    9.09   100   419
    ## 14 HHC_PCT_HHA_MEDICAL      South    89.7 18.6   16.7    100   724
    ## 15 HHC_PCT_HHA_MEDICAL      West     92.5 15.8   33.3    100   224
    ## 16 HHC_PCT_HHA_AIDE         Midwest  98.0  7.85  50      100   588
    ## 17 HHC_PCT_HHA_AIDE         South    96.6  9.82  42.9    100   816
    ## 18 HHC_PCT_HHA_AIDE         West     96.4 10.8   33.3    100   247

Use a list if you want to exclude several values from the same
`var_stem` or `group` variable:

    mean_group_tbl(data = sdoh, 
                   var_stem = "HHC_PCT",
                   group = "REGION",
                   na_removal = "pairwise",
                   ignore = list(HHC_PCT = 0, REGION = c("Northeast", "South")
                   ))

    ## # A tibble: 12 × 7
    ##    variable                 REGION   mean    sd    min   max  nobs
    ##    <chr>                    <chr>   <dbl> <dbl>  <dbl> <dbl> <int>
    ##  1 HHC_PCT_HHA_NURSING      Midwest 100    0    100      100   606
    ##  2 HHC_PCT_HHA_NURSING      West    100    0    100      100   252
    ##  3 HHC_PCT_HHA_PHYS_THERAPY Midwest  97.8  8.36  25      100   595
    ##  4 HHC_PCT_HHA_PHYS_THERAPY West     97.7  8.14  33.3    100   251
    ##  5 HHC_PCT_HHA_OCC_THERAPY  Midwest  96.3 11.5   25      100   579
    ##  6 HHC_PCT_HHA_OCC_THERAPY  West     94.5 13.0   25      100   232
    ##  7 HHC_PCT_HHA_SPEECH       Midwest  92.6 16.1   25      100   552
    ##  8 HHC_PCT_HHA_SPEECH       West     91.3 17.0   20      100   221
    ##  9 HHC_PCT_HHA_MEDICAL      Midwest  83.0 23.6    9.09   100   419
    ## 10 HHC_PCT_HHA_MEDICAL      West     92.5 15.8   33.3    100   224
    ## 11 HHC_PCT_HHA_AIDE         Midwest  98.0  7.85  50      100   588
    ## 12 HHC_PCT_HHA_AIDE         West     96.4 10.8   33.3    100   247

Another application of `mean_group_tbl` is summarizing responses based
on a matching pattern, such as survey time points (e.g., waves). To use
this feature, set `group_type` to `pattern` and enter the pattern to
search for in the `group` argument.

    set.seed(0803)
    symptoms_data <-
      data.frame(
        symptoms_t1 = sample(c(0:10, -999), replace = TRUE, size = 50),
        symptoms_t2 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50),
        symptoms_t3 = sample(c(NA, 0:10, -999), replace = TRUE, size = 50)
      )

    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "_t\\d",
                   group_type = "pattern",
                   ignore = c(symptoms = -999))

    ## # A tibble: 3 × 7
    ##   variable    group  mean    sd   min   max  nobs
    ##   <chr>       <chr> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 symptoms_t1 t1     4.03  3.14     0    10    33
    ## 2 symptoms_t2 t2     5.12  3.33     0    10    33
    ## 3 symptoms_t3 t3     4.64  3.29     0    10    33

Use the `group_name` argument to give a descriptive label to the column
with matched patterns or grouping variable values, and the `var_labels`
argument to add labels to the variables in the summary table.

    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "_t\\d",
                   group_type = "pattern",
                   group_name = "time_point",
                   ignore = c(symptoms = -999), 
                   var_labels = c(
                     symptoms_t1 = "# of symptoms at baseline",
                     symptoms_t2 = "# of symptoms at 6 months follow up",
                     symptoms_t3 = "# of symptoms at one-year follow up"))

    ## # A tibble: 3 × 8
    ##   variable    variable_label            time_point  mean    sd   min   max  nobs
    ##   <chr>       <chr>                     <chr>      <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 symptoms_t1 # of symptoms at baseline t1          4.03  3.14     0    10    33
    ## 2 symptoms_t2 # of symptoms at 6 month… t2          5.12  3.33     0    10    33
    ## 3 symptoms_t3 # of symptoms at one-yea… t3          4.64  3.29     0    10    33

Finally, you can choose what information to return using the `only`
argument.

    # Default: all summary statistics returned
    # (mean, sd, min, max, nobs)
    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "_t\\d",
                   group_type = "pattern",
                   group_name = "time_point",
                   ignore = c(symptoms = -999))

    ## # A tibble: 3 × 7
    ##   variable    time_point  mean    sd   min   max  nobs
    ##   <chr>       <chr>      <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 symptoms_t1 t1          4.03  3.14     0    10    33
    ## 2 symptoms_t2 t2          5.12  3.33     0    10    33
    ## 3 symptoms_t3 t3          4.64  3.29     0    10    33

    # Means and non-missing observations only
    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "_t\\d",
                   group_type = "pattern",
                   group_name = "time_point",
                   ignore = c(symptoms = -999),
                   only = c("mean", "nobs"))

    ## # A tibble: 3 × 4
    ##   variable    time_point  mean  nobs
    ##   <chr>       <chr>      <dbl> <int>
    ## 1 symptoms_t1 t1          4.03    33
    ## 2 symptoms_t2 t2          5.12    33
    ## 3 symptoms_t3 t3          4.64    33

    # Means and standard deviations only
    mean_group_tbl(data = symptoms_data, 
                   var_stem = "symptoms",
                   group = "_t\\d",
                   group_type = "pattern",
                   group_name = "time_point",
                   ignore = c(symptoms = -999),
                   only = c("mean", "sd"))

    ## # A tibble: 3 × 4
    ##   variable    time_point  mean    sd
    ##   <chr>       <chr>      <dbl> <dbl>
    ## 1 symptoms_t1 t1          4.03  3.14
    ## 2 symptoms_t2 t2          5.12  3.33
    ## 3 symptoms_t3 t3          4.64  3.29
