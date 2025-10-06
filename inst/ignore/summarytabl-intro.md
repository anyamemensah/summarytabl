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
    Unselected/Selected), multiple-response (e.g., never, sometimes,
    often), or ordinal-scale (e.g., strongly disagree to strongly agree)
    variables
3.  Continuous data, like interval (e.g., test scores) and ratio-level
    (e.g., age) variables

Functions for categorical data start with `cat_`, those for multiple
response data start with `select_`, and functions for continuous data
start with `mean_`.

To learn more about how these functions work, read the next few
sections.

## Categorical variables

### Summarize a single categorical variable

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

### Summarize a categorical variable grouped by another variable

To create a grouped frequency table for two categorical variables, use
the `cat_group_tbl()` function.

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

### Summarize a series of multiple response variables

With `select_tbl()`, you can produce a summary table for multiple
response variables with the same variable stem. A variable stem is a
common prefix found in related variable names, often corresponding to
similar survey items, that represents a shared concept before unique
identifiers.

For example, the `depressive` dataset contains eight variables that
share the same stem (i.e., `dep`), with each one representing a
different item (such as a statement, question, or indicator) used to
measure depression:

    names(depressive)

    ##  [1] "cid"   "race"  "sex"   "yob"   "dep_1" "dep_2" "dep_3" "dep_4" "dep_5"
    ## [10] "dep_6" "dep_7" "dep_8"

With the `select_tbl()` function, you can summarize these responses to
see how many survey respondents chose each answer option for every
variable:

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
a wide format. By default, the summary table is presented in the long
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

### Summarize a series of multiple response variables grouped by a variable or matching pattern

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
      )

    # longer format
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer")

    ## # A tibble: 72 × 5
    ##    variable race                   values      count percent
    ##    <chr>    <chr>                  <chr>       <int>   <dbl>
    ##  1 dep_1    Black                  hardly ever   248  0.154 
    ##  2 dep_1    Black                  often          45  0.0280
    ##  3 dep_1    Black                  sometimes     194  0.121 
    ##  4 dep_1    Hispanic               hardly ever   187  0.116 
    ##  5 dep_1    Hispanic               often          28  0.0174
    ##  6 dep_1    Hispanic               sometimes     155  0.0965
    ##  7 dep_1    Non-Black/Non-Hispanic hardly ever   374  0.233 
    ##  8 dep_1    Non-Black/Non-Hispanic often          36  0.0224
    ##  9 dep_1    Non-Black/Non-Hispanic sometimes     340  0.212 
    ## 10 dep_2    Black                  hardly ever   234  0.146 
    ## # ℹ 62 more rows

    # wider format
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "wider")

    ## # A tibble: 24 × 8
    ##    variable values   count_race_Black count_race_Hispanic count_race_Non-Black…¹
    ##    <chr>    <chr>               <int>               <int>                  <int>
    ##  1 dep_1    hardly …              248                 187                    374
    ##  2 dep_1    often                  45                  28                     36
    ##  3 dep_1    sometim…              194                 155                    340
    ##  4 dep_2    hardly …              234                 171                    312
    ##  5 dep_2    often                  49                  36                     59
    ##  6 dep_2    sometim…              204                 163                    379
    ##  7 dep_3    hardly …               19                  20                     14
    ##  8 dep_3    often                 330                 246                    586
    ##  9 dep_3    sometim…              138                 104                    150
    ## 10 dep_4    hardly …               45                  38                     87
    ## # ℹ 14 more rows
    ## # ℹ abbreviated name: ¹​`count_race_Non-Black/Non-Hispanic`
    ## # ℹ 3 more variables: percent_race_Black <dbl>, percent_race_Hispanic <dbl>,
    ## #   `percent_race_Non-Black/Non-Hispanic` <dbl>

As with `cat_group_tbl()`, you can specify which values to exclude and
how remove missing values. However, when specifying values to exclude,
use the `var_stem` argument you provide to determine which values to
exclude for variables sharing the same stem.

    # Default listwise removal: 'often' value removed from all
    # dep_ variables, and 'Non-Black/Non-Hispanic' value removed
    # from race variable
    select_group_tbl(data = dep_recoded, 
                     var_stem = "dep",
                     group = "race",
                     pivot = "longer",
                     ignore = c(dep = "often", race = "Non-Black/Non-Hispanic"))

    ## # A tibble: 31 × 5
    ##    variable race     values      count percent
    ##    <chr>    <chr>    <chr>       <int>   <dbl>
    ##  1 dep_1    Black    hardly ever    18   0.286
    ##  2 dep_1    Black    sometimes      16   0.254
    ##  3 dep_1    Hispanic hardly ever     9   0.143
    ##  4 dep_1    Hispanic sometimes      20   0.317
    ##  5 dep_2    Black    hardly ever    18   0.286
    ##  6 dep_2    Black    sometimes      16   0.254
    ##  7 dep_2    Hispanic hardly ever    13   0.206
    ##  8 dep_2    Hispanic sometimes      16   0.254
    ##  9 dep_3    Black    sometimes      34   0.540
    ## 10 dep_3    Hispanic hardly ever     8   0.127
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
    ##    variable race     values      count percent
    ##    <chr>    <chr>    <chr>       <int>   <dbl>
    ##  1 dep_1    Black    hardly ever   256  0.317 
    ##  2 dep_1    Black    sometimes     203  0.251 
    ##  3 dep_1    Hispanic hardly ever   190  0.235 
    ##  4 dep_1    Hispanic sometimes     159  0.197 
    ##  5 dep_2    Black    hardly ever   241  0.305 
    ##  6 dep_2    Black    sometimes     213  0.269 
    ##  7 dep_2    Hispanic hardly ever   172  0.217 
    ##  8 dep_2    Hispanic sometimes     165  0.209 
    ##  9 dep_3    Black    hardly ever    20  0.0680
    ## 10 dep_3    Black    sometimes     149  0.507 
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
    ##    <chr>    <chr> <chr>       <int>   <dbl>
    ##  1 dep_1    Black hardly ever   248  0.509 
    ##  2 dep_1    Black often          45  0.0924
    ##  3 dep_1    Black sometimes     194  0.398 
    ##  4 dep_2    Black hardly ever   234  0.480 
    ##  5 dep_2    Black often          49  0.101 
    ##  6 dep_2    Black sometimes     204  0.419 
    ##  7 dep_3    Black hardly ever    19  0.0390
    ##  8 dep_3    Black often         330  0.678 
    ##  9 dep_3    Black sometimes     138  0.283 
    ## 10 dep_4    Black hardly ever    45  0.0924
    ## # ℹ 14 more rows

Another application of `select_group_tbl` is summarizing responses based
on a matching pattern, such as survey time points (e.g., waves). To use
this feature, set `group_type` to `pattern` and enter the pattern to
search for in the `group` argument.

For example, the `stem_social_psych` dataset includes a set of variables
responded to by students at two different time points (“w1” and “w2”).
You can summarize the responses for one of set of these variables using
the following approach:

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

### Summarize a series of continuous variables

With the `mean_tbl()` function, you can summarize a group of continuous
variables that share the same variable stem. The resulting table
provides descriptive statistics for each variable, including the mean,
standard deviation, minimum, maximum, and the number of non-missing
values.

    mean_tbl(data = social_psy_data, 
             var_stem = "belong")

    ## # A tibble: 3 × 6
    ##   variable  mean    sd   min   max  nobs
    ##   <chr>    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_1  3.83 1.10      1     5 10105
    ## 2 belong_2  2.52 1.22      1     5 10105
    ## 3 belong_3  3.77 0.995     1     5 10105

Like the other functions in this package, you can use the `ignore`
argument to specify which values to exclude from all variables
associated with the provided variable stem.

    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             ignore = 5)

    ## # A tibble: 3 × 6
    ##   variable  mean    sd   min   max  nobs
    ##   <chr>    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_1  3.31 0.827     1     4  5752
    ## 2 belong_2  2.85 0.970     1     4  5752
    ## 3 belong_3  3.38 0.754     1     4  5752

You can also specify how missing values are removed:

    # Default listwise removal
    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             ignore = 5)

    ## # A tibble: 3 × 6
    ##   variable  mean    sd   min   max  nobs
    ##   <chr>    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_1  3.31 0.827     1     4  5752
    ## 2 belong_2  2.85 0.970     1     4  5752
    ## 3 belong_3  3.38 0.754     1     4  5752

    # Pairwise removal
    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             na_removal = "pairwise", 
             ignore = 5)

    ## # A tibble: 3 × 6
    ##   variable  mean    sd   min   max  nobs
    ##   <chr>    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_1  3.27 0.898     1     4  6855
    ## 2 belong_2  2.37 1.10      1     4  9589
    ## 3 belong_3  3.36 0.808     1     4  7633

Including variable labels in your summary table can help make the
variable names easier to interpret.

    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             na_removal = "pairwise",
             var_labels = c(
               belong_1 = "I feel like I belong at this institution",
               belong_2 = "I feel like part of the community",
               belong_3 = "I feel valued by this institution")
    )

    ## # A tibble: 3 × 7
    ##   variable variable_label                           mean    sd   min   max  nobs
    ##   <chr>    <chr>                                   <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_1 I feel like I belong at this instituti…  3.83 1.10      1     5 10138
    ## 2 belong_2 I feel like part of the community        2.52 1.22      1     5 10145
    ## 3 belong_3 I feel valued by this institution        3.77 0.995     1     5 10142

Finally, you can choose what information to return using the `only`
argument.

    # Default: all summary statistics returned
    # (mean, sd, min, max, nobs)
    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             na_removal = "pairwise")

    ## # A tibble: 3 × 6
    ##   variable  mean    sd   min   max  nobs
    ##   <chr>    <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_1  3.83 1.10      1     5 10138
    ## 2 belong_2  2.52 1.22      1     5 10145
    ## 3 belong_3  3.77 0.995     1     5 10142

    # Means and non-missing observations returned
    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             na_removal = "pairwise",
             only = c("mean", "nobs"))

    ## # A tibble: 3 × 3
    ##   variable  mean  nobs
    ##   <chr>    <dbl> <int>
    ## 1 belong_1  3.83 10138
    ## 2 belong_2  2.52 10145
    ## 3 belong_3  3.77 10142

    # Means and standard deviations  returned
    mean_tbl(data = social_psy_data, 
             var_stem = "belong",
             na_removal = "pairwise",
             only = c("mean", "sd"))

    ## # A tibble: 3 × 3
    ##   variable  mean    sd
    ##   <chr>    <dbl> <dbl>
    ## 1 belong_1  3.83 1.10 
    ## 2 belong_2  2.52 1.22 
    ## 3 belong_3  3.77 0.995

### Summarize a series of continuous variables grouped by a variable or matching pattern

With the `mean_group_tbl()` function, you can produce a summary table
for a series of continuous variables sharing the same variable stem,
grouped by another variable in your dataset or by matching a pattern in
variable names.

For example, we often want to present summary statistics for responses
by demographic variables like gender, age, or race:

    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "urm",
                   group_type = "variable")

    ## # A tibble: 4 × 7
    ##   variable               urm  mean    sd   min   max  nobs
    ##   <chr>                <dbl> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1     0  3.86 0.966     1     5   188
    ## 2 belong_belongStem_w1     1  3.89 1.03      1     5    80
    ## 3 belong_belongStem_w2     0  3.95 0.993     1     5   188
    ## 4 belong_belongStem_w2     1  3.99 1.07      1     5    80

As with `select_group_tbl()`, you can specify which values to exclude
and whether to omit missing values. Be sure to use the `var_stem`
argument you provide to determine which values to exclude for variables
sharing the same stem.

    # Default listwise removal
    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "urm",
                   ignore = c(belong_belong = 5, urm = 0)
                   )

    ## # A tibble: 2 × 7
    ##   variable               urm  mean    sd   min   max  nobs
    ##   <chr>                <dbl> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1     1  3.30 0.916     1     4    46
    ## 2 belong_belongStem_w2     1  3.37 0.974     1     4    46

    # Pairwise removal
    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "urm",
                   na_removal = "pairwise",
                   ignore = c(belong_belong = 5, urm = 0)
                   )

    ## # A tibble: 2 × 7
    ##   variable               urm  mean    sd   min   max  nobs
    ##   <chr>                <dbl> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1     1  3.38 0.887     1     4   103
    ## 2 belong_belongStem_w2     1  3.44 0.925     1     4    54

Use a list if you want to exclude several values from the same
`var_stem` or `group` variable:

    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "urm",
                   ignore = list(belong_belong = c(4,5), urm = 0)
                   )

    ## # A tibble: 2 × 7
    ##   variable               urm  mean    sd   min   max  nobs
    ##   <chr>                <dbl> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1     1  2.23 0.832     1     3    13
    ## 2 belong_belongStem_w2     1  2.15 0.987     1     3    13

Another application of `mean_group_tbl` is summarizing responses based
on a matching pattern, such as survey time points (e.g., waves). To use
this feature, set `group_type` to `pattern` and enter the pattern to
search for in the `group` argument.

    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "_w\\d",
                   group_type = "pattern")

    ## # A tibble: 2 × 7
    ##   variable             group  mean    sd   min   max  nobs
    ##   <chr>                <chr> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1 w1     3.87 0.980     1     5   270
    ## 2 belong_belongStem_w2 w2     3.97 1.02      1     5   270

Use the `group_name` argument to give a descriptive label to the column
with matched patterns or grouping variable values, and the `var_labels`
argument to add labels to the variables in the summary table.

    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "_w\\d",
                   group_type = "pattern",
                   group_name = "wave",
                   var_labels = c(
                     belong_belongStem_w1 = "I feel like I belong in computing",
                     belong_belongStem_w2 = "I feel like I belong in computing")
    )

    ## # A tibble: 2 × 8
    ##   variable             variable_label        wave   mean    sd   min   max  nobs
    ##   <chr>                <chr>                 <chr> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1 I feel like I belong… w1     3.87 0.980     1     5   270
    ## 2 belong_belongStem_w2 I feel like I belong… w2     3.97 1.02      1     5   270

Finally, you can choose what information to return using the `only`
argument.

    # Default: all summary statistics returned
    # (mean, sd, min, max, nobs)
    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "_w\\d",
                   group_type = "pattern",
                   group_name = "wave",
                   var_labels = c(
                     belong_belongStem_w1 = "I feel like I belong in computing",
                     belong_belongStem_w2 = "I feel like I belong in computing")
    )

    ## # A tibble: 2 × 8
    ##   variable             variable_label        wave   mean    sd   min   max  nobs
    ##   <chr>                <chr>                 <chr> <dbl> <dbl> <dbl> <dbl> <int>
    ## 1 belong_belongStem_w1 I feel like I belong… w1     3.87 0.980     1     5   270
    ## 2 belong_belongStem_w2 I feel like I belong… w2     3.97 1.02      1     5   270

    # Means and non-missing observations only
    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "_w\\d",
                   group_type = "pattern",
                   group_name = "wave",
                   var_labels = c(
                     belong_belongStem_w1 = "I feel like I belong in computing",
                     belong_belongStem_w2 = "I feel like I belong in computing"),
                   only = c("mean", "nobs")
    )

    ## # A tibble: 2 × 5
    ##   variable             variable_label                    wave   mean  nobs
    ##   <chr>                <chr>                             <chr> <dbl> <int>
    ## 1 belong_belongStem_w1 I feel like I belong in computing w1     3.87   270
    ## 2 belong_belongStem_w2 I feel like I belong in computing w2     3.97   270

    # Means and standard deviations only
    mean_group_tbl(data = stem_social_psych, 
                   var_stem = "belong_belong",
                   group = "_w\\d",
                   group_type = "pattern",
                   group_name = "wave",
                   var_labels = c(
                     belong_belongStem_w1 = "I feel like I belong in computing",
                     belong_belongStem_w2 = "I feel like I belong in computing"),
                   only = c("mean", "sd")
    )

    ## # A tibble: 2 × 5
    ##   variable             variable_label                    wave   mean    sd
    ##   <chr>                <chr>                             <chr> <dbl> <dbl>
    ## 1 belong_belongStem_w1 I feel like I belong in computing w1     3.87 0.980
    ## 2 belong_belongStem_w2 I feel like I belong in computing w2     3.97 1.02
