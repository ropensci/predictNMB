# predictNMBsim - make_summary_table (default) works

    # A tibble: 8 x 3
      method           median `95% CI`    
      <chr>             <dbl> <chr>       
    1 all               -1.19 -1.2 to -1.2
    2 cost minimising   -0.39 -0.5 to -0.4
    3 index of union    -0.61 -0.7 to -0.6
    4 none              -0.39 -0.4 to -0.4
    5 prod sens spec    -0.6  -0.7 to -0.6
    6 roc01             -0.6  -0.7 to -0.6
    7 value optimising  -0.39 -0.4 to -0.4
    8 youden            -0.59 -0.7 to -0.5

# predictNMBsim - make_summary_table (rename_vector) works

    # A tibble: 8 x 3
      method           median `95% CI`    
      <chr>             <dbl> <chr>       
    1 Treat All         -1.19 -1.2 to -1.2
    2 Treat None        -0.39 -0.4 to -0.4
    3 cost minimising   -0.39 -0.5 to -0.4
    4 index of union    -0.61 -0.7 to -0.6
    5 prod sens spec    -0.6  -0.7 to -0.6
    6 roc01             -0.6  -0.7 to -0.6
    7 value optimising  -0.39 -0.4 to -0.4
    8 youden            -0.59 -0.7 to -0.5

# predictNMBsim - make_summary_table (what) works

    # A tibble: 8 x 3
      method           median `95% CI`  
      <chr>             <dbl> <chr>     
    1 all                0    0 to 0    
    2 cost minimising    0.5  0.5 to 0.5
    3 index of union     0.12 0.1 to 0.2
    4 none               1    1 to 1    
    5 prod sens spec     0.12 0.1 to 0.2
    6 roc01              0.12 0.1 to 0.2
    7 value optimising   0.54 0.4 to 1  
    8 youden             0.13 0.1 to 0.2

---

    # A tibble: 7 x 3
      method           median `95% CI`  
      <chr>             <dbl> <chr>     
    1 cost minimising    0.8  0.8 to 0.8
    2 index of union     0.6  0.5 to 0.6
    3 none               0.81 0.8 to 0.8
    4 prod sens spec     0.59 0.5 to 0.6
    5 roc01              0.6  0.5 to 0.6
    6 value optimising   0.8  0.8 to 0.8
    7 youden             0.6  0.5 to 0.7

# predictNMBsim - make_summary_table (agg_functions) works

    # A tibble: 8 x 4
      method             mean    min    max
      <chr>             <dbl>  <dbl>  <dbl>
    1 all              -1.20  -1.22  -1.18 
    2 cost minimising  -0.395 -0.464 -0.356
    3 index of union   -0.616 -0.694 -0.55 
    4 none             -0.391 -0.436 -0.356
    5 prod sens spec   -0.624 -0.71  -0.55 
    6 roc01            -0.619 -0.699 -0.55 
    7 value optimising -0.394 -0.445 -0.356
    8 youden           -0.593 -0.71  -0.475

# predictNMBscreen - make_summary_table works

    # A tibble: 6 x 18
      sim_auc fx_nmb_train~1 all_m~2 all_9~3 none_~4 none_~5 value~6 value~7 youde~8
        <dbl> <chr>            <dbl> <chr>     <dbl> <chr>     <dbl> <chr>     <dbl>
    1     0.7 f1                -1.2 -1.2 t~    -0.4 -0.5 t~   -0.4  -0.5 t~   -0.64
    2     0.7 f2                -1.2 -1.2 t~    -0.4 -0.5 t~   -0.4  -0.5 t~   -0.64
    3     0.8 f1                -1.2 -1.2 t~    -0.4 -0.5 t~   -0.4  -0.5 t~   -0.56
    4     0.8 f2                -1.2 -1.2 t~    -0.4 -0.5 t~   -0.4  -0.5 t~   -0.56
    5     0.9 f1                -1.2 -1.2 t~    -0.4 -0.5 t~   -0.38 -0.5 t~   -0.47
    6     0.9 f2                -1.2 -1.2 t~    -0.4 -0.5 t~   -0.38 -0.4 t~   -0.47
    # ... with 9 more variables: `youden_95% CI` <chr>,
    #   `cost minimising_median` <dbl>, `cost minimising_95% CI` <chr>,
    #   `prod sens spec_median` <dbl>, `prod sens spec_95% CI` <chr>,
    #   roc01_median <dbl>, `roc01_95% CI` <chr>, `index of union_median` <dbl>,
    #   `index of union_95% CI` <chr>, and abbreviated variable names
    #   1: fx_nmb_training, 2: all_median, 3: `all_95% CI`, 4: none_median,
    #   5: `none_95% CI`, 6: `value optimising_median`, ...

---

    # A tibble: 6 x 26
      sim_auc fx_nmb_train~1 Treat~2 Treat~3 Treat~4 Treat~5 Treat~6 Treat~7 value~8
        <dbl> <chr>            <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    1     0.7 f1               -1.20   -1.27   -1.13  -0.399  -0.536  -0.26   -0.408
    2     0.7 f2               -1.20   -1.26   -1.14  -0.398  -0.512  -0.288  -0.409
    3     0.8 f1               -1.20   -1.26   -1.15  -0.401  -0.528  -0.3    -0.401
    4     0.8 f2               -1.20   -1.26   -1.14  -0.400  -0.512  -0.284  -0.400
    5     0.9 f1               -1.20   -1.26   -1.16  -0.400  -0.52   -0.312  -0.382
    6     0.9 f2               -1.20   -1.25   -1.14  -0.401  -0.508  -0.284  -0.382
    # ... with 17 more variables: `value optimising_min` <dbl>,
    #   `value optimising_max` <dbl>, youden_mean <dbl>, youden_min <dbl>,
    #   youden_max <dbl>, `cost minimising_mean` <dbl>,
    #   `cost minimising_min` <dbl>, `cost minimising_max` <dbl>,
    #   `prod sens spec_mean` <dbl>, `prod sens spec_min` <dbl>,
    #   `prod sens spec_max` <dbl>, roc01_mean <dbl>, roc01_min <dbl>,
    #   roc01_max <dbl>, `index of union_mean` <dbl>, ...

---

    # A tibble: 6 x 26
      n_sims n_valid fx_nm~1 fx_nm~2 sampl~3 sim_auc event~4 min_e~5 meet_~6 .sim_id
       <dbl>   <dbl> <chr>   <chr>     <dbl>   <dbl>   <dbl>   <dbl> <lgl>     <int>
    1    500    1000 f1      unname~     189     0.7     0.1      19 TRUE          1
    2    500    1000 f2      unname~     189     0.7     0.1      19 TRUE          2
    3    500    1000 f1      unname~     139     0.8     0.1      14 TRUE          3
    4    500    1000 f2      unname~     139     0.8     0.1      14 TRUE          4
    5    500    1000 f1      unname~     139     0.9     0.1      14 TRUE          5
    6    500    1000 f2      unname~     139     0.9     0.1      14 TRUE          6
    # ... with 16 more variables: all_median <dbl>, `all_95% CI` <chr>,
    #   none_median <dbl>, `none_95% CI` <chr>, `value optimising_median` <dbl>,
    #   `value optimising_95% CI` <chr>, youden_median <dbl>,
    #   `youden_95% CI` <chr>, `cost minimising_median` <dbl>,
    #   `cost minimising_95% CI` <chr>, `prod sens spec_median` <dbl>,
    #   `prod sens spec_95% CI` <chr>, roc01_median <dbl>, `roc01_95% CI` <chr>,
    #   `index of union_median` <dbl>, `index of union_95% CI` <chr>, and ...

