# predictNMBsim - summary (default) works

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

# predictNMBsim - summary (rename_vector) works

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

# predictNMBsim - summary (what) works

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

# predictNMBsim - summary (agg_functions) works

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

# predictNMBscreen - summary works

      sim_auc fx_nmb_training all_median   all_95% CI none_median  none_95% CI
    1     0.7              f1       -1.2 -1.2 to -1.2        -0.4 -0.5 to -0.3
    2     0.7              f2       -1.2 -1.2 to -1.2        -0.4 -0.5 to -0.3
    3     0.8              f1       -1.2 -1.2 to -1.2        -0.4 -0.5 to -0.3
    4     0.8              f2       -1.2 -1.2 to -1.2        -0.4 -0.5 to -0.3
    5     0.9              f1       -1.2 -1.2 to -1.2        -0.4 -0.5 to -0.3
    6     0.9              f2       -1.2 -1.2 to -1.2        -0.4 -0.5 to -0.3
      value optimising_median value optimising_95% CI youden_median youden_95% CI
    1                   -0.40            -0.5 to -0.3         -0.64  -0.9 to -0.4
    2                   -0.40            -0.5 to -0.3         -0.64  -0.9 to -0.4
    3                   -0.40            -0.5 to -0.3         -0.56  -0.8 to -0.4
    4                   -0.40            -0.5 to -0.3         -0.56  -0.8 to -0.4
    5                   -0.38            -0.5 to -0.3         -0.47  -0.7 to -0.4
    6                   -0.38            -0.4 to -0.3         -0.47  -0.6 to -0.4
      cost minimising_median cost minimising_95% CI prod sens spec_median
    1                  -0.40           -0.5 to -0.3                 -0.64
    2                  -0.40           -0.5 to -0.3                 -0.63
    3                  -0.40           -0.5 to -0.3                 -0.55
    4                  -0.40           -0.5 to -0.3                 -0.56
    5                  -0.38           -0.5 to -0.3                 -0.47
    6                  -0.38           -0.4 to -0.3                 -0.47
      prod sens spec_95% CI roc01_median roc01_95% CI index of union_median
    1          -0.8 to -0.5        -0.64 -0.8 to -0.5                 -0.64
    2          -0.8 to -0.5        -0.63 -0.8 to -0.5                 -0.64
    3          -0.7 to -0.4        -0.55 -0.7 to -0.4                 -0.55
    4          -0.7 to -0.4        -0.56 -0.7 to -0.4                 -0.56
    5          -0.6 to -0.4        -0.47 -0.6 to -0.4                 -0.47
    6          -0.6 to -0.4        -0.46 -0.6 to -0.4                 -0.46
      index of union_95% CI
    1          -0.8 to -0.5
    2          -0.8 to -0.5
    3          -0.7 to -0.4
    4          -0.7 to -0.5
    5          -0.6 to -0.4
    6          -0.6 to -0.4

---

      sim_auc fx_nmb_training Treat All_mean Treat All_min Treat All_max
    1     0.7              f1      -1.199592        -1.268        -1.130
    2     0.7              f2      -1.198992        -1.256        -1.144
    3     0.8              f1      -1.200352        -1.264        -1.150
    4     0.8              f2      -1.199896        -1.256        -1.142
    5     0.9              f1      -1.199988        -1.260        -1.156
    6     0.9              f2      -1.200256        -1.254        -1.142
      Treat None_mean Treat None_min Treat None_max value optimising_mean
    1       -0.399184         -0.536         -0.260             -0.408276
    2       -0.397984         -0.512         -0.288             -0.408688
    3       -0.400704         -0.528         -0.300             -0.400800
    4       -0.399792         -0.512         -0.284             -0.400154
    5       -0.399976         -0.520         -0.312             -0.382228
    6       -0.400512         -0.508         -0.284             -0.382186
      value optimising_min value optimising_max youden_mean youden_min youden_max
    1               -1.245               -0.264   -0.650752     -0.975     -0.371
    2               -1.225               -0.296   -0.645796     -1.003     -0.373
    3               -0.518               -0.298   -0.567532     -0.887     -0.347
    4               -0.507               -0.301   -0.572142     -0.933     -0.351
    5               -0.502               -0.298   -0.480682     -0.807     -0.328
    6               -0.475               -0.278   -0.474018     -0.811     -0.308
      cost minimising threshold_mean cost minimising threshold_min
    1                      -0.400036                        -0.539
    2                      -0.398764                        -0.512
    3                      -0.398862                        -0.518
    4                      -0.398204                        -0.500
    5                      -0.380126                        -0.492
    6                      -0.379994                        -0.482
      cost minimising threshold_max prod sens spec_mean prod sens spec_min
    1                        -0.264           -0.641890             -0.904
    2                        -0.289           -0.630782             -0.874
    3                        -0.299           -0.558626             -0.807
    4                        -0.300           -0.566424             -0.815
    5                        -0.294           -0.474916             -0.763
    6                        -0.281           -0.470020             -0.701
      prod sens spec_max roc01_mean roc01_min roc01_max index of union_mean
    1             -0.390  -0.638180    -0.903    -0.439           -0.640204
    2             -0.373  -0.632544    -0.874    -0.433           -0.638788
    3             -0.347  -0.554978    -0.776    -0.385           -0.558036
    4             -0.351  -0.562972    -0.808    -0.351           -0.565276
    5             -0.333  -0.470748    -0.697    -0.330           -0.470538
    6             -0.308  -0.463606    -0.655    -0.327           -0.465722
      index of union_min index of union_max
    1             -0.836             -0.479
    2             -0.838             -0.462
    3             -0.727             -0.386
    4             -0.805             -0.406
    5             -0.663             -0.330
    6             -0.691             -0.312

---

      n_sims n_valid fx_nmb_training      fx_nmb_evaluation sample_size sim_auc
    1    500    1000              f1 unnamed-nmb-function-1         189     0.7
    2    500    1000              f2 unnamed-nmb-function-1         189     0.7
    3    500    1000              f1 unnamed-nmb-function-1         139     0.8
    4    500    1000              f2 unnamed-nmb-function-1         139     0.8
    5    500    1000              f1 unnamed-nmb-function-1         139     0.9
    6    500    1000              f2 unnamed-nmb-function-1         139     0.9
      event_rate min_events meet_min_events .sim_id all_median   all_95% CI
    1        0.1         19            TRUE       1       -1.2 -1.2 to -1.2
    2        0.1         19            TRUE       2       -1.2 -1.2 to -1.2
    3        0.1         14            TRUE       3       -1.2 -1.2 to -1.2
    4        0.1         14            TRUE       4       -1.2 -1.2 to -1.2
    5        0.1         14            TRUE       5       -1.2 -1.2 to -1.2
    6        0.1         14            TRUE       6       -1.2 -1.2 to -1.2
      none_median  none_95% CI value optimising_median value optimising_95% CI
    1        -0.4 -0.5 to -0.3                   -0.40            -0.5 to -0.3
    2        -0.4 -0.5 to -0.3                   -0.40            -0.5 to -0.3
    3        -0.4 -0.5 to -0.3                   -0.40            -0.5 to -0.3
    4        -0.4 -0.5 to -0.3                   -0.40            -0.5 to -0.3
    5        -0.4 -0.5 to -0.3                   -0.38            -0.5 to -0.3
    6        -0.4 -0.5 to -0.3                   -0.38            -0.4 to -0.3
      youden_median youden_95% CI cost minimising_median cost minimising_95% CI
    1         -0.64  -0.9 to -0.4                  -0.40           -0.5 to -0.3
    2         -0.64  -0.9 to -0.4                  -0.40           -0.5 to -0.3
    3         -0.56  -0.8 to -0.4                  -0.40           -0.5 to -0.3
    4         -0.56  -0.8 to -0.4                  -0.40           -0.5 to -0.3
    5         -0.47  -0.7 to -0.4                  -0.38           -0.5 to -0.3
    6         -0.47  -0.6 to -0.4                  -0.38           -0.4 to -0.3
      prod sens spec_median prod sens spec_95% CI roc01_median roc01_95% CI
    1                 -0.64          -0.8 to -0.5        -0.64 -0.8 to -0.5
    2                 -0.63          -0.8 to -0.5        -0.63 -0.8 to -0.5
    3                 -0.55          -0.7 to -0.4        -0.55 -0.7 to -0.4
    4                 -0.56          -0.7 to -0.4        -0.56 -0.7 to -0.4
    5                 -0.47          -0.6 to -0.4        -0.47 -0.6 to -0.4
    6                 -0.47          -0.6 to -0.4        -0.46 -0.6 to -0.4
      index of union_median index of union_95% CI
    1                 -0.64          -0.8 to -0.5
    2                 -0.64          -0.8 to -0.5
    3                 -0.55          -0.7 to -0.4
    4                 -0.56          -0.7 to -0.5
    5                 -0.47          -0.6 to -0.4
    6                 -0.46          -0.6 to -0.4

