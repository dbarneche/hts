# Hypothesis testing in metabolic scaling

[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![Ask Us Anything
\!](https://img.shields.io/badge/Ask%20us-anything-1abc9c.svg)](https://github.com/dbarneche/pulseChaseDorset/issues/new)
![Open Source
Love](https://badges.frapsoft.com/os/v2/open-source.svg?v=103)

This repository contains code and data needed to reproduce the working draft:

**Barneche DR, Moeller HV, Bernhardt JR**, Hypothesis testing in metabolic scaling (in preparation)

## Instructions

All figures were drawn in `R` and are defined in `figs.R`. This routine loads multiple packages which are found at the top of `figs.R`, **so make sure to successfully install and load them before running the code**. This script should take only a few minutes depending on your computing power. Figures will be automatically placed in a directory called `output` (it is going to be automatically created for you).

### These figures were produced using the following software and associated packages:
```
R version 4.2.2 (2022-10-31)
Platform: aarch64-apple-darwin20 (64-bit)
Running under: macOS Ventura 13.2.1

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/lib/libRlapack.dylib

locale:
[1] en_AU.UTF-8/en_AU.UTF-8/en_AU.UTF-8/C/en_AU.UTF-8/en_AU.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] brms_2.18.0     Rcpp_1.0.10     patchwork_1.1.2 forcats_0.5.2   stringr_1.5.0  
 [6] dplyr_1.1.0     purrr_1.0.1     readr_2.1.3     tidyr_1.3.0     tibble_3.1.8   
[11] ggplot2_3.4.1   tidyverse_1.3.2

loaded via a namespace (and not attached):
  [1] TH.data_1.1-1        googledrive_2.0.0    colorspace_2.1-0     ellipsis_0.3.2      
  [5] estimability_1.4.1   markdown_1.5         base64enc_0.1-3      fs_1.6.1            
  [9] farver_2.1.1         rstan_2.21.8         DT_0.27              fansi_1.0.4         
 [13] mvtnorm_1.1-3        lubridate_1.9.0      xml2_1.3.3           splines_4.2.2       
 [17] bridgesampling_1.1-2 codetools_0.2-18     shinythemes_1.2.0    bayesplot_1.10.0    
 [21] jsonlite_1.8.4       broom_1.0.1          dbplyr_2.2.1         shiny_1.7.4         
 [25] compiler_4.2.2       httr_1.4.4           emmeans_1.8.3        backports_1.4.1     
 [29] assertthat_0.2.1     Matrix_1.5-1         fastmap_1.1.0        gargle_1.2.1        
 [33] cli_3.6.0            later_1.3.0          htmltools_0.5.4      prettyunits_1.1.1   
 [37] tools_4.2.2          igraph_1.4.0         coda_0.19-4          gtable_0.3.1        
 [41] glue_1.6.2           reshape2_1.4.4       posterior_1.4.0      cellranger_1.1.0    
 [45] vctrs_0.5.2          nlme_3.1-160         crosstalk_1.2.0      tensorA_0.36.2      
 [49] ps_1.7.2             rvest_1.0.3          timechange_0.1.1     mime_0.12           
 [53] miniUI_0.1.1.1       lifecycle_1.0.3      gtools_3.9.4         googlesheets4_1.0.1 
 [57] MASS_7.3-58.1        zoo_1.8-11           scales_1.2.1         colourpicker_1.2.0  
 [61] ragg_1.2.4           hms_1.1.2            promises_1.2.0.1     Brobdingnag_1.2-9   
 [65] parallel_4.2.2       sandwich_3.0-2       inline_0.3.19        shinystan_2.6.0     
 [69] gridExtra_2.3        loo_2.5.1            StanHeaders_2.21.0-7 stringi_1.7.12      
 [73] dygraphs_1.1.1.6     checkmate_2.1.0      pkgbuild_1.4.0       systemfonts_1.0.4   
 [77] rlang_1.0.6          pkgconfig_2.0.3      matrixStats_0.63.0   distributional_0.3.1
 [81] lattice_0.20-45      labeling_0.4.2       rstantools_2.2.0     htmlwidgets_1.6.1   
 [85] tidyselect_1.2.0     processx_3.8.0       plyr_1.8.8           magrittr_2.0.3      
 [89] R6_2.5.1             generics_0.1.3       multcomp_1.4-20      DBI_1.1.3           
 [93] pillar_1.8.1         haven_2.5.1          withr_2.5.0          xts_0.13.0          
 [97] survival_3.4-0       abind_1.4-5          modelr_0.1.10        crayon_1.5.2        
[101] utf8_1.2.3           tzdb_0.3.0           grid_4.2.2           readxl_1.4.1        
[105] callr_3.7.3          threejs_0.3.3        reprex_2.0.2         digest_0.6.31       
[109] xtable_1.8-4         httpuv_1.6.9         textshaping_0.3.6    RcppParallel_5.1.6  
[113] stats4_4.2.2         munsell_0.5.0        shinyjs_2.1.0       
```

## License

This repository is provided by the authors under the MIT License ([MIT](http://opensource.org/licenses/MIT)).

### How to download this project for people not familiar with GitHub:  
* on the project main page on GitHub, click on the green button `clone or download` and then click on `Download ZIP`  

## Bug reporting
* Please [report any issues or bugs](https://github.com/dbarneche/hts/issues).
