# DD360 Challenge. Román A Vélez Jiménez. :house:

The challenge is to infer the prices of $$m^2$$ for certain properties in the Mexican Republic. On the other hand, in presenting or a personal ML project that has been carried out, or, carrying out a study through NLP techniques to find the most important themes in reviews of various restaurants.

In my case, I chose to display a personal ML project. For this reason, the presentation is not available in the repository but you can ask for it directly at roman.velez.jimenez@gmail.com.

# Programming languagues :computer:

The programming language R 4.1.3 (one-push-up) and the following packages were used:

```
R version 4.1.3 (2022-03-10)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS 13.1

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] sf_1.0-9             syuzhet_1.0.6        wordcloud_2.6        RColorBrewer_1.1-3  
 [5] SnowballC_0.7.0      stopwords_2.3        tm_0.7-11            NLP_0.2-1           
 [9] stringi_1.7.8        caret_6.0-93         lattice_0.20-45      ggrepel_0.9.1       
[13] glmnet_4.1-6         Matrix_1.4-0         gbm_2.1.8.1          randomForest_4.7-1.1
[17] tree_1.0-43          forecast_8.16        forcats_0.5.1        stringr_1.4.1       
[21] dplyr_1.0.10         purrr_0.3.5          readr_2.1.2          tidyr_1.2.1         
[25] tibble_3.1.8         ggplot2_3.4.0        tidyverse_1.3.1     

loaded via a namespace (and not attached):
  [1] colorspace_2.0-3     ISOcodes_2022.09.29  ellipsis_0.3.2       class_7.3-20        
  [5] rprojroot_2.0.3      fs_1.5.2             proxy_0.4-27         rstudioapi_0.13     
  [9] listenv_0.9.0        farver_2.1.1         bit64_4.0.5          prodlim_2019.11.13  
 [13] fansi_1.0.3          lubridate_1.9.0      xml2_1.3.3           codetools_0.2-18    
 [17] splines_4.1.3        pkgload_1.2.4        jsonlite_1.8.0       pROC_1.18.0         
 [21] broom_1.0.1          dbplyr_2.1.1         compiler_4.1.3       httr_1.4.2          
 [25] backports_1.4.1      assertthat_0.2.1     cli_3.4.1            tools_4.1.3         
 [29] gtable_0.3.1         glue_1.6.2           reshape2_1.4.4       Rcpp_1.0.8.3        
 [33] slam_0.1-50          cellranger_1.1.0     fracdiff_1.5-1       vctrs_0.5.0         
 [37] urca_1.3-0           nlme_3.1-155         iterators_1.0.14     lmtest_0.9-40       
 [41] timeDate_3043.102    gower_1.0.1          globals_0.16.2       brio_1.1.3          
 [45] testthat_3.1.3       rvest_1.0.2          timechange_0.1.1     lifecycle_1.0.3     
 [49] future_1.31.0        MASS_7.3-55          zoo_1.8-10           scales_1.2.1        
 [53] ipred_0.9-13         vroom_1.5.7          hms_1.1.1            parallel_4.1.3      
 [57] quantmod_0.4.18      curl_4.3.2           rpart_4.1.16         tseries_0.10-50     
 [61] desc_1.4.1           foreach_1.5.2        e1071_1.7-13         TTR_0.24.3          
 [65] hardhat_1.2.0        lava_1.7.1           shape_1.4.6          rlang_1.0.6         
 [69] pkgconfig_2.0.3      recipes_1.0.4        labeling_0.4.2       bit_4.0.4           
 [73] tidyselect_1.2.0     parallelly_1.34.0    plyr_1.8.8           magrittr_2.0.3      
 [77] R6_2.5.1             generics_0.1.3       DBI_1.1.2            pillar_1.8.1        
 [81] haven_2.4.3          withr_2.5.0          mgcv_1.8-39          units_0.8-1         
 [85] xts_0.12.1           survival_3.4-0       nnet_7.3-17          future.apply_1.10.0
 [89] modelr_0.1.8         crayon_1.5.1         KernSmooth_2.23-20   utf8_1.2.2          
 [93] tzdb_0.3.0           grid_4.1.3           readxl_1.3.1         data.table_1.14.2   
 [97] NCmisc_1.2.0         ModelMetrics_1.2.2.2 classInt_0.4-8       reprex_2.0.1        
[101] digest_0.6.30        stats4_4.1.3         munsell_0.5.0        viridisLite_0.4.1   
[105] quadprog_1.5-8     
```

# Replicate work :sunglasses:

1. **Wrangle**. In the file `src/wrangling.R` the info from `data` is joined and wrangled to use it later in the statistical models.

2. **Modeling**. In the file `src/pricing_models.R` the prices per square meter are infered given some characteristics in the previously wrangled data.

*Note: Some personal notes are founded at `src/notes.txt`.


# External Information :bar_chart:

There were three public data sets used in order to accomplish the main task.
  - [Social Lag from CONEVAL](!https://www.coneval.org.mx/Medicion/IRS/Paginas/Rezago_social_AGEB_2020.aspx).
  - [Mexico Geostatsitcal 2022 Framework](!https://www.inegi.org.mx/temas/mg/#Descargas)
  - [Mexican 2020 Census by AGEB](!https://www.inegi.org.mx/programas/ccpv/2020/#Microdatos)
