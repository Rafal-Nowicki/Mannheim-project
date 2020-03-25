Unemployment and income in Germany - Case study
================

# Loading necessary libraries

``` r
library(eurostat)
library(rgdal)
```

    ## Loading required package: sp

    ## rgdal: version: 1.4-3, (SVN revision 828)
    ##  Geospatial Data Abstraction Library extensions to R successfully loaded
    ##  Loaded GDAL runtime: GDAL 2.2.3, released 2017/11/20
    ##  Path to GDAL shared files: C:/Users/rafal/OneDrive/Dokumenty/R/win-library/3.5/rgdal/gdal
    ##  GDAL binary built with GEOS: TRUE 
    ##  Loaded PROJ.4 runtime: Rel. 4.9.3, 15 August 2016, [PJ_VERSION: 493]
    ##  Path to PROJ.4 shared files: C:/Users/rafal/OneDrive/Dokumenty/R/win-library/3.5/rgdal/proj
    ##  Linking to sp version: 1.3-1

``` r
library(sp)
library(spdep)
```

    ## Loading required package: spData

    ## To access larger datasets in this package, install the spDataLarge
    ## package with: `install.packages('spDataLarge',
    ## repos='https://nowosad.github.io/drat/', type='source')`

    ## Loading required package: sf

    ## Linking to GEOS 3.6.1, GDAL 2.2.3, PROJ 4.9.3

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------------------------ tidyverse 1.2.1 --

    ## <U+221A> ggplot2 3.1.1       <U+221A> purrr   0.3.2  
    ## <U+221A> tibble  2.1.1       <U+221A> dplyr   0.8.0.1
    ## <U+221A> tidyr   0.8.3       <U+221A> stringr 1.4.0  
    ## <U+221A> readr   1.3.1       <U+221A> forcats 0.4.0

    ## -- Conflicts --------------------------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

# Now get the data

``` r
unemp <- get_eurostat("lfst_r_lfu3rt")
```

    ## Table lfst_r_lfu3rt cached at C:\Users\rafal\AppData\Local\Temp\Rtmp0kzPBL/eurostat/lfst_r_lfu3rt_date_code_TF.rds

``` r
gdp <- get_eurostat("nama_10r_2gdp")
```

    ## Table nama_10r_2gdp cached at C:\Users\rafal\AppData\Local\Temp\Rtmp0kzPBL/eurostat/nama_10r_2gdp_date_code_TF.rds

``` r
hh_inc <- get_eurostat("nama_10r_2hhinc")
```

    ## Table nama_10r_2hhinc cached at C:\Users\rafal\AppData\Local\Temp\Rtmp0kzPBL/eurostat/nama_10r_2hhinc_date_code_TF.rds

``` r
gva_grwth <- get_eurostat("nama_10r_2gvagr")
```

    ## Table nama_10r_2gvagr cached at C:\Users\rafal\AppData\Local\Temp\Rtmp0kzPBL/eurostat/nama_10r_2gvagr_date_code_TF.rds
