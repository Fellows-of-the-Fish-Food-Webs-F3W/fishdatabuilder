
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fishdatabuilder

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/fishdatabuilder)](https://CRAN.R-project.org/package=fishdatabuilder)
<!-- badges: end -->

The goal of fishdatabuilder is to extract the fish monitoring data
collected within the European Water Framework Directive (WFD).

## Installation

<!--You can install the released version of fishdatabuilder from [CRAN](https://CRAN.R-project.org) with:-->
<!--``` r-->
<!--install.packages("fishdatabuilder")-->
<!--```-->
<!--And the development version from [GitHub](https://github.com/) with:-->

You can install the released version of fishdatabuilder from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Fellows-of-the-Fish-Food-Webs-F3W/fishdatabuilder")
```

## Starting

We can extract the french fish monitoring data up to 2022. Those data
have been archived on [Zenodo](https://zenodo.org/records/8099409) at
the occasion of the publication of the
[aspe](https://github.com/PascalIrz/aspe) R package used to manipulate
those data.

The first thing to do with the package is to download the zip database
and to extract it:

``` r
library(fishdatabuilder)
# Download the data
download_optional_data()
# Extract the data
extract_optional_data()
```

The data are located in the cache folder of the package, using the
[`pkgfilecache`](https://cran.r-project.org/web/packages/pkgfilecache/)
R package.

Now that you did that, you should be able to list and load the files in
the cache of the package:

In development mode:

``` r
library(devtools)
#> Le chargement a nécessité le package : usethis
library(pkgfilecache)
load_all()
#> ℹ Loading fishdatabuilder
document()
#> ℹ Updating fishdatabuilder documentation
#> ℹ Loading fishdatabuilder
```

``` r
head(list_optional_files())
#> [1] "ambiance.csv"          "autres_especes.csv"    "bassin_simp.csv"      
#> [4] "classe_ipr_plus.csv"   "classe_ipr.csv"        "code_wama_commune.csv"
```

You can get station data, clean them, and convert all of them in the
same coordinates:

``` r
station <- clean_station_aspe(
  station = get_raw_station_aspe(),
  ref_coordinates = get_raw_ref_coordinates_station_aspe(),
  crs_to = 4326
  )
#> Warning: There was 1 warning in `dplyr::mutate()`.
#> ℹ In argument: `data = purrr::map2(data, typ_code_epsg, convert_crs,
#>   crs_to = crs_to)`.
#> ℹ In group 4: `typ_code_epsg = 2989`.
#> Caused by warning in `CPL_crs_from_input()`:
#> ! GDAL Message 1: CRS EPSG:2989 is deprecated. Its non-deprecated replacement EPSG:4559 will be used instead. To use the original CRS, set the OSR_USE_NON_DEPRECATED configuration option to NO.
head(station)
#>   site_id          x        y
#> 1    1216  6.1403141 48.60643
#> 2    1819  7.0324279 48.98280
#> 3   47594  0.3320969 44.41637
#> 4   12559 -2.0061139 48.36974
#> 5   12830 -3.4009409 48.51967
#> 6   12987 -3.9799718 48.48472
```

``` r
# Cleaned operation files:
op <- clean_operation_aspe()
head(op)
#>   operation_id site_id       date                                objective
#> 1        87711   21356 2022-06-21 RCS – Réseau de Contrôle de Surveillance
#> 2        87711   21356 2022-06-21        RRP – Réseau de Référence Pérenne
#> 3        86077    7003 2021-08-31 RCS – Réseau de Contrôle de Surveillance
#> 4        86230    5443 2021-09-22 RCS – Réseau de Contrôle de Surveillance
#> 5        87257    5690 2021-08-06 RCS – Réseau de Contrôle de Surveillance
#> 6        87255    5953 2021-09-30 RCS – Réseau de Contrôle de Surveillance
#>           protocol without_fish computed_surface           date_time
#> 1         complete        FALSE            897.0 2022-06-21 09:30:00
#> 2         complete        FALSE            897.0 2022-06-21 09:30:00
#> 3 partial_by_point        FALSE            937.5 2021-08-31 09:30:00
#> 4 partial_by_point        FALSE           1250.0 2021-09-22 10:00:00
#> 5         complete        FALSE            823.2 2021-08-06 08:15:00
#> 6         complete        FALSE            319.2 2021-09-30 13:00:00

# Cleaned description of operation files:
op_description <- clean_description_operation_aspe()
head(op_description)
#>   operation_id time_fishing upstream_isolation downstream_isolation width_river
#> 1        38610            0                net           no_barrier        6.00
#> 2        38612            0                net           no_barrier        6.00
#> 3         4402           90         no_barrier           no_barrier        2.70
#> 4        38613            0                net           no_barrier        6.00
#> 5        38623           75               <NA>                 <NA>       50.00
#> 6        38631            0         no_barrier           no_barrier        3.22
#>   width_left_shore length_left_shore width_right_shore length_right_shore
#> 1               NA                NA                NA                 NA
#> 2               NA                NA                NA                 NA
#> 3                0                 0                 0                  0
#> 4               NA                NA                NA                 NA
#> 5               NA                NA                NA                 NA
#> 6               NA                NA                NA                 NA
#>   length_sourced instant_temperature
#> 1            150                  NA
#> 2            150                  NA
#> 3            172                14.5
#> 4            150                  NA
#> 5            600                  NA
#> 6             86                  NA

# Cleaned elementary sampling
elementary_sampling <- cleaning_elementary_sampling(
  sampling = get_elementary_sampling_aspe(),
  ref_sampling = get_ref_elementary_sampling_aspe(),
  ref_passage = get_ref_passage_aspe()
)
head(elementary_sampling)
#>   prelevement_id operation_id prelevement_type passage_number
#> 1              1         1966          passage              1
#> 2              2         1966          passage              2
#> 3              3         1971          passage              1
#> 4              4         1971          passage              2
#> 5              5         1983          passage              1
#> 6              6         1983          passage              2

# Cleaning point group
point_group <- cleaning_point_group(
  point_group = get_point_group_aspe(),
  ref_point_group = get_ref_point_group_aspe()
)
#> Joining with `by = join_by(grp_tgp_id)`
head(point_group)
#>   grp_id          point_type grp_nombre grp_nombre_points_sans_poisson
#> 1   7169 complementary_point          1                              0
#> 2   7563      standard_point         75                             37
#> 3   7042      standard_point        100                             42
#> 4   7474 complementary_point          2                              0
#> 5   7624      standard_point         75                             23
#> 6   7087 complementary_point          3                              0
#>   grp_nombre_points_facies_profond grp_nombre_points_facies_courant
#> 1                                1                                0
#> 2                                2                               12
#> 3                              100                                0
#> 4                                0                                0
#> 5                               31                               13
#> 6                                3                                0
#>   grp_nombre_points_facies_plat grp_nombre_points_berge
#> 1                             0                       1
#> 2                            61                      65
#> 3                             0                     100
#> 4                             1                       2
#> 5                            31                      49
#> 6                             0                       1
#>   grp_nombre_points_chenal grp_nombre_points_annexe
#> 1                        0                        0
#> 2                       10                        0
#> 3                        0                        0
#> 4                        0                        1
#> 5                       26                        0
#> 6                        2                        0
```
