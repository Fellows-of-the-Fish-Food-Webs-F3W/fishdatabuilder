
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

``` r
library(pkgfilecache)
pkg_info <- get_pkg_info("fishdatabuilder")
file_names <- list_available(pkg_info)
file_names
#>   [1] "ambiance.csv"                            "autres_especes.csv"                      "bassin_simp.csv"                        
#>   [4] "classe_ipr_plus.csv"                     "classe_ipr.csv"                          "code_wama_commune.csv"                  
#>   [7] "code_wama_riviere.csv"                   "commune.csv"                             "departement.csv"                        
#>  [10] "dispositif_collecte.csv"                 "dispositif_point.csv"                    "dispositif_station.csv"                 
#>  [13] "facies.csv"                              "groupe_points.csv"                       "habitat_anguilles.csv"                  
#>  [16] "habitat.csv"                             "historique_environnemental_pp.csv"       "intervenant_departement.csv"            
#>  [19] "libelles.csv"                            "localisation_point_wama.csv"             "lot_poissons.csv"                       
#>  [22] "maj_objectif.csv"                        "map_pointprelmetier.csv"                 "materiels.csv"                          
#>  [25] "mesure_individuelle.csv"                 "operation_description_peche.csv"         "operation_donnees_environnementales.csv"
#>  [28] "operation_ipr_plus.csv"                  "operation_ipr.csv"                       "operation_objectif.csv"                 
#>  [31] "operation_suivi.csv"                     "operation.csv"                           "parametres_globaux.csv"                 
#>  [34] "parametres.csv"                          "passage.csv"                             "pathologie_lot.csv"                     
#>  [37] "pathologie_poisson.csv"                  "point_prelevement.csv"                   "poste_intervenant.csv"                  
#>  [40] "prelevement_elementaire.csv"             "probabilite_presence_ipr_plus.csv"       "probabilite_presence_ipr.csv"           
#>  [43] "profil_droit.csv"                        "ref_accessibilite.csv"                   "ref_bassin.csv"                         
#>  [46] "ref_categorie_piscicole.csv"             "ref_classe_vitesse_courant.csv"          "ref_colmatage.csv"                      
#>  [49] "ref_condition_hydrologique.csv"          "ref_droit.csv"                           "ref_entite_hydrographique.csv"          
#>  [52] "ref_espece.csv"                          "ref_etat_avancement.csv"                 "ref_fabricant_materiel.csv"             
#>  [55] "ref_facies_ambiance.csv"                 "ref_flag_wama.csv"                       "ref_formation_geologique.csv"           
#>  [58] "ref_fraction_analysee.csv"               "ref_granulometrie.csv"                   "ref_importance.csv"                     
#>  [61] "ref_intervenant.csv"                     "ref_isolement.csv"                       "ref_localisation_ambiance.csv"          
#>  [64] "ref_logique_3.csv"                       "ref_logique_4.csv"                       "ref_methode_estimation_poids.csv"       
#>  [67] "ref_modele_materiel.csv"                 "ref_moyen_prospection.csv"               "ref_niveau_qualification.csv"           
#>  [70] "ref_objectif.csv"                        "ref_occupation_sol.csv"                  "ref_ombrage_ambiance.csv"               
#>  [73] "ref_ombrage_riviere.csv"                 "ref_ombrage.csv"                         "ref_pathologie.csv"                     
#>  [76] "ref_profil.csv"                          "ref_protocole.csv"                       "ref_regime_hydrologique.csv"            
#>  [79] "ref_repeuplement.csv"                    "ref_sexe.csv"                            "ref_sinuosite.csv"                      
#>  [82] "ref_situation.csv"                       "ref_support_sandre.csv"                  "ref_tendance_debit.csv"                 
#>  [85] "ref_turbidite.csv"                       "ref_type_abondance_anguilles.csv"        "ref_type_abondance_saumon.csv"          
#>  [88] "ref_type_abondance.csv"                  "ref_type_colmatage.csv"                  "ref_type_facies.csv"                    
#>  [91] "ref_type_groupe_points.csv"              "ref_type_illustration.csv"               "ref_type_longueur.csv"                  
#>  [94] "ref_type_lot.csv"                        "ref_type_prelevement_elementaire.csv"    "ref_type_projection.csv"                
#>  [97] "ref_unite_hydrographique.csv"            "ref_vegetation_dominante.csv"            "ref_vegetation.csv"                     
#> [100] "ref_zone_huet.csv"                       "referentiel_communes.csv"                "region.csv"                             
#> [103] "station.csv"
```

``` r
station_file_path <- get_filepath(pkg_info, file_names[103])
station <- read.csv2(station_file_path, row.names = 1)
head(station)
#>   sta_id sta_code_sandre               sta_libelle_sandre sta_enh_id sta_eligibilite_ipr_iprplus sta_com_code_insee sta_point_km_aval
#> 1   1216        02057002 LA MOSELLE À MÉRÉVILLE (CAPTAGE)      21936                           t              54364                 0
#> 2   1819        02096903    LA SARRE À SARRALBE (CAPTAGE)      22572                           t              57628                 0
#> 3  47594        05555555     La Torgue au niveau de Varès         NA                           t              47316                NA
#> 4  12559        04306011     RANCE A SAINT-ANDRE-DES-EAUX         NA                           t              22274                 0
#> 5  12830        04322006       GUIC A BELLE-ISLE-EN-TERRE         NA                           t              22005                 0
#> 6  12987        04327010                PENZE A GUIMILIAU         NA                           t              29074                 0
#>           sta_localisation_precise sta_code_national_masse_eau                                      sta_geometrie sta_typ_id sta_coordonnees_x
#> 1 LA MOSELLE À MÉRÉVILLE (CAPTAGE)                       CR211 0101000020E6100000D9A32083AE8F18407A62DF899F4D4840          2          931461.8
#> 2    LA SARRE À SARRALBE (CAPTAGE)                       CR413 0101000020E6100000AF78B5C434211C402740244FCC7D4840          2          995029.4
#> 3                  Pont de la D120                        <NA> 0101000020E61000008A2B2C381341D53FF186479D4B354640          2          487621.0
#> 4                    PONT DU BESSO                        <NA> 0101000020E610000044F6B06D850C00C0DC5D8F9A532F4840          2          329499.5
#> 5                        PONT COAT                        <NA> 0101000020E61000009535398320350BC0449F0CB384424840          2          227820.0
#> 6                           VIADUC                        <NA> 0101000020E6100000BFBB0772FBD60FC0142C305F0B3E4840          2          184870.1
#>   sta_coordonnees_y sta_date_derniere_modification sta_uti_id sta_statut
#> 1           6838629            2018-01-01 00:00:00         NA          t
#> 2           6883408            2018-01-01 00:00:00         NA          t
#> 3           6372180            2019-02-20 00:00:00         NA          t
#> 4           6819464            2018-01-01 00:00:00         NA          t
#> 5           6843530            2018-01-01 00:00:00         NA          t
#> 6           6843278            2018-01-01 00:00:00         NA          t
```
