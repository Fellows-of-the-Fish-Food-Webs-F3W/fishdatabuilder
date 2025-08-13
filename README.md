
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
#>   [1] "ambiance.csv"                           
#>   [2] "autres_especes.csv"                     
#>   [3] "bassin_simp.csv"                        
#>   [4] "classe_ipr_plus.csv"                    
#>   [5] "classe_ipr.csv"                         
#>   [6] "code_wama_commune.csv"                  
#>   [7] "code_wama_riviere.csv"                  
#>   [8] "commune.csv"                            
#>   [9] "departement.csv"                        
#>  [10] "dispositif_collecte.csv"                
#>  [11] "dispositif_point.csv"                   
#>  [12] "dispositif_station.csv"                 
#>  [13] "facies.csv"                             
#>  [14] "groupe_points.csv"                      
#>  [15] "habitat_anguilles.csv"                  
#>  [16] "habitat.csv"                            
#>  [17] "historique_environnemental_pp.csv"      
#>  [18] "intervenant_departement.csv"            
#>  [19] "libelles.csv"                           
#>  [20] "localisation_point_wama.csv"            
#>  [21] "lot_poissons.csv"                       
#>  [22] "maj_objectif.csv"                       
#>  [23] "map_pointprelmetier.csv"                
#>  [24] "materiels.csv"                          
#>  [25] "mesure_individuelle.csv"                
#>  [26] "operation_description_peche.csv"        
#>  [27] "operation_donnees_environnementales.csv"
#>  [28] "operation_ipr_plus.csv"                 
#>  [29] "operation_ipr.csv"                      
#>  [30] "operation_objectif.csv"                 
#>  [31] "operation_suivi.csv"                    
#>  [32] "operation.csv"                          
#>  [33] "parametres_globaux.csv"                 
#>  [34] "parametres.csv"                         
#>  [35] "passage.csv"                            
#>  [36] "pathologie_lot.csv"                     
#>  [37] "pathologie_poisson.csv"                 
#>  [38] "point_prelevement.csv"                  
#>  [39] "poste_intervenant.csv"                  
#>  [40] "prelevement_elementaire.csv"            
#>  [41] "probabilite_presence_ipr_plus.csv"      
#>  [42] "probabilite_presence_ipr.csv"           
#>  [43] "profil_droit.csv"                       
#>  [44] "ref_accessibilite.csv"                  
#>  [45] "ref_bassin.csv"                         
#>  [46] "ref_categorie_piscicole.csv"            
#>  [47] "ref_classe_vitesse_courant.csv"         
#>  [48] "ref_colmatage.csv"                      
#>  [49] "ref_condition_hydrologique.csv"         
#>  [50] "ref_droit.csv"                          
#>  [51] "ref_entite_hydrographique.csv"          
#>  [52] "ref_espece.csv"                         
#>  [53] "ref_etat_avancement.csv"                
#>  [54] "ref_fabricant_materiel.csv"             
#>  [55] "ref_facies_ambiance.csv"                
#>  [56] "ref_flag_wama.csv"                      
#>  [57] "ref_formation_geologique.csv"           
#>  [58] "ref_fraction_analysee.csv"              
#>  [59] "ref_granulometrie.csv"                  
#>  [60] "ref_importance.csv"                     
#>  [61] "ref_intervenant.csv"                    
#>  [62] "ref_isolement.csv"                      
#>  [63] "ref_localisation_ambiance.csv"          
#>  [64] "ref_logique_3.csv"                      
#>  [65] "ref_logique_4.csv"                      
#>  [66] "ref_methode_estimation_poids.csv"       
#>  [67] "ref_modele_materiel.csv"                
#>  [68] "ref_moyen_prospection.csv"              
#>  [69] "ref_niveau_qualification.csv"           
#>  [70] "ref_objectif.csv"                       
#>  [71] "ref_occupation_sol.csv"                 
#>  [72] "ref_ombrage_ambiance.csv"               
#>  [73] "ref_ombrage_riviere.csv"                
#>  [74] "ref_ombrage.csv"                        
#>  [75] "ref_pathologie.csv"                     
#>  [76] "ref_profil.csv"                         
#>  [77] "ref_protocole.csv"                      
#>  [78] "ref_regime_hydrologique.csv"            
#>  [79] "ref_repeuplement.csv"                   
#>  [80] "ref_sexe.csv"                           
#>  [81] "ref_sinuosite.csv"                      
#>  [82] "ref_situation.csv"                      
#>  [83] "ref_support_sandre.csv"                 
#>  [84] "ref_tendance_debit.csv"                 
#>  [85] "ref_turbidite.csv"                      
#>  [86] "ref_type_abondance_anguilles.csv"       
#>  [87] "ref_type_abondance_saumon.csv"          
#>  [88] "ref_type_abondance.csv"                 
#>  [89] "ref_type_colmatage.csv"                 
#>  [90] "ref_type_facies.csv"                    
#>  [91] "ref_type_groupe_points.csv"             
#>  [92] "ref_type_illustration.csv"              
#>  [93] "ref_type_longueur.csv"                  
#>  [94] "ref_type_lot.csv"                       
#>  [95] "ref_type_prelevement_elementaire.csv"   
#>  [96] "ref_type_projection.csv"                
#>  [97] "ref_unite_hydrographique.csv"           
#>  [98] "ref_vegetation_dominante.csv"           
#>  [99] "ref_vegetation.csv"                     
#> [100] "ref_zone_huet.csv"                      
#> [101] "referentiel_communes.csv"               
#> [102] "region.csv"                             
#> [103] "station.csv"
```

``` r
station_file_path <- get_filepath(pkg_info, file_names[103])
station <- read.csv2(station_file_path, row.names = 1)
head(station)
#>   sta_id sta_code_sandre               sta_libelle_sandre sta_enh_id
#> 1   1216        02057002 LA MOSELLE À MÉRÉVILLE (CAPTAGE)      21936
#> 2   1819        02096903    LA SARRE À SARRALBE (CAPTAGE)      22572
#> 3  47594        05555555     La Torgue au niveau de Varès         NA
#> 4  12559        04306011     RANCE A SAINT-ANDRE-DES-EAUX         NA
#> 5  12830        04322006       GUIC A BELLE-ISLE-EN-TERRE         NA
#> 6  12987        04327010                PENZE A GUIMILIAU         NA
#>   sta_eligibilite_ipr_iprplus sta_com_code_insee sta_point_km_aval
#> 1                           t              54364                 0
#> 2                           t              57628                 0
#> 3                           t              47316                NA
#> 4                           t              22274                 0
#> 5                           t              22005                 0
#> 6                           t              29074                 0
#>           sta_localisation_precise sta_code_national_masse_eau
#> 1 LA MOSELLE À MÉRÉVILLE (CAPTAGE)                       CR211
#> 2    LA SARRE À SARRALBE (CAPTAGE)                       CR413
#> 3                  Pont de la D120                        <NA>
#> 4                    PONT DU BESSO                        <NA>
#> 5                        PONT COAT                        <NA>
#> 6                           VIADUC                        <NA>
#>                                        sta_geometrie sta_typ_id
#> 1 0101000020E6100000D9A32083AE8F18407A62DF899F4D4840          2
#> 2 0101000020E6100000AF78B5C434211C402740244FCC7D4840          2
#> 3 0101000020E61000008A2B2C381341D53FF186479D4B354640          2
#> 4 0101000020E610000044F6B06D850C00C0DC5D8F9A532F4840          2
#> 5 0101000020E61000009535398320350BC0449F0CB384424840          2
#> 6 0101000020E6100000BFBB0772FBD60FC0142C305F0B3E4840          2
#>   sta_coordonnees_x sta_coordonnees_y sta_date_derniere_modification sta_uti_id
#> 1          931461.8           6838629            2018-01-01 00:00:00         NA
#> 2          995029.4           6883408            2018-01-01 00:00:00         NA
#> 3          487621.0           6372180            2019-02-20 00:00:00         NA
#> 4          329499.5           6819464            2018-01-01 00:00:00         NA
#> 5          227820.0           6843530            2018-01-01 00:00:00         NA
#> 6          184870.1           6843278            2018-01-01 00:00:00         NA
#>   sta_statut
#> 1          t
#> 2          t
#> 3          t
#> 4          t
#> 5          t
#> 6          t
```

``` r
list_optional_data()
#>   [1] "ambiance.csv"                           
#>   [2] "autres_especes.csv"                     
#>   [3] "bassin_simp.csv"                        
#>   [4] "classe_ipr_plus.csv"                    
#>   [5] "classe_ipr.csv"                         
#>   [6] "code_wama_commune.csv"                  
#>   [7] "code_wama_riviere.csv"                  
#>   [8] "commune.csv"                            
#>   [9] "departement.csv"                        
#>  [10] "dispositif_collecte.csv"                
#>  [11] "dispositif_point.csv"                   
#>  [12] "dispositif_station.csv"                 
#>  [13] "facies.csv"                             
#>  [14] "groupe_points.csv"                      
#>  [15] "habitat_anguilles.csv"                  
#>  [16] "habitat.csv"                            
#>  [17] "historique_environnemental_pp.csv"      
#>  [18] "intervenant_departement.csv"            
#>  [19] "libelles.csv"                           
#>  [20] "localisation_point_wama.csv"            
#>  [21] "lot_poissons.csv"                       
#>  [22] "maj_objectif.csv"                       
#>  [23] "map_pointprelmetier.csv"                
#>  [24] "materiels.csv"                          
#>  [25] "mesure_individuelle.csv"                
#>  [26] "operation_description_peche.csv"        
#>  [27] "operation_donnees_environnementales.csv"
#>  [28] "operation_ipr_plus.csv"                 
#>  [29] "operation_ipr.csv"                      
#>  [30] "operation_objectif.csv"                 
#>  [31] "operation_suivi.csv"                    
#>  [32] "operation.csv"                          
#>  [33] "parametres_globaux.csv"                 
#>  [34] "parametres.csv"                         
#>  [35] "passage.csv"                            
#>  [36] "pathologie_lot.csv"                     
#>  [37] "pathologie_poisson.csv"                 
#>  [38] "point_prelevement.csv"                  
#>  [39] "poste_intervenant.csv"                  
#>  [40] "prelevement_elementaire.csv"            
#>  [41] "probabilite_presence_ipr_plus.csv"      
#>  [42] "probabilite_presence_ipr.csv"           
#>  [43] "profil_droit.csv"                       
#>  [44] "ref_accessibilite.csv"                  
#>  [45] "ref_bassin.csv"                         
#>  [46] "ref_categorie_piscicole.csv"            
#>  [47] "ref_classe_vitesse_courant.csv"         
#>  [48] "ref_colmatage.csv"                      
#>  [49] "ref_condition_hydrologique.csv"         
#>  [50] "ref_droit.csv"                          
#>  [51] "ref_entite_hydrographique.csv"          
#>  [52] "ref_espece.csv"                         
#>  [53] "ref_etat_avancement.csv"                
#>  [54] "ref_fabricant_materiel.csv"             
#>  [55] "ref_facies_ambiance.csv"                
#>  [56] "ref_flag_wama.csv"                      
#>  [57] "ref_formation_geologique.csv"           
#>  [58] "ref_fraction_analysee.csv"              
#>  [59] "ref_granulometrie.csv"                  
#>  [60] "ref_importance.csv"                     
#>  [61] "ref_intervenant.csv"                    
#>  [62] "ref_isolement.csv"                      
#>  [63] "ref_localisation_ambiance.csv"          
#>  [64] "ref_logique_3.csv"                      
#>  [65] "ref_logique_4.csv"                      
#>  [66] "ref_methode_estimation_poids.csv"       
#>  [67] "ref_modele_materiel.csv"                
#>  [68] "ref_moyen_prospection.csv"              
#>  [69] "ref_niveau_qualification.csv"           
#>  [70] "ref_objectif.csv"                       
#>  [71] "ref_occupation_sol.csv"                 
#>  [72] "ref_ombrage_ambiance.csv"               
#>  [73] "ref_ombrage_riviere.csv"                
#>  [74] "ref_ombrage.csv"                        
#>  [75] "ref_pathologie.csv"                     
#>  [76] "ref_profil.csv"                         
#>  [77] "ref_protocole.csv"                      
#>  [78] "ref_regime_hydrologique.csv"            
#>  [79] "ref_repeuplement.csv"                   
#>  [80] "ref_sexe.csv"                           
#>  [81] "ref_sinuosite.csv"                      
#>  [82] "ref_situation.csv"                      
#>  [83] "ref_support_sandre.csv"                 
#>  [84] "ref_tendance_debit.csv"                 
#>  [85] "ref_turbidite.csv"                      
#>  [86] "ref_type_abondance_anguilles.csv"       
#>  [87] "ref_type_abondance_saumon.csv"          
#>  [88] "ref_type_abondance.csv"                 
#>  [89] "ref_type_colmatage.csv"                 
#>  [90] "ref_type_facies.csv"                    
#>  [91] "ref_type_groupe_points.csv"             
#>  [92] "ref_type_illustration.csv"              
#>  [93] "ref_type_longueur.csv"                  
#>  [94] "ref_type_lot.csv"                       
#>  [95] "ref_type_prelevement_elementaire.csv"   
#>  [96] "ref_type_projection.csv"                
#>  [97] "ref_unite_hydrographique.csv"           
#>  [98] "ref_vegetation_dominante.csv"           
#>  [99] "ref_vegetation.csv"                     
#> [100] "ref_zone_huet.csv"                      
#> [101] "referentiel_communes.csv"               
#> [102] "region.csv"                             
#> [103] "station.csv"
```
