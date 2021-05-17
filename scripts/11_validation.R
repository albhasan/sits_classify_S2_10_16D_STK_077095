# Validate the results of the classification
library(caret)
library(dplyr)
library(ensurer)
library(gdalUtils)
library(knitr)
library(purrr)
library(raster)
library(sf)



#---- Configuration ----


class_labels <- c(`1` = "Deforestation",
                  `2` = "Forest",
                  `3` = "NatNonForest",
                  `4` = "Pasture")
# class_labels <- c(`1` = "Abandoned", `2` = "Deforestation", `3` = "Forest",
#                   `4` = "NatNonForest", `5` = "Pasture")

classification_dir <- "./results/paper_defor"
stopifnot(dir.exists(classification_dir))


source("./scripts/00_util.R")



#---- Validation using Olofson's method ----

validation_shp <- "./data/validation/validation.shp"
stopifnot(file.exists(validation_shp))

# Read the validation points.
validation_sf <- validation_shp %>%
    sf::read_sf() %>%
    ensurer::ensure_that(all(class_labels %in% .$class),
                         err_des = "Unknown labels found!") %>%
    dplyr::filter(class %in% class_labels) %>%
    ensurer::ensure_that(length(unique(.$class)) >= length(class_labels),
                         err_desc = "Unsampled labels!")
# Print the number of samples.
validation_sf %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    (function(x) {
        x %>%
            dplyr::count(class) %>%
            print()
        print(nrow(x))
        invisible(x)
    })

# Read the classification rasters processed with different bayesian smoothers.
classification_tb <- classification_dir %>%
    list.files(pattern = "^paper_defor_(bayes|bilin)_class.+[.]tif$",
               recursive = TRUE,
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(smooth = file_path %>%
                      dirname() %>%
                      basename())


classification_tb <- classification_tb %>%
    dplyr::mutate(class_r = purrr::map(file_path, raster::raster),
                  validation = purrr::map(class_r, raster::extract,
                                          y = as(validation_sf, "Spatial"),
                                          cellnumbers = FALSE,
                                          sp = TRUE),
                  ref_pred = purrr::map(validation, get_ref_pred,
                                        class_labels = class_labels),
                  confmat_obj = purrr::map(ref_pred, get_conmat),
                  conmat = purrr::map(confmat_obj, get_mt),
                  acc  = purrr::map(conmat, get_accuracies))

# Print the accuracies.
classification_tb %>%
    dplyr::select(smooth, acc) %>%
    dplyr::arrange(smooth) %>%
    tidyr::unnest(acc) %>%
    knitr::kable(digits = 2)



#---- Compare our classification to PRODES -----

#my_smoother <- "5x5_bayes"
my_smoother <- "9x9_bilinear"

prodes_dir <- "./data/prodes/yearly_deforestation_biome/077095"
stopifnot(dir.exists(prodes_dir))

smoother_tb <- classification_tb %>%
    dplyr::select(file_path, smooth, class_r) %>%
    dplyr::mutate(class_crs = purrr::map(class_r, raster::crs)) %>%
    dplyr::filter(smooth == my_smoother) %>%
    ensurer::ensure_that(nrow(.) == 1,
                         err_desc = "Only one classification raster is expected!")

#' Helper function for writing an sf object to a temporal file
#'
#' @param x An sf objet.
#' @retun A character. Path to a temporal shp file.
sf_to_tmp <- function(x){
    tmp_file <- tempfile(pattern = "prodes_proj_",
                         fileext = ".shp")
    x %>%
        dplyr::mutate(ID = as.integer(ID)) %>%
        sf::write_sf(dsn = tmp_file)
    return(tmp_file)
}

prodes_tb <- prodes_dir %>%
    list.files(pattern = ".shp",
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(name = file_path %>%
                      tools::file_path_sans_ext() %>%
                      basename()) %>%
    dplyr::filter(name == "yearly_deforestation_biome") %>%
    dplyr::mutate(sf = purrr::map(file_path, sf::read_sf)) %>%
    # Ensure unique IDs.
    dplyr::mutate(n_features = purrr::map_int(sf, nrow),
                  unique_id  = purrr::map_int(sf, function(x){length(unique(x$ID))})) %>%
    ensurer::ensure_that(all(.$unique_id == .$n_features),
                         err_desc = "IDs aren't unique!") %>%
    dplyr::select(-n_features, -unique_id) %>%
    # Project to the classification's crs
    dplyr::mutate(sf_proj = purrr::map(sf,
                                       sf::st_transform,
                                       crs = smoother_tb$class_crs[[1]])) %>%
    dplyr::mutate(sf_proj_file = purrr::map(sf_proj, sf_to_tmp))

class_extent <- raster::extent(smoother_tb$class_r[[1]])
class_resolution <- raster::res(smoother_tb$class_r[[1]])


# rasterization!!!
prodes_tb <- prodes_tb %>%
    dplyr::mutate(sf_rasterized = purrr::map(name, function(x){
        return(tempfile(pattern = "prodes_rasterized_",
                        fileext = ".tif"))
    })) %>%
    dplyr::mutate(class_masked = purrr::map2(sf_proj_file,
                                             sf_rasterized,
                                             gdalUtils::gdal_rasterize,
                                             a = "ID",
                                             of = "GTiff",
                                             co = c("COMPRESS=LZW",
                                                    "BIGTIFF=YES"),
                                             a_nodata = -9999,
                                             init     = -9999,
                                             te = c(class_extent@xmin,
                                                    class_extent@ymin,
                                                    class_extent@xmax,
                                                    class_extent@ymax),
                                             tr = class_resolution,
                                             ot = "Int32",
                                             output_Raster = TRUE))

# TODO: Convert to a map in a mutate of prodes_tb.
masked_tb <- tibble::tibble(prodes_id = as.vector(prodes_tb$class_masked[[1]][]),
                            classification = smoother_tb$class_r[[1]][]) %>%
    magrittr::set_names(c("prodes_id", "classification")) %>%
    dplyr::arrange(prodes_id)

# Helper for computing the mode.
# NOTE: R doesn't already have a mode?
getMode <- function(x) {
    keys <- unique(x)
    keys[which.max(tabulate(match(x, keys)))]
}


# Most common classification class in each Prodes polygon.
class_by_prodes <- masked_tb %>%
    dplyr::group_by(prodes_id) %>%
    dplyr::summarise(mode = getMode(classification)) %>%
    dplyr::mutate(class_mode = dplyr::recode(mode, !!!class_labels))

prodes_class_sf <- prodes_tb$sf[[1]] %>%
    dplyr::left_join(class_by_prodes, by = c("ID" = "prodes_id"))

saveRDS(prodes_class_sf,
        file = paste0("class_by_prodes_", my_smoother, ".rds"))

acc_2019 <- prodes_class_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::filter(CLASS_NAME == "d2019") %>%
    dplyr::mutate(reference = dplyr::recode(MAIN_CLASS,
                                            "DESMATAMENTO" = "deforestation",
                                            .default = NA_character_),
                  predicted = tolower(class_mode)) %>%
    dplyr::select(reference, predicted) %>%
    tidyr::drop_na()

my_levels <- acc_2019 %>%
    as.matrix() %>%
    as.vector() %>%
    unique() %>%
    sort()

caret::confusionMatrix(data =      factor(acc_2019$predicted,
                                          levels = my_levels),
                       reference = factor(acc_2019$reference,
                                          levels = my_levels))

prodes_class_sf %>%
    dplyr::filter(CLASS_NAME == "d2019") %>%
    dplyr::mutate(reference = dplyr::recode(MAIN_CLASS,
                                            "DESMATAMENTO" = "deforestation",
                                            .default = NA_character_),
                  predicted = tolower(class_mode)) %>%
    dplyr::mutate(match = reference == predicted) %>%
    dplyr::select(reference, predicted, match) %>%
    sf::write_sf(paste0("deforestation_prodes_classification", my_smoother,
                        ".shp"))


#---- 5x5 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           552      0            0       0
# forest                    4      0            0       0
# natnonforest              2      0            0       0
# pasture                 139      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.792
# 95% CI : (0.7599, 0.8215)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                         0.792            NA                  NA             NA
# Specificity                            NA      0.994261            0.997131         0.8006
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                          1.000      0.000000            0.000000         0.0000
# Detection Rate                      0.792      0.000000            0.000000         0.0000
# Detection Prevalence                0.792      0.005739            0.002869         0.1994
# Balanced Accuracy                      NA            NA                  NA

#---- 9x9 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           545      0            0       0
# forest                    8      0            0       0
# natnonforest              4      0            0       0
# pasture                 140      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.7819
# 95% CI : (0.7494, 0.812)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                        0.7819            NA                  NA             NA
# Specificity                            NA       0.98852            0.994261         0.7991
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                         1.0000       0.00000            0.000000         0.0000
# Detection Rate                     0.7819       0.00000            0.000000         0.0000
# Detection Prevalence               0.7819       0.01148            0.005739         0.2009
# Balanced Accuracy                      NA            NA                  NA
#
#---- 15x15 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           540      0            0       0
# forest                   14      0            0       0
# natnonforest              4      0            0       0
# pasture                 139      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.7747
# 95% CI : (0.7419, 0.8053)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest
# Sensitivity                        0.7747            NA                  NA
# Specificity                            NA       0.97991            0.994261
# Pos Pred Value                         NA            NA                  NA
# Neg Pred Value                         NA            NA                  NA
# Prevalence                         1.0000       0.00000            0.000000
# Detection Rate                     0.7747       0.00000            0.000000
# Detection Prevalence               0.7747       0.02009            0.005739
# Balanced Accuracy                      NA            NA                  NA
#                      Class: pasture
# Sensitivity                      NA
# Specificity                  0.8006
# Pos Pred Value                   NA
# Neg Pred Value                   NA
# Prevalence                   0.0000
# Detection Rate               0.0000
# Detection Prevalence         0.1994
# Balanced Accuracy                NA
#
#---- 19x19 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           524      0            0       0
# forest                   21      0            0       0
# natnonforest              4      0            0       0
# pasture                 148      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.7518
# 95% CI : (0.718, 0.7835)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest
# Sensitivity                        0.7518            NA                  NA
# Specificity                            NA       0.96987            0.994261
# Pos Pred Value                         NA            NA                  NA
# Neg Pred Value                         NA            NA                  NA
# Prevalence                         1.0000       0.00000            0.000000
# Detection Rate                     0.7518       0.00000            0.000000
# Detection Prevalence               0.7518       0.03013            0.005739
# Balanced Accuracy                      NA            NA                  NA
#                      Class: pasture
# Sensitivity                      NA
# Specificity                  0.7877
# Pos Pred Value                   NA
# Neg Pred Value                   NA
# Prevalence                   0.0000
# Detection Rate               0.0000
# Detection Prevalence         0.2123
# Balanced Accuracy                NA
#
#---- 25x25 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           497      0            0       0
# forest                   43      0            0       0
# natnonforest              4      0            0       0
# pasture                 153      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.7131
# 95% CI : (0.6779, 0.7464)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest
# Sensitivity                        0.7131            NA                  NA
# Specificity                            NA       0.93831            0.994261
# Pos Pred Value                         NA            NA                  NA
# Neg Pred Value                         NA            NA                  NA
# Prevalence                         1.0000       0.00000            0.000000
# Detection Rate                     0.7131       0.00000            0.000000
# Detection Prevalence               0.7131       0.06169            0.005739
# Balanced Accuracy                      NA            NA                  NA
#                      Class: pasture
# Sensitivity                      NA
# Specificity                  0.7805
# Pos Pred Value                   NA
# Neg Pred Value                   NA
# Prevalence                   0.0000
# Detection Rate               0.0000
# Detection Prevalence         0.2195
# Balanced Accuracy                NA
#
#---- 5x5 bilinear ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           551      0            0       0
# forest                    4      0            0       0
# natnonforest              3      0            0       0
# pasture                 139      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.7905
# 95% CI : (0.7584, 0.8202)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                        0.7905            NA                  NA             NA
# Specificity                            NA      0.994261            0.995696         0.8006
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                         1.0000      0.000000            0.000000         0.0000
# Detection Rate                     0.7905      0.000000            0.000000         0.0000
# Detection Prevalence               0.7905      0.005739            0.004304         0.1994
# Balanced Accuracy                      NA            NA                  NA             NA
#
#---- 9x9 bilinear ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           551      0            0       0
# forest                    4      0            0       0
# natnonforest              3      0            0       0
# pasture                 139      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.7905
# 95% CI : (0.7584, 0.8202)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                        0.7905            NA                  NA             NA
# Specificity                            NA      0.994261            0.995696         0.8006
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                         1.0000      0.000000            0.000000         0.0000
# Detection Rate                     0.7905      0.000000            0.000000         0.0000
# Detection Prevalence               0.7905      0.005739            0.004304         0.1994
# Balanced Accuracy                      NA            NA                  NA             NA


acc_2018 <- prodes_class_sf %>%
    sf::st_set_geometry(NULL) %>%
    dplyr::filter(CLASS_NAME == "d2018") %>%
    dplyr::mutate(reference = dplyr::recode(MAIN_CLASS,
                                            "DESMATAMENTO" = "deforestation",
                                            .default = NA_character_),
                  predicted = tolower(class_mode)) %>%
    dplyr::select(reference, predicted) %>%
    tidyr::drop_na()

caret::confusionMatrix(data =      factor(acc_2018$predicted,
                                          levels = my_levels),
                       reference = factor(acc_2018$reference,
                                          levels = my_levels))
#---- 5x5 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            18      0            0       0
# forest                   67      0            0       0
# natnonforest             13      0            0       0
# pasture                 731      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0217
# 95% CI : (0.0129, 0.0341)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                       0.02171            NA                  NA             NA
# Specificity                            NA       0.91918             0.98432         0.1182
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                        1.00000       0.00000             0.00000         0.0000
# Detection Rate                    0.02171       0.00000             0.00000         0.0000
# Detection Prevalence              0.02171       0.08082             0.01568         0.8818
# Balanced Accuracy                      NA            NA                  NA

#---- 9x9 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            19      0            0       0
# forest                   77      0            0       0
# natnonforest             17      0            0       0
# pasture                 716      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0229
# 95% CI : (0.0139, 0.0356)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                       0.02292            NA                  NA             NA
# Specificity                            NA       0.90712             0.97949         0.1363
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                        1.00000       0.00000             0.00000         0.0000
# Detection Rate                    0.02292       0.00000             0.00000         0.0000
# Detection Prevalence              0.02292       0.09288             0.02051         0.8637
# Balanced Accuracy                      NA            NA                  NA             NA
#
#---- 15x15 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            21      0            0       0
# forest                   92      0            0       0
# natnonforest             16      0            0       0
# pasture                 700      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0253
# 95% CI : (0.0157, 0.0385)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest
# Sensitivity                       0.02533            NA                  NA
# Specificity                            NA         0.889              0.9807
# Pos Pred Value                         NA            NA                  NA
# Neg Pred Value                         NA            NA                  NA
# Prevalence                        1.00000         0.000              0.0000
# Detection Rate                    0.02533         0.000              0.0000
# Detection Prevalence              0.02533         0.111              0.0193
# Balanced Accuracy                      NA            NA                  NA
#                      Class: pasture
# Sensitivity                      NA
# Specificity                  0.1556
# Pos Pred Value                   NA
# Neg Pred Value                   NA
# Prevalence                   0.0000
# Detection Rate               0.0000
# Detection Prevalence         0.8444
# Balanced Accuracy                NA
#
#---- 19x19 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            21      0            0       0
# forest                  106      0            0       0
# natnonforest             17      0            0       0
# pasture                 685      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0253
# 95% CI : (0.0157, 0.0385)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest
# Sensitivity                       0.02533            NA                  NA
# Specificity                            NA        0.8721             0.97949
# Pos Pred Value                         NA            NA                  NA
# Neg Pred Value                         NA            NA                  NA
# Prevalence                        1.00000        0.0000             0.00000
# Detection Rate                    0.02533        0.0000             0.00000
# Detection Prevalence              0.02533        0.1279             0.02051
# Balanced Accuracy                      NA            NA                  NA
#                      Class: pasture
# Sensitivity                      NA
# Specificity                  0.1737
# Pos Pred Value                   NA
# Neg Pred Value                   NA
# Prevalence                   0.0000
# Detection Rate               0.0000
# Detection Prevalence         0.8263
# Balanced Accuracy                NA
#
#---- 25x25 bayes ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            18      0            0       0
# forest                  114      0            0       0
# natnonforest             15      0            0       0
# pasture                 682      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0217
# 95% CI : (0.0129, 0.0341)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest
# Sensitivity                       0.02171            NA                  NA
# Specificity                            NA        0.8625             0.98191
# Pos Pred Value                         NA            NA                  NA
# Neg Pred Value                         NA            NA                  NA
# Prevalence                        1.00000        0.0000             0.00000
# Detection Rate                    0.02171        0.0000             0.00000
# Detection Prevalence              0.02171        0.1375             0.01809
# Balanced Accuracy                      NA            NA                  NA
#                      Class: pasture
# Sensitivity                      NA
# Specificity                  0.1773
# Pos Pred Value                   NA
# Neg Pred Value                   NA
# Prevalence                   0.0000
# Detection Rate               0.0000
# Detection Prevalence         0.8227
# Balanced Accuracy                NA
#
#---- 5x5 bilinear ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            18      0            0       0
# forest                   62      0            0       0
# natnonforest             13      0            0       0
# pasture                 736      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0217
# 95% CI : (0.0129, 0.0341)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                       0.02171            NA                  NA             NA
# Specificity                            NA       0.92521             0.98432         0.1122
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                        1.00000       0.00000             0.00000         0.0000
# Detection Rate                    0.02171       0.00000             0.00000         0.0000
# Detection Prevalence              0.02171       0.07479             0.01568         0.8878
# Balanced Accuracy                      NA            NA                  NA             NA
#
#---- 9x9 bilinear ----
# Confusion Matrix and Statistics
#
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation            18      0            0       0
# forest                   62      0            0       0
# natnonforest             12      0            0       0
# pasture                 737      0            0       0
#
# Overall Statistics
#
# Accuracy : 0.0217
# 95% CI : (0.0129, 0.0341)
# No Information Rate : 1
# P-Value [Acc > NIR] : 1
#
# Kappa : 0
#
# Mcnemar's Test P-Value : NA
#
# Statistics by Class:
#
#                      Class: deforestation Class: forest Class: natnonforest Class: pasture
# Sensitivity                       0.02171            NA                  NA             NA
# Specificity                            NA       0.92521             0.98552          0.111
# Pos Pred Value                         NA            NA                  NA             NA
# Neg Pred Value                         NA            NA                  NA             NA
# Prevalence                        1.00000       0.00000             0.00000          0.000
# Detection Rate                    0.02171       0.00000             0.00000          0.000
# Detection Prevalence              0.02171       0.07479             0.01448          0.889
# Balanced Accuracy                      NA            NA                  NA             NA