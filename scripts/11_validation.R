# Validate the results of the classification
library(caret)
library(dplyr)
library(ensurer)
library(gdalUtils)
library(knitr)
library(purrr)
library(raster)
library(sf)
library(stringr)



#---- Configuration -----

my_cube       <- "amazon_S2_10_16D_STK"
my_model_files <- c("./results/paper_defor_rf/ml_model.rds",
                    "./results/paper_defor_resnet/ml_model.rds",
                    "./results/paper_defor_tempcnn/ml_model.rds")
validation_shp <- "./data/validation/validation.shp"
stopifnot(file.exists(validation_shp))
stopifnot(all(file.exists(my_model_files)))

source("./scripts/00_util.R")



#---- Helper functions ----

# Helper function. Read the classification rasters processed with different smoothers.
list_classifications <- function(out_dir){
    out_dir %>%
        list.files(pattern = paste0("^", my_cube,
                                    "_.+_class_(bayes|bilin)_v[0-9]+[.]tif$"),
                   recursive = TRUE,
                   full.names = TRUE) %>%
        tibble::as_tibble() %>%
        dplyr::rename(file_path = value) %>%
        dplyr::mutate(file_name = basename(file_path),
                      smooth = stringr::str_extract(file_path,
                                                    pattern = "(bayes|bilin)")) %>%
        dplyr::select(-file_name) %>%
        return()
}

# Helper fucntion. Prepare classification labels for recoding.
prepare_labels <- function(label_file) {
    label_file %>%
        readLines() %>%
        magrittr::set_names(1:length(.)) %>%
        return()
}

# Validate sits's raster classifications using sample points.
do_validation <- function(out_dir, label_file, validation_shp, my_cube) {
    # Read the labels used during the classification.
    class_labels <- prepare_labels(label_file)
    # Read the validation points.
    validation_sf <- validation_shp %>%
        sf::read_sf() %>%
        ensurer::ensure_that(all(class_labels %in% .$class),
                             err_des = "Unknown labels found!") %>%
        dplyr::filter(class %in% class_labels) %>%
        ensurer::ensure_that(length(unique(.$class)) >= length(class_labels),
                             err_desc = "Unsampled labels!")

    # Validate each classification raster.
    classification_tb <- out_dir %>%
        list_classifications() %>%
        dplyr::mutate(class_r = purrr::map(file_path, raster::raster)) %>%
        dplyr::mutate(validation = purrr::map(class_r, raster::extract,
                                              y = as(validation_sf, "Spatial"),
                                              cellnumbers = FALSE,
                                              sp = TRUE)) %>%
        dplyr::mutate(ref_pred = purrr::map(validation, get_ref_pred,
                                            class_labels = class_labels)) %>%
        dplyr::mutate(confmat_obj = purrr::map(ref_pred, get_conmat)) %>%
        dplyr::mutate(conmat = purrr::map(confmat_obj, get_mt)) %>%
        dplyr::mutate(acc  = purrr::map(conmat, get_accuracies))
    # Return the accuracy.
    classification_tb %>%
        dplyr::select(smooth, acc, conmat) %>%
        dplyr::arrange(smooth) %>%
        tidyr::unnest(acc) %>%
        return()
}

# Helper. Compare sits's raster classifications to PRODES.
do_validation_prodes <- function(out_dir, label_file, my_cube) {
    my_smoother <- "bayes"
    prodes_dir <- "./data/prodes/yearly_deforestation_biome/077095"
    stopifnot(dir.exists(prodes_dir))

    class_labels <- prepare_labels(label_file)
    smoother_tb <- out_dir %>%
        list_classifications() %>%
        dplyr::mutate(class_r = purrr::map(file_path, raster::raster)) %>%
        dplyr::select(file_path, smooth, class_r) %>%
        dplyr::mutate(class_crs = purrr::map(class_r, raster::crs)) %>%
        dplyr::filter(smooth == my_smoother) %>%
        ensurer::ensure_that(nrow(.) == 1,
                             err_desc = "Only one classification raster is expected!")
    # Read the data from PRODES.
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
                      unique_id  = purrr::map_int(sf,
                                                  function(x){
                                                      length(unique(x$ID))
                                                  })) %>%
        ensurer::ensure_that(all(.$unique_id == .$n_features),
                             err_desc = "IDs aren't unique!") %>%
        dplyr::select(-n_features, -unique_id) %>%
        # Project to the classification's crs
        dplyr::mutate(sf_proj = purrr::map(sf,
                                           sf::st_transform,
                                           crs = smoother_tb$class_crs[[1]])) %>%
        dplyr::mutate(sf_proj_file = purrr::map(sf_proj, sf_to_tmp))
    class_extent     <- raster::extent(smoother_tb$class_r[[1]])
    class_resolution <- raster::res(smoother_tb$class_r[[1]])
    # Rasterize Prodes.
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
    # Create a tibble of using the values of each pixel (classification and Prodes).
    masked_tb <- tibble::tibble(prodes_id = as.vector(prodes_tb$class_masked[[1]][]),
                                classification = smoother_tb$class_r[[1]][]) %>%
        magrittr::set_names(c("prodes_id", "classification")) %>%
        dplyr::arrange(prodes_id)
    # Compute the most common classification class in each Prodes polygon.
    class_by_prodes <- masked_tb %>%
        dplyr::group_by(prodes_id) %>%
        dplyr::summarise(mode = getMode(classification)) %>%
        dplyr::mutate(class_mode = dplyr::recode(mode, !!!class_labels))
    # Join the results in a sf object.
    prodes_class_sf <- prodes_tb$sf[[1]] %>%
        dplyr::left_join(class_by_prodes, by = c("ID" = "prodes_id"))
    # Save Prodes validation results.
    saveRDS(prodes_class_sf,
            file = file.path(out_dir,
                             paste0("class_by_prodes_", my_smoother, ".rds")))
    # Create a tibble of reference and predicted labels.
    acc_2019 <- prodes_class_sf %>%
        sf::st_set_geometry(NULL) %>%
        dplyr::filter(CLASS_NAME == "d2019") %>%
        dplyr::mutate(reference = dplyr::recode(MAIN_CLASS,
                                                "DESMATAMENTO" = "deforestation",
                                                .default = NA_character_),
                      predicted = tolower(class_mode)) %>%
        dplyr::select(reference, predicted) %>%
        tidyr::drop_na()
    # Get the labels needed for creating R factors.
    my_levels <- acc_2019 %>%
        as.matrix() %>%
        as.vector() %>%
        unique() %>%
        sort()
    # Save results as a shapefile.
    prodes_class_sf %>%
        dplyr::filter(CLASS_NAME == "d2019") %>%
        dplyr::mutate(reference = dplyr::recode(MAIN_CLASS,
                                                "DESMATAMENTO" = "deforestation",
                                                .default = NA_character_),
                      predicted = tolower(class_mode)) %>%
        dplyr::mutate(match = reference == predicted) %>%
        dplyr::select(reference, predicted, match) %>%
        sf::write_sf(dsn = file.path(out_dir,
                                     paste0("prodes_vs_classification_",
                                            my_smoother,
                                            ".shp")))
    # Compute and print a confusion matrix.
    caret::confusionMatrix(data =      factor(acc_2019$predicted,
                                              levels = my_levels),
                           reference = factor(acc_2019$reference,
                                              levels = my_levels)) %>%
        return()
}

# Helper. Convert raster codes into labels.
add_labels_to_freq <- function(class_freq, labels) {
    class_freq %>%
        tibble::as_tibble() %>%
        dplyr::mutate(label = dplyr::recode(value, !!!labels)) %>%
        return()
}

# Helper. Get the area of a pixel.
get_pixel_area <- function(x) {
    x %>%
        terra::res() %>%
        prod() %>%
        return()
}

#---- Validation using sample points and Olofson ----


# Print areas in each map
label_area_tb <- my_model_files %>%
    tibble::as_tibble() %>%
    dplyr::rename(model_file = value) %>%
    # NOTE: Assume the results are hosted along with the model.
    dplyr::mutate(out_dir    = purrr::map_chr(model_file, dirname),
                  model_type = purrr::map_chr(model_file, get_model_type),
                  label_file = file.path(out_dir, "labels.txt"),
                  labels     = purrr::map(label_file, prepare_labels),
                  class_tb   = purrr::map(out_dir, list_classifications)) %>%
    tidyr::unnest(class_tb) %>%
    dplyr::mutate(class_r    = purrr::map(file_path, terra::rast),
                  class_freq = purrr::map(class_r, terra::freq),
                  labels     = purrr::map2(class_freq, labels,
                                           add_labels_to_freq),
                  sp_res     = purrr::map_dbl(class_r, get_pixel_area)) %>%
    dplyr::select(class_file = file_path, smooth, labels, sp_res, model_type) %>%
    tidyr::unnest(labels) %>%
    dplyr::select(class_file, smooth, model_type, label, sp_res, count) %>%
    dplyr::mutate(area_km2 = count * sp_res / 1e6) %>%
    dplyr::select(-sp_res, -count) %>%
    tidyr::pivot_wider(names_from = label,
                       values_from = area_km2)
label_area_tb %>%
    knitr::kable(digits = 2)

# Get the models and their resulting classification rasters.
model_tb <- my_model_files %>%
    tibble::as_tibble() %>%
    dplyr::rename(model_file = value) %>%
    # NOTE: Assume the results are hosted along with the model.
    dplyr::mutate(out_dir = purrr::map_chr(model_file, dirname),
                  model_type = purrr::map_chr(model_file, get_model_type),
                  label_file  = file.path(out_dir, "labels.txt")) %>%
    # Validation points were collected using Olofson's method.
    dplyr::mutate(validation = purrr::map2(out_dir, label_file,
                                           do_validation,
                                           validation_shp = validation_shp,
                                           my_cube = my_cube))

# Print the results.
model_tb %>%
    dplyr::select(model_type, validation) %>%
    tidyr::unnest(validation) %>%
    dplyr::select(-conmat) %>%
    knitr::kable(digits = 2)
# A tibble: 3 x 11
# model_type smooth overall prod_Deforestation prod_Forest prod_NatNonForest prod_Pasture user_Deforestati… user_Forest
# 1 rf         bayes    0.892              0.930       0.860             0.788        0.947             0.976       0.936
# 2 resnet     bayes    0.869              0.907       0.875             0.692        0.912             0.940       0.869
# 3 tempcnn    bayes    0.897              0.884       0.912             0.731        0.965             0.987       0.925


# Print the confusion matrices.
for (i in seq_along(model_tb$model_type)) {
    print("--------------------------------------------------------------------")
    print(model_tb$model_type[[i]])
    print(model_tb$validation[[i]]$conmat[[1]])
}
# [1] "--------------------------------------------------------------------"
# [1] "rf"
# Reference
# Prediction      Deforestation Forest NatNonForest Pasture
# Deforestation            80      2            0       0
# Forest                    1    117            7       0
# NatNonForest              2     15           41       6
# Pasture                   3      2            4     108
# [1] "--------------------------------------------------------------------"
# [1] "resnet"
# Reference
# Prediction      Deforestation Forest NatNonForest Pasture
# Deforestation            78      2            1       2
# Forest                    5    119           12       1
# NatNonForest              1     10           36       7
# Pasture                   2      5            3     104
# [1] "--------------------------------------------------------------------"
# [1] "tempcnn"
# Reference
# Prediction      Deforestation Forest NatNonForest Pasture
# Deforestation            76      0            0       1
# Forest                    2    124            8       0
# NatNonForest              3      5           38       3
# Pasture                   5      7            6     110

# Print the number of samples.
validation_shp %>%
    sf::read_sf() %>%
    sf::st_set_geometry(NULL) %>%
    tibble::as_tibble() %>%
    (function(x) {
        x %>%
            dplyr::count(class) %>%
            print()
        print(nrow(x))
        invisible(x)
    })
# A tibble: 5 x 2
# class             n
# 1 Abandoned        40
# 2 Deforestation    86
# 3 Forest          136
# 4 NatNonForest     52
# 5 Pasture         114
# [1] 428



#---- Compare the classification to PRODES -----

model_tb <- model_tb %>%
    dplyr::mutate(validation_prodes = purrr::map2(out_dir, label_file,
                                                  do_validation_prodes,
                                                  my_cube = my_cube))

# Print the confusion matrices.
for (i in seq_along(model_tb$model_type)) {
    print("--------------------------------------------------------------------")
    print(model_tb$model_type[[i]])
    print(model_tb$validation_prodes[[i]]$table)
}
# 1] "--------------------------------------------------------------------"
# [1] "rf"
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           548      0            0       0
# forest                    5      0            0       0
# natnonforest              6      0            0       0
# pasture                 138      0            0       0
# [1] "--------------------------------------------------------------------"
# [1] "resnet"
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           568      0            0       0
# forest                    8      0            0       0
# natnonforest              8      0            0       0
# pasture                 113      0            0       0
# [1] "--------------------------------------------------------------------"
# [1] "tempcnn"
# Reference
# Prediction      deforestation forest natnonforest pasture
# deforestation           536      0            0       0
# forest                    8      0            0       0
# natnonforest              1      0            0       0
# pasture                 152      0            0       0
