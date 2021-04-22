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



#---- Configuration ----

model_file     <- "./results/paper_defor_rf/ml_model.rds"
validation_shp <- "./data/validation/validation.shp"
my_cube       <- "amazon_S2_10_16D_STK"

out_dir    <- dirname(model_file)
label_file <- file.path(out_dir, "labels.txt")

stopifnot(file.exists(validation_shp))
stopifnot(dir.exists(out_dir))
stopifnot(file.exists(model_file))
stopifnot(file.exists(label_file))

source("./scripts/00_util.R")



#---- Validation using Olofson's method ----

# Read the labels used during the classification.
class_labels <- label_file %>%
    readLines() %>%
    magrittr::set_names(1:length(.))

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
# class             n
# 1 Deforestation    86
# 2 Forest          136
# 3 NatNonForest     52
# 4 Pasture         114
# [1] 388

# Read the classification rasters processed with different smoothers.
classification_tb <- out_dir %>%
    list.files(pattern = paste0("^", my_cube,
                                "_.+_class_(bayes|bilin)_v[0-9]+[.]tif$"),
               recursive = TRUE,
               full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = value) %>%
    dplyr::mutate(file_name = basename(file_path),
                  smooth = stringr::str_extract(file_path,
                                                pattern = "(bayes|bilin)")) %>%
    dplyr::select(-file_name)

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

# Print the accuracies. Choose a smoother.
classification_tb %>%
    dplyr::select(smooth, acc) %>%
    dplyr::arrange(smooth) %>%
    tidyr::unnest(acc) %>%
    knitr::kable(digits = 2)



#---- Compare our classification to PRODES -----

my_smoother <- "bayes"
prodes_dir <- "./data/prodes/yearly_deforestation_biome/077095"
stopifnot(dir.exists(prodes_dir))

smoother_tb <- classification_tb %>%
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

# Compute and print a confusion matrix.
caret::confusionMatrix(data =      factor(acc_2019$predicted,
                                          levels = my_levels),
                       reference = factor(acc_2019$reference,
                                          levels = my_levels))

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
                                 paste0("deforestation_prodes_classification_",
                                        my_smoother,
                                        ".shp")))
