# -*- coding: UTF-8 -*-
#' Helper function to load packages from .R script package exports. Mostly used in the EDSAN jupyter env

#' @param path Default to where i save exported packages files.
#' @param package_name Package to load.


source_package <- function(path = "/appli/edsan_datalake/edsan_data/utils/", package_name = "") {
  path <- paste0("/appli/edsan_datalake/edsan_data/utils/", package_name, ".R")
  source(path)
}
