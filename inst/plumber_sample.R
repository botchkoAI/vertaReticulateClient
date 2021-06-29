# Title     : TODO
# Objective : TODO
# Created by: sidi
# Created on: 28.06.21

#
# This is a Plumber API. You can run the API by clicking
# the 'Run API' button above.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#


# Title     : TODO
# Objective : TODO
# Created by: sidi
# Created on: 23.06.21

# renv::install(c("assertthat","jsonlite","stringr"))

#' @title Save Model Object
#' @description write model to a file of certain format
#' @param file_to_save_at target file path
#' @param modFile the fit model object
#' @param formula_obj formula object
#' @param original_df sample of the original dataset
#' @param required_packages character vector of required packages to call the predict function
#'
#' @return nothing
#' @export
#'
#' @examples
save_model_data <- function(file_to_save_at,
                            modFile,
                            formula_obj,
                            original_df,
                            required_packages=NULL
) {

  lenN <- min(ceiling(
    nrow(original_df)/10
  ),3,nrow(original_df))
  if(is.null(required_packages)) {
    required_packages = c("")
  }
  dat <- list(
    model_file = modFile,
    formula_obj = formula_obj,
    original_df=original_df[1:lenN],
    required_packages = required_packages
  )
  saveRDS(object = dat,file=file_to_save_at)
}


#' Title
#'
#' @param file
#'
#' @return
#' @export
#'
#' @examples
load_model_from_metadata <- function(file){
  dat  <- readRDS(file = file)
  for (nm in c("model_file","formula_obj","original_df","required_packages")) {
    assertthat::assert_that(nm%in% names(dat))
  }

  return(dat)
}

#' Predict Data From Saved Model
#'
#' @param file_to_save_at
#' @param modFile
#' @param formula_obj
#' @param original_df
#' @param required_packages
#'
#' @return
#' @export
#'
#' @examples
predict_new_model <- function(file,data){
  modeldat <- load_model_from_metadata(file)
  modelObj <- modeldat[["model_file"]]
  formula_obj <- modeldat[["formula_obj"]]
  original_df <- modeldat[["original_df"]]
  required_packages <- modeldat[['required_packages']]

  #region install packages
  lib <- .libPaths()[1]

  i1 <- !(required_packages %in% row.names(installed.packages()))
  if(any(i1)) {
    install.packages(required_packages[i1], dependencies = TRUE, lib = lib)
  }

  #endregion install packages

  predicted_values <- predict(modelObj,newdata = data)

  predicted_values

}
library(plumber)
library(jsonlite)
# source("model_io.R")



deployment_object <- load_model_from_metadata(
  "model_file.RData"
  )

# mod_metadata$model_file


# library(logger)

#* @apiTitle Plumber Example API

# deployment_object <- readRDS("model_to_deploy.rds")
model_object <- deployment_object[["model_file"]]
required_libraries <- deployment_object[["required_packages"]]

for(lib in required_libraries){
  if(length(lib)>0){

  if(!require(lib,character.only=T)){
    tryCatch({
    install.packages(lib)
    },error =function(e){
      print(e)
    })
    }
    }
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}



# rq <- list()
#* @post /score
function(req)
{
  dat <- req[["postBody"]]
  df <- as.data.frame(fromJSON(dat))
  predict(model_object, newdata=df)
}

