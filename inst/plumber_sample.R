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
    model_fit = modFile,
    formula_obj = formula_obj,
    original_df=original_df[1:lenN],
    required_packages = required_packages
  )
  saveRDS(object = dat,file=file_to_save_at)
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
  modelObj <- modeldat[["model_fit"]]
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



# library(logger)

#* @apiTitle Plumber Example API

# deployment_object <- readRDS("model_to_deploy.rds")
model_object <- deployment_object[["model_fit"]]
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

# if("additional_objects"%in%names(deployment_object)){
#   add_object_list <- deployment_object[["additional_objects"]]
#   stopifnot(is.list(add_object_list))
#   for (nm in names(add_object_list)){
#     print(str_glue("assigning {nm} to global environment"))
#     assign(nm,add_object_list[[nm]],envir = globalenv())
#   }
#   print("contents of the global environment")
#   print(ls())
#
# }

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg = "") {
    list(msg = paste0("The message is: '", msg, "'"))
}

if("additional_objects.Rdata"%in%list.files()){
  load("additional_objects.RData")
}


# rq <- list()
#* @post /score
function(req)
{
  if("additional_objects.RData"%in%list.files()){
  print("loading additional objects from objects.Rdata")
  load("additional_objects.RData")
  print("loaded additional objects from objects.Rdata")
  print(
    paste("loaded additional objects from additional file",
       paste0(ls(),collapse = ",")
    ))
  }

  # print(names(deployment_object))
  # if("additional_objects"%in%names(deployment_object)){
  #   add_object_list <- deployment_object[["additional_objects"]]
  #   stopifnot(is.list(add_object_list))
  #   for (nm in names(add_object_list)){
  #     print(str_glue("assigning {nm} to global environment"))
  #     assign(nm,add_object_list[[nm]],envir = globalenv())
  #   }
  #   print("contents of the global environment")
  #   print(ls())
  #
  # }

  print("request body")
  print(req)
  dat <- req[["postBody"]]

  print("request body processed")
  print(dat)
  df <- as.data.frame(fromJSON(dat))
  if("predict.arima"%in%ls()){
    print("predict.arima is in ls()")
  }else{
    print("ls():")
    print(ls())
    print(ls(envir = globalenv()))
  }
  print(class(model_object))
  print("Currently available methods for predict:")
  print(methods(predict))
  predict(model_object, newdata=df)
}
