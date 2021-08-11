# Title     : TODO
# Objective : TODO
# Created by: sidi
# Created on: 28.06.21
# --- launch API ----
# plumb_path <- ""

library(stringr)

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
  print(names(dat))
  for (nm in c("model_fit","required_packages",
               "additional_objects")) {
    assertthat::assert_that(nm%in% names(dat))
  }

  return(dat)
}

if("additional_objects.RData"%in%list.files()){
  load("additional_objects.RData")
}

deployment_object <- load_model_from_metadata(
  "model_file.RData"
)

# mod_metadata$model_file


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

if("additional_objects"%in%names(deployment_object)){
  add_object_list <- deployment_object[["additional_objects"]]
  stopifnot(is.list(add_object_list))
  for (nm in names(add_object_list)){
    print(str_glue("assigning {nm} to global environment"))
    assign(nm,add_object_list[[nm]],envir = globalenv())
  }
  print("contents of the global environment")
  print(ls())

}



r <- plumber::plumb("plumber.R")
r$run(host = "0.0.0.0", port = 8000,swagger=T)

