# Title     : TODO
# Objective : TODO
# Created by: sidi
# Created on: 23.06.21
library(stringr)

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
                            required_packages=NULL,
                            additional_objects=list()
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
    required_packages = required_packages,
    additional_objects = additional_objects
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
  for (nm in c("model_file","formula_obj","original_df","required_packages",
               "required_packages")) {
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


#' Create Dockerfile From Saved Model Object
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
createDockerfile <- function(
    model_file_location,
    required_packages=c("forecast"),
    plumber_template_location = file.path(path.package("vertaReticulateClient"),
                                          "plumber_sample.R",
    additional_objects_file_location = "additional_objects.RData"
    ),
  runner_template_location = file.path(path.package("vertaReticulateClient"), "to_run.R" )
){

  MAGIC_FILE_NAME <- str_glue("./tmp_model_file_{rpois(1,10000)}.RData")

  if(!is.character(model_file_location)){
    if(is.list(model_file_location)){
      if("model_file"%in%names(model_file_location)){
        saveRDS(model_file_location,file=MAGIC_FILE_NAME)
      }
    }
  }else{
    MAGIC_FILE_NAME <- model_file_location
  }
  if(!file.exists(additional_objects_file_location)){
    print(str_glue("Additional object location not found!"))
  }else{
    print(str_glue("Additional object location found!"))
  }

  mydocker <- dockerfiler::Dockerfile$new("rstudio/plumber")
  if(length(required_packages) > 0) {
    for(req in required_packages){
        if(length(req)>0){
          # mydocker$RUN(r(install.packages(eval(req))))
          mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote({req})))" '))
        }
      }
    }

    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(assertthat)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(renv)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(jsonlite)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(stringr)))" '))

    mydocker$RUN("mkdir /usr/scripts")
    mydocker$RUN("cd /usr/scripts")

    mydocker$COPY(MAGIC_FILE_NAME, "/usr/scripts/model_file.RData")
    mydocker$COPY(plumber_template_location, "/usr/scripts/plumber.R")
    mydocker$COPY(runner_template_location, "/usr/scripts/run.R")

    if(!file.exists(additional_objects_file_location)){
      print(str_glue("Additional object location not found!"))
    }else{
      print(str_glue("Additional object location found!"))
      mydocker$COPY(additional_objects_file_location, "/usr/scripts/additional_objects.RData")
      print(str_glue("copied additional file to Docker!"))
    }
    # mydocker$COPY(runner_template_location, "/usr/scripts/plumber.R")
    # mydocker$RUN
    mydocker$EXPOSE(8000)
    mydocker$WORKDIR("usr/scripts")
    # mydocker$RUN(r(plumber::plumb(file="plumber.R")))
    mydocker$ENTRYPOINT("R -f run.R --slave")
    mydocker$write("Dockerfile")
}



buildDocker <- function(location = ".",tag="verta-plumber"){
  system(str_glue("docker build -t {tag} {location}"))
}





runDocker <- function(location=".",tag = "verta-plumber"){
  buildDocker(location,tag)
  system(str_glue("docker run -p 8000:8000 {tag}"))
}

#' generate random string
#'
#' @return
#'
#' @examples
createRandString<- function(letter_len1=5,digit_len=4,letter_len2=1) {
  digits = 0:9 %>% as.character()
  v = c(sample(LETTERS, letter_len1, replace = TRUE),
        sample(digits, digit_len, replace = TRUE),
        sample(LETTERS, letter_len2, replace = TRUE))
  paste0("docker_folder_",paste0(v,collapse = ""),collapse = "")

}


#' Create Dockerfile From Saved Model Object
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
createDockerContextZip <- function(
    model_file_location,
    required_packages=c("forecast"),
    plumber_template_location = file.path(path.package("vertaReticulateClient"),
                                          "plumber_sample.R"
    ),
  runner_template_location = file.path(
    path.package("vertaReticulateClient"), "to_run.R" ),
  additional_objects_file_location = NULL
    # "/media/sidi/SSDPrograms/toptal/verta/VertaPackageTests/plumber_sample_file/to_run"
){

  # MAGIC_FILE_NAME <- str_glue("./tmp_model_file_{rpois(1,10000)}.RData")
  # MAGIC_FILE_NAME <- str_glue("./tmp_model_file_{rpois(1,10000)}.RData")
  MAGIC_FILE_NAME <- "tmp_model_file.RData"

  MAGIC_FOLDER_NAME <- createRandString()
  while(dir.exists(MAGIC_FOLDER_NAME)){
    MAGIC_FOLDER_NAME <- createRandString()
  }
  dir.create(MAGIC_FOLDER_NAME)


  if(!is.character(model_file_location)){
    if(is.list(model_file_location)){
      if("model_file"%in%names(model_file_location)){
        saveRDS(model_file_location,file=file.path(MAGIC_FOLDER_NAME,MAGIC_FILE_NAME))
      }
    }
  }else{
    MAGIC_FILE_NAME <- model_file_location
    file.copy(model_file_location,
              file.path(MAGIC_FOLDER_NAME,MAGIC_FILE_NAME))
  }


  file.copy(runner_template_location,
            file.path(MAGIC_FOLDER_NAME,"runner_template.R"))
  file.copy(plumber_template_location,
            file.path(MAGIC_FOLDER_NAME,"plumber_template.R"))
  if(!is.null(additional_objects_file_location)){
    file.copy(additional_objects_file_location,
              file.path(MAGIC_FOLDER_NAME,"additional_objects.RData"))
  }



  mydocker <- dockerfiler::Dockerfile$new("rstudio/plumber")
  if(length(required_packages) > 0) {
    for(req in required_packages){
        if(length(req)>0){
          # mydocker$RUN(r(install.packages(eval(req))))
          mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote({req})))" '))
        }
      }
    }

    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(assertthat)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(renv)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(jsonlite)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(stringr)))" '))

    mydocker$RUN("mkdir /usr/scripts")
    mydocker$RUN("cd /usr/scripts")

    mydocker$COPY(MAGIC_FILE_NAME, "/usr/scripts/model_file.RData")
    mydocker$COPY("plumber_template.R", "/usr/scripts/plumber.R")
    mydocker$COPY("runner_template.R", "/usr/scripts/run.R")
    if(!is.null(additional_objects_file_location)){
      mydocker$COPY("additional_objects.RData", "/usr/scripts/additional_objects.RData")
    }

    # mydocker$COPY(runner_template_location, "/usr/scripts/plumber.R")
    # mydocker$RUN
    mydocker$EXPOSE(8000)
    mydocker$WORKDIR("usr/scripts")
    # mydocker$RUN(r(plumber::plumb(file="plumber.R")))
    mydocker$ENTRYPOINT("R -f run.R --slave")
    mydocker$write(file.path(MAGIC_FOLDER_NAME,"Dockerfile"))
    # print(str_glue("DONE! Context Written in {MAGIC_FOLDER_NAME}"))
    zip("{MAGIC_FOLDER_NAME}.zip",files = MAGIC_FOLDER_NAME)
    print(str_glue("DONE! Context Written in {MAGIC_FOLDER_NAME}.zip"))
    # delete the dir
    unlink(MAGIC_FOLDER_NAME,recursive = F)
    print(str_glue("DONE! Context Written in {MAGIC_FOLDER_NAME}.zip"))
}
