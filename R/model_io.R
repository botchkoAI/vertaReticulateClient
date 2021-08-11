# Title     : TODO
# Objective : TODO
# Created by: sidi
# Created on: 23.06.21
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
#   print(names(dat))
  for (nm in c("model_file","required_packages","required_packages")) {
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

#     if(!file.exists(additional_objects_file_location)){
#       print(str_glue("Additional object location not found!"))
#     }else{
#       print(str_glue("Additional object location found!"))
#       mydocker$COPY(additional_objects_file_location, "/usr/scripts/additional_objects.RData")
#       print(str_glue("copied additional file to Docker!"))
#     }
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


#' @title Create Dockerfile From Saved Model Object
#' @description This function creates the docker context from either an R object in the workspace, or a file, containing such an object.
#' The object contains the model, package dependencies, a dictionary of additional R object, needed for it to work, and the source code of
#' files, needed to create the API. Those are stored with the log_model object, and finally
#' @param model_file_location a model file, created in the first place by by using createDockerContextZip. It should contain a
#' single R object, which is a dictionary, and which has the following fields:
#' A key note is that the predict function should have a method for the type of model_file
#' model_fit - a fit model object
#' formula_obj-Optional
#' original_df - Optional
#' required_packages - a vector of required packages to install
#' additional_objects- a named list of all adidtional objects to load, together with their names.
#' plumberTemplateFileLines -a vector of strings, representing the lines of one of the R source files, needed to create the API.
#' runnerTemplateFileLines -a vector of strings, representing the lines of the other R source file, needed to create the API.
#'
#' @return
#' @export
#'
#' @examples
createDockerContextZip <- function(
    model_file_location
){

# Steps:
# 1. Read the contents of the serialized file in memory (if the data is already in memory, instead write it to a )
# 2. Out of the plumberTemplateFileLines and runnerTemplateFileLines fields of that object,
# this part is needed only if we pass


  # create a temporary folder to work in.
  MAGIC_FOLDER_NAME <- createRandString()
  dir.create(MAGIC_FOLDER_NAME)
  # if we are passing a file name, then read that in memory
  if(is.character(model_file_location)){
      modelData <- readRDS(model_file_location)
      MAGIC_FILE_NAME <- model_file_location

        if(is.list(modelData)){
          if("model_fit"%in%names(modelData)){
            saveRDS(modelData,file=file.path(MAGIC_FOLDER_NAME,MAGIC_FILE_NAME))
          }
        }
	}
  else{
     # if the model is already in memory, write it to a temporary folder
      MAGIC_FILE_NAME <- "tmp_model_file.RData"
      while(dir.exists(MAGIC_FOLDER_NAME)){
        MAGIC_FOLDER_NAME <- createRandString()
      }
      dir.create(MAGIC_FOLDER_NAME)
        if(is.list(model_file_location)){
          if("model_fit"%in%names(model_file_location)){
            saveRDS(model_file_location,file=file.path(MAGIC_FOLDER_NAME,MAGIC_FILE_NAME))
          }
        }

      modelData <- readRDS(file.path(MAGIC_FOLDER_NAME,MAGIC_FILE_NAME))
  }

  # 2. Create source files from two of the fields in the passed dictionary
  plumberTemplateFileLines <- modelData[["plumberTemplateFileLines"]]
  runnerTemplateFileLines <- modelData[["runnerTemplateFileLines"]]
  runner_template_location <- "runner_template_location.R"
  plumber_template_location <- "plumber_template_location.R"
  runnerTemplate <-writeLines(runnerTemplateFileLines,runner_template_location)
  plumberTemplate <-writeLines(plumberTemplateFileLines,plumber_template_location)

  # 3. Copy the files to the temporary directory, containing the docker context
  #

  file.copy(runner_template_location,
            file.path(MAGIC_FOLDER_NAME,"runner_template.R"))
  file.copy(plumber_template_location,
            file.path(MAGIC_FOLDER_NAME,"plumber_template.R"))
  # 4. Start writing the Dockerfile
  # start from the rstudio/plumber dockerfile here:
  # https://hub.docker.com/r/rstudio/plumber/
  mydocker <- dockerfiler::Dockerfile$new("rstudio/plumber")

  #4.1 find all package dependencies, and add lines like
  #RUN R -e "install.packages(as.character(quote(__packagename__)))"
  # to the dockerfile. They will install said packages

  if("required_packages"%in%modelData){
     required_packages <- modelData[["required_packages"]]
  }else{
  required_packages <- c()
  }

  # 4.2 Add dockerfile instructions to install model-specific packages
  if(length(required_packages) > 0) {
    for(req in required_packages){
        if(length(req)>0){
          # NB: R's string interpolation is quite bad out of the box, and thus
          # we use the str_glue function from stringr
          mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote({req})))" '))
        }
      }
    }

    # 4.3 Write instructions in the dockerfile to add some some basic additional libraries,  to raise up an API
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(assertthat)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(renv)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(jsonlite)))" '))
    mydocker$RUN(stringr::str_glue('R -e "install.packages(as.character(quote(stringr)))" '))

    # 4.4. Instructions to create a directory for the entry point.
    mydocker$RUN("mkdir /usr/scripts")
    mydocker$RUN("cd /usr/scripts")

    # 4.5. Instructions to copy the needed files to the entry point directory
    # * the model file, downloaded from the artifact repository
    # * the two source code files
    mydocker$COPY(MAGIC_FILE_NAME, "/usr/scripts/model_file.RData")
    mydocker$COPY("plumber_template.R", "/usr/scripts/plumber.R")
    mydocker$COPY("runner_template.R", "/usr/scripts/run.R")


    # 4.6. Instructions to open port 8000
    mydocker$EXPOSE(8000)

    # 4.7 Instructions to move to the entry point and execute
    mydocker$WORKDIR("usr/scripts")
    # mydocker$RUN(r(plumber::plumb(file="plumber.R")))
    mydocker$ENTRYPOINT("R -f run.R --slave")
    # 4.8 write the whole Dockerfile
    mydocker$write(file.path(MAGIC_FOLDER_NAME,"Dockerfile"))
    # print(str_glue("DONE! Context Written in {MAGIC_FOLDER_NAME}"))


    # 5. zip the whole context dir to a zip file, and clean up the temporary directory
    zip(str_glue("{MAGIC_FOLDER_NAME}.zip"),files = MAGIC_FOLDER_NAME)
    print(str_glue("DONE! Context Written in {MAGIC_FOLDER_NAME}.zip"))
    # delete the dir
    unlink(MAGIC_FOLDER_NAME,recursive = F)
    print(str_glue("DONE! Context Written in {MAGIC_FOLDER_NAME}.zip"))
}


#' @title Create an Object, Deployable to the Verta System
#' @description This function gathers R objects from the workspace, needed to deploy an API endpoint and is intended to be used with log_model.
#' It simply gathers all needed objects, puts them in a list, and saves the list to disk. Then that file is logged with run$log_model.
#' This object can then be turned into a docker context with createDockerContextZip
#' @param file_to_save_at target file path
#' @param modFile the fit model object
#' @param required_packages character vector of required packages to call the predict function
#' @param runner_template_location - a location of the runner template file- by default taken from the package
#' @param plumber_template_location - a location of the plumber template file- by default taken from the package directory
#' @return nothing
#' @export
#'
#' @examples
save_model_data <- function(file_to_save_at,
                            modFile,
                            required_packages=NULL,
                            additional_objects=list(),
                            plumber_template_location = file.path(path.package("vertaReticulateClient"),
                                                                  "plumber_sample.R"
                            ),
                          runner_template_location = file.path(
                            path.package("vertaReticulateClient"), "to_run.R" )
) {

  if(is.null(required_packages)) {
    required_packages = c()
  }

  plumberTemplateFileLines <- readLines(plumber_template_location)
  runnerTemplateFileLines <- readLines(runner_template_location)


  dat <- list(
    model_fit = modFile,
    required_packages = required_packages,
    additional_objects = additional_objects,
    plumberTemplateFileLines = plumberTemplateFileLines,
    runnerTemplateFileLines = runnerTemplateFileLines
  )
  saveRDS(object = dat,file=file_to_save_at)
}
