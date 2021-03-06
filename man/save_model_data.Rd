% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_io.R
\name{save_model_data}
\alias{save_model_data}
\title{Create an Object, Deployable to the Verta System}
\usage{
save_model_data(
  file_to_save_at,
  modelObject,
  required_packages = NULL,
  additional_objects = list(),
  plumber_template_location = file.path(path.package("vertaReticulateClient"),
    "plumber_sample.R"),
  runner_template_location = file.path(path.package("vertaReticulateClient"),
    "to_run.R")
)
}
\arguments{
\item{file_to_save_at}{target file path}

\item{required_packages}{character vector of required packages to call the predict function}

\item{plumber_template_location}{- a location of the plumber template file- by default taken from the package directory}

\item{runner_template_location}{- a location of the runner template file- by default taken from the package}

\item{modFile}{the fit model object}
}
\value{
nothing
}
\description{
This function gathers R objects from the workspace, needed to deploy an API endpoint and is intended to be used with log_model.
It simply gathers all needed objects, puts them in a list, and saves the list to disk. Then that file is logged with run$log_model.
This object can then be turned into a docker context with createDockerContextZip
}
