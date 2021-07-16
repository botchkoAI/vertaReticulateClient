#' Install Verta Library
#' @param method - auto, virtualenv, or conda
#' @param conda The path to a conda executable. Use "auto" to allow reticulate
#' to automatically find an appropriate conda binary. See
#' Finding Conda in reticulate  for more details.
#' @param envname  The name, or full path, of the environment in which Python
#' packages are to be installed. When NULL (the default), the active environment
#'  as set by the RETICULATE_PYTHON_ENV variable will be used; if that is unset,
#'   then the r-reticulate environment will be used.
#' @param
#'
#'
#' @return
#' @export
#'
#' @examples
install_verta <- function(
  method = c("auto", "virtualenv", "conda"),
  conda = "auto",
  envname = "verta_reticulate",
  extra_packages = NULL,
  conda_python_version = "3.9.5",
  ...


) {


  requireNamespace("reticulate", quietly = TRUE)
  if (!isNamespaceLoaded("reticulate"))
    stop('couldn\'t load reticulate package')

  method <- match.arg(method)
  # unroll version

  package <- "verta"

  extra_packages <- unique(extra_packages)

  reticulate::py_install(
    packages       = c(package,"psutil", extra_packages),
    envname        = envname,
    method         = method,
    conda          = conda,
    python_version = conda_python_version,
    pip            = TRUE,
    ...
  )


  # reticulate::conda_create(envname = "verta_reticulate")
  # reticulate::use_condaenv("verta_reticulate")
  # reticulate::py_available(initialize = T)
  # reticulate::py_install(packages = 'verta')
  # tryCatch({
  #   reticulate::import('psutil')
  # }, error = function(e){
  #   reticulate::py_install(packages = 'psutil')
  # })

}


#' Initialize Verta Client object
#'
#' @param HOST
#' @param method - auto, virtualenv, or conda
#' @param conda - path to conda executable
#' @param envname
#' @param python one of [python,conda, miniconda, venv]
#' @param python_path path, optional
#'
#' @return
#' @export
#'
#' @examples
init_verta <- function(HOST,
                       method = "conda",
                       conda = "auto",
                       envname = "verta_reticulate",
                       extra_packages = NULL,
                       conda_python_version = "3.9.5",
                       # python = NULL,
                        python_path=NULL
                       # condaenv = "verta_reticulate"
) {
  # username <- Sys.getenv("VERTA_EMAIL")
  # token <- Sys.getenv("VERTA_DEV_KEY")
  requireNamespace("reticulate", quietly = TRUE)
  if (!isNamespaceLoaded("reticulate"))
    stop('couldn\'t load reticulate package')
  # reticulate::py_available()
  if(length(method)>1){
    method <- "conda"
  }
  library(reticulate)

if(T){
  use_condaenv(condaenv = envname,required = T)
  1
}else{

  tryCatch({
    switch(
      method,
      python= {
        use_python(python_path,required=T)
        },
      conda = {
        use_condaenv(condaenv = envname,required = T)
        },
      virtualenv = {
        use_virtualenv(envname,required = T)
        },
      miniconda = {
        use_miniconda(envname,required = T)
      }
    )


  },
  error= function(e){
    print(e)
    install_verta( method = method, conda = conda, envname = envname,
       extra_packages = NULL,
       conda_python_version = conda_python_version  )
    switch(
      method,
      python= {
        use_python(python_path,required=T)
        },
      conda = {
        use_condaenv(condaenv = envname,required = T)
        },
      virtualenv = {
        use_virtualenv(envname,required = T)
        },
      miniconda = {
        use_miniconda(envname,required = T)
      }
    )


  }


  )


  switch(
      method,
      python= {
        use_python(python_path,required=T)
        },
      conda = {
        use_condaenv(condaenv = envname,required = T)
        },
      virtualenv = {
        use_virtualenv(envname,required = T)
        },
      miniconda = {
        use_miniconda(envname,required = T)
      }
    )

  # py_install <- switch(method,
  #                  python =install_python,
  #                  conda = py_insta,
  #                  miniconda =use_miniconda,
  #                  virtualenv=use_virtualenv
  # )
  # if(!is.null(python)){
  #   switch (python,
  #           python = reticulate::use_python(python = python_path, required = TRUE),
  #           conda = reticulate::use_condaenv(condaenv = python_path, required = TRUE),
  #           miniconda = reticulate::use_miniconda(condaenv = python_path, required = TRUE),
  #           venv = reticulate::use_virtualenv(virtualenv = python_path, required = TRUE),
  #           stop('Invalid python argument, should be one of [python, conda, miniconda, venv]')
  #   )
  # }

  # tryCatch({
  # py_use(condaenv,required = TRUE)
  # },error=function(e){
  #   print(str_glue("Couldn't initialize conda env {condaenv}. Attempting to install..."))
  # reticulate::conda_create(envname = "verta_reticulate")
  # reticulate::use_condaenv(condaenv=condaenv,required = TRUE)
  #
  # py_use(condaenv=condaenv,required = TRUE)
  #
  # })
}

  # stopifnot(reticulate::py_available(initialize = TRUE))
  # tryCatch({
  #   verta <- reticulate::import("verta")
  # }, error = function(e) {
  #   message('couldn\'t import verta client. Trying to install')
  #   install_verta()
  #   print("Successfully Installed Verta")
  #
  #   verta <- reticulate::import("verta")
  #   print("Successfully loaded Verta")
  # })
  #

  verta <- reticulate::import("verta")

  # options(verta=verta)
  client <- (verta$Client(
    host = HOST
    # project_qualified_name = project_name
  ))

  options(verta_client=client)
  return(client)
}


#---------------------

get_verta_client <- function() {
  ret <- getOption('verta_client')
  if (is.null('neptune'))
    stop('Please call init_verta first')
  return(ret)
}



#' @title set_project
#'
#' @description Attaches a Project to this Client.
#'
#' @details If an accessible Project with name `name` does not already exist, it will be created
#' and initialized with specified metadata parameters. If such a Project does already exist,
#' it will be retrieved; specifying metadata parameters in this case will raise a warning. If an Experiment is already attached to this Client, it will be detached.
#'
#' @param name Name of the Project. If no name is provided, one will be generated.
#' @param desc Description of the Project.
#' @param tags Tags of the Project.
#' @param attrs Attributes of the Project.
#' @param workspace Workspace under which the Project with name name exists. If not provided, the current
#' user's personal workspace will be used.
#' @param public_within_org If creating a Project in an organization's workspace: True for
# public, False for private. In older backends, default is
# private; in newer backends, uses the org's settings by default.
#' @param visibility <string>:28: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Visibility to set when creating this project. If not provided, an
# appropriate default will be used. This parameter should be
# preferred over public_within_org.
#' @param id ID of the Project. This parameter cannot be provided alongside name, and other
# parameters will be ignored.
#'
#' @return :class:`~verta._tracking.project.Project`
#'
#' @export
set_verta_project <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL, id = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$set_project(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility,
    id = id
  )
  return(python_function_result)
}


#' @title set_experiment
#'
#' @description Attaches an Experiment under the currently active Project to this Client.
#'
#' @details If an accessible Experiment with name `name` does not already exist under the currently
#' active Project, it will be created and initialized with specified metadata parameters. If
#' such an Experiment does already exist, it will be retrieved; specifying metadata parameters
#' in this case will raise a warning.
#'
#' @param name Name of the Experiment. If no name is provided, one will be generated.
#' @param desc Description of the Experiment.
#' @param tags Tags of the Experiment.
#' @param attrs Attributes of the Experiment.
#' @param id ID of the Experiment. This parameter cannot be provided alongside name, and other
#' parameters will be ignored.
#'
#' @return :class:`~verta._tracking.experiment.Experiment`
#'
#' @export
set_verta_experiment <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL, id = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$set_experiment(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs,
    id = id
  )
  return(python_function_result)
}

#' @title create_endpoint
#'
#' @description Attaches an endpoint to this Client.
#'
#' @details An accessible endpoint with name `name` will be created and initialized with specified metadata parameters.
#'
#' @param path Path for the endpoint.
#' @param description Description of the endpoint.
#' @param workspace Workspace under which the endpoint with name name exists. If not provided, the current
# user's personal workspace will be used.
#' @param public_within_org If creating an endpoint in an organization's workspace: True
# for public, False for private. In older backends, default is
# private; in newer backends, uses the org's settings by default.
#' @param visibility <string>:21: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Trying "ref" as canonical role name.
#
# <string>:21: (ERROR/3) Unknown interpreted text role "ref".
#
# Visibility to set when creating this endpoint. If not provided, an
# appropriate default will be used. This parameter should be
# preferred over public_within_org.
#'
#' @return :class:`~verta.registry._entities.model.RegisteredModel`
# <string>:25: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
#
# <string>:25: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_create_endpoint <- function(path, description = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$create_endpoint(
    path = path,
    description = description,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility
  )
  return(python_function_result)
}

#' @title create_experiment
#'
#' @description Creates a new Experiment under the currently active Project.
#'
#' @details Experiment with name `name` will be created and initialized with specified metadata parameters.
#'
#' @param name Name of the Experiment. If no name is provided, one will be generated.
#' @param desc Description of the Experiment.
#' @param tags Tags of the Experiment.
#' @param attrs Attributes of the Experiment.
#'
#' @return :class:`~verta._tracking.experiment.Experiment`
# <string>:18: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:18: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_create_experiment <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$create_experiment(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs
  )
  return(python_function_result)
}


#' @title create_project
#'
#' @description Creates a new Project.
#'
#' @details A Project with name `name` will be created and initialized with specified metadata parameters. If an Experiment is already attached to this Client, it will be detached.
#'
#' @param name Name of the Project. If no name is provided, one will be generated.
#' @param desc Description of the Project.
#' @param tags Tags of the Project.
#' @param attrs Attributes of the Project.
#' @param workspace Workspace under which the Project with name name exists. If not provided, the current
# user's personal workspace will be used.
#' @param public_within_org If creating a Project in an organization's workspace: True for
# public, False for private. In older backends, default is
# private; in newer backends, uses the org's settings by default.
#' @param visibility <string>:27: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Trying "ref" as canonical role name.
# <string>:27: (ERROR/3) Unknown interpreted text role "ref".
# Visibility to set when creating this project. If not provided, an
# appropriate default will be used. This parameter should be
# preferred over public_within_org.
#'
#' @return :class:`~verta._tracking.project.Project`
# <string>:31: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:31: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_create_project <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$create_project(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility
  )
  return(python_function_result)
}

#' @title create_registered_model
#'
#' @description Creates a new Registered Model.
#'
#' @details A registered_model with name `name` does will be created and initialized with specified metadata parameters.
#'
#' @param name name
#' @param desc desc
#' @param labels labels
#' @param workspace workspace
#' @param public_within_org public_within_org
#' @param visibility visibility
#'
#' @return :class:`~verta.registry._entities.model.RegisteredModel`
# <string>:28: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:28: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_create_registered_model <- function(name = NULL, desc = NULL, labels = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$create_registered_model(
    name = name,
    desc = desc,
    labels = labels,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility
  )
  return(python_function_result)
}

#' @title download_endpoint_manifest
#'
#' @description Downloads this endpoint's Kubernetes manifest YAML.
#'
#' @details
#'
#' @param download_to_path Local path to download manifest YAML to.
#' @param path Path of the endpoint.
#' @param name Name of the endpoint.
#' @param strategy <string>:11: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Trying "ref" as canonical role name.
# <string>:11: (ERROR/3) Unknown interpreted text role "ref".
# Strategy (direct or canary) for updating the endpoint.
# #' @param resources <string>:13: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:13: (ERROR/3) Unknown interpreted text role "class".
# Resources allowed for the updated endpoint.
#' @param autoscaling <string>:15: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:15: (ERROR/3) Unknown interpreted text role "class".
# Autoscaling condition for the updated endpoint.
#' @param env_vars Environment variables.
#' @param workspace Workspace for the endpoint. If not provided, the current user's
# personal workspace will be used.
#'
#' @return downloaded_to_path
# str
# Absolute path where deployment YAML was downloaded to. Matches download_to_path.
#'
#' @export
verta_download_endpoint_manifest <- function(download_to_path, path, name, strategy = NULL, resources = NULL, autoscaling = NULL, env_vars = NULL, workspace = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$download_endpoint_manifest(
    download_to_path = download_to_path,
    path = path,
    name = name,
    strategy = strategy,
    resources = resources,
    autoscaling = autoscaling,
    env_vars = env_vars,
    workspace = workspace
  )
  return(python_function_result)
}

#' @title find_datasets
#'
#' @description
#'
#' @details
#'
#' @param dataset_ids dataset_ids
#' @param name name
#' @param tags tags
#' @param sort_key sort_key
#' @param ascending ascending
#' @param workspace workspace
#'
#' @export
verta_find_datasets <- function(dataset_ids = NULL, name = NULL, tags = NULL, sort_key = NULL, ascending = FALSE, workspace = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$find_datasets(
    dataset_ids = dataset_ids,
    name = name,
    tags = tags,
    sort_key = sort_key,
    ascending = ascending,
    workspace = workspace
  )
  return(python_function_result)
}

#' @title get_dataset
#'
#' @description Gets a dataset.
#'
#' @details .. versionchanged:: 0.16.0 The dataset versioning interface was overhauled.
#'
#' @param name Name of the dataset. This parameter cannot be provided alongside id.
#' @param workspace Workspace under which the dataset with name name exists. If not provided, the current
# user's personal workspace will be used.
#' @param id ID of the dataset. This parameter cannot be provided alongside name.
#'
#' @return :class:`~verta._dataset_versioning.dataset.Dataset`
# <string>:18: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
#
# <string>:18: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_dataset <- function(name = NULL, workspace = NULL, id = NULL) {

  cl <- get_verta_client()
  python_function_result <- cl$get_dataset(
    name = name,
    workspace = workspace,
    id = id
  )
  return(python_function_result)
}

#' @title get_dataset_version
#'
#' @description Gets a dataset version.
#'
#' @details .. versionchanged:: 0.16.0 The dataset versioning interface was overhauled.
#'
#' @param id ID of the dataset version.
#'
#' @return :class:`~verta._dataset_versioning.dataset_version.DatasetVersion`
# <string>:13: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:13: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_dataset_version <- function(id) {

  cl <- get_verta_client()
  python_function_result <- cl$get_dataset_version(
    id = id
  )
  return(python_function_result)
}

#' @title get_endpoint
#'
#' @description Retrieves an already created Endpoint. Only one of `path` or `id` can be provided.
#'
#' @details
#'
#' @param path Path of the Endpoint.
#' @param workspace Name of the workspace of the Endpoint.
#' @param id ID of the Endpoint. This parameter cannot be provided alongside path.
#'
#' @return :class:`~verta.endpoint._endpoint.Endpoint`
# <string>:14: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:14: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_endpoint <- function(path = NULL, workspace = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_endpoint(
    path = path,
    workspace = workspace,
    id = id
  )
  return(python_function_result)
}

#' @title get_experiment
#'
#' @description Retrieves an already created Experiment. Only one of `name` or `id` can be provided.
#'
#' @details
#'
#' @param name Name of the Experiment.
#' @param id ID of the Experiment. This parameter cannot be provided alongside name.
#'
#' @return :class:`~verta._tracking.experiment.Experiment`
# <string>:12: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:12: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_experiment <- function(name = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_experiment(
    name = name,
    id = id
  )
  return(python_function_result)
}

#' @title get_experiment_run
#'
#' @description Retrieves an already created Experiment Run. Only one of `name` or `id` can be provided.
#'
#' @details
#'
#' @param name Name of the Experiment Run.
#' @param id ID of the Experiment Run. This parameter cannot be provided alongside name.
#'
#' @return :class:`~verta._tracking.experimentrun.ExperimentRun`
# <string>:12: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:12: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_experiment_run <- function(name = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_experiment_run(
    name = name,
    id = id
  )
  return(python_function_result)
}

#' @title get_or_create_endpoint
#'
#' @description Attaches an endpoint to this Client.
#'
#' @details If an accessible endpoint with name `path` does not already exist, it will be created
#' and initialized with specified metadata parameters. If such an endpoint does already exist,
#' it will be retrieved; specifying metadata parameters in this case will raise a warning.
#'
#' @param path Path for the endpoint.
#' @param description Description of the endpoint.
#' @param workspace Workspace under which the endpoint with name name exists. If not provided, the current
# user's personal workspace will be used.
#' @param public_within_org If creating an endpoint in an organization's workspace: True
# for public, False for private. In older backends, default is
# private; in newer backends, uses the org's settings by default.
#' @param visibility <string>:22: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Trying "ref" as canonical role name.
# <string>:22: (ERROR/3) Unknown interpreted text role "ref".
# Visibility to set when creating this endpoint. If not provided, an
# appropriate default will be used. This parameter should be
# preferred over public_within_org.
#' @param id ID of the endpoint. This parameter cannot be provided alongside name, and other
# parameters will be ignored.
#'
#' @return :class:`~verta.endpoint._endpoint.Endpoint`
# <string>:30: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:30: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_or_create_endpoint <- function(path = NULL, description = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_or_create_endpoint(
    path = path,
    description = description,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility,
    id = id
  )
  return(python_function_result)
}

#' @title get_or_create_registered_model
#'
#' @description Attaches a registered_model to this Client.
#'
#' @details If an accessible registered_model with name `name` does not already exist, it will be created
#' and initialized with specified metadata parameters. If such a registered_model does already exist,
#' it will be retrieved; specifying metadata parameters in this case will raise a warning.
#'
#' @param name name
#' @param desc desc
#' @param labels labels
#' @param workspace workspace
#' @param public_within_org public_within_org
#' @param visibility visibility
#' @param id id
#'
#' @return :class:`~verta.registry._entities.model.RegisteredModel`
# <string>:33: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:33: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_or_create_registered_model <- function(name = NULL, desc = NULL, labels = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_or_create_registered_model(
    name = name,
    desc = desc,
    labels = labels,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility,
    id = id
  )
  return(python_function_result)
}

#' @title get_or_create_repository
#'
#' @description Gets or creates a Repository by `name` and `workspace`, or gets a Repository by `id`.
#'
#' @details
#'
#' @param name Name of the Repository. This parameter cannot be provided alongside id.
#' @param workspace Workspace under which the Repository with name name exists. If not provided, the
# current user's personal workspace will be used.
#' @param id ID of the Repository, to be provided instead of name.
#' @param public_within_org If creating a Repository in an organization's workspace: True
# for public, False for private. In older backends, default is
# private; in newer backends, uses the org's settings by default.
# #' @param visibility <string>:19: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Trying "ref" as canonical role name.
#
# <string>:19: (ERROR/3) Unknown interpreted text role "ref".
#
# Visibility to set when creating this repository. If not provided,
# an appropriate default will be used. This parameter should be
# preferred over public_within_org.
#'
#' @return :class:`~verta._repository.Repository`
# <string>:23: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
#
# <string>:23: (ERROR/3) Unknown interpreted text role "class".
#
# Specified Repository.
#'
#' @export
verta_get_or_create_repository <- function(name = NULL, workspace = NULL, id = NULL, public_within_org = NULL, visibility = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_or_create_repository(
    name = name,
    workspace = workspace,
    id = id,
    public_within_org = public_within_org,
    visibility = visibility
  )
  return(python_function_result)
}

#' @title get_project
#'
#' @description Retrieves an already created Project. Only one of `name` or `id` can be provided.
#'
#' @details
#'
#' @param name Name of the Project.
#' @param workspace Workspace under which the Project with name name exists. If not provided, the current
# user's personal workspace will be used.
#' @param id ID of the Project. This parameter cannot be provided alongside name.
#'
#' @return :class:`~verta._tracking.project.Project`

# <string>:15: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
#
# <string>:15: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_project <- function(name = NULL, workspace = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$get_project(
    name = name,
    workspace = workspace,
    id = id
  )
  return(python_function_result)
}

#' @title get_registered_model_version
#'
#' @description Retrieve an already created Model Version.
#'
#' @details
#'
#' @param id ID of the Model Version.
#'
#' @return :class:`~verta.registry._entities.modelversion.RegisteredModelVersion`
# <string>:10: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
# <string>:10: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_get_registered_model_version <- function(id) {
  cl <- get_verta_client()

  python_function_result <- cl$get_registered_model_version(
    id = id
  )
  return(python_function_result)
}

#' @title set_experiment
#'
#' @description Attaches an Experiment under the currently active Project to this Client.
#'
#' @details If an accessible Experiment with name `name` does not already exist under the currently
#' active Project, it will be created and initialized with specified metadata parameters. If
#' such an Experiment does already exist, it will be retrieved; specifying metadata parameters
#' in this case will raise a warning.
#'
#' @param name Name of the Experiment. If no name is provided, one will be generated.
#' @param desc Description of the Experiment.
#' @param tags Tags of the Experiment.
#' @param attrs Attributes of the Experiment.
#' @param id ID of the Experiment. This parameter cannot be provided alongside name, and other
# parameters will be ignored.
#'
#' @return :class:`~verta._tracking.experiment.Experiment`

# <string>:24: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
#
# <string>:24: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_set_experiment <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$set_experiment(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs,
    id = id
  )
  return(python_function_result)
}

#' @title set_project
#'
#' @description Attaches a Project to this Client.
#'
#' @details If an accessible Project with name `name` does not already exist, it will be created
#' and initialized with specified metadata parameters. If such a Project does already exist,
#' it will be retrieved; specifying metadata parameters in this case will raise a warning. If an Experiment is already attached to this Client, it will be detached.
#'
#' @param name Name of the Project. If no name is provided, one will be generated.
#' @param desc Description of the Project.
#' @param tags Tags of the Project.
#' @param attrs Attributes of the Project.
#' @param workspace Workspace under which the Project with name name exists. If not provided, the current
# user's personal workspace will be used.
#' @param public_within_org If creating a Project in an organization's workspace: True for
# public, False for private. In older backends, default is
# private; in newer backends, uses the org's settings by default.
#' @param visibility <string>:28: (INFO/1) No role entry for "ref" in module "docutils.parsers.rst.languages.en".
# Trying "ref" as canonical role name.
#
# <string>:28: (ERROR/3) Unknown interpreted text role "ref".
#
# Visibility to set when creating this project. If not provided, an
# appropriate default will be used. This parameter should be
# preferred over public_within_org.
#' @param id ID of the Project. This parameter cannot be provided alongside name, and other
# parameters will be ignored.
#'
#' @return :class:`~verta._tracking.project.Project`
# <string>:36: (INFO/1) No role entry for "class" in module "docutils.parsers.rst.languages.en".
# Trying "class" as canonical role name.
#
# <string>:36: (ERROR/3) Unknown interpreted text role "class".
#'
#' @export
verta_set_project <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL, workspace = NULL, public_within_org = NULL, visibility = NULL, id = NULL) {
  cl <- get_verta_client()

  python_function_result <- cl$set_project(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs,
    workspace = workspace,
    public_within_org = public_within_org,
    visibility = visibility,
    id = id
  )
  return(python_function_result)
}

#' @title set_workspace
#'
#' @description Sets the active workspace for this client instance.
#'
#' @details .. versionadded:: 0.17.0 Parameters
#' ----------
#' workspace : str Verta workspace.
#'
#' @param workspace Verta workspace.
#'
#' @section workspace : str:
#' Verta workspace.
#'
#' @export
verta_set_workspace <- function(workspace) {
  cl <- get_verta_client()

  python_function_result <- cl$set_workspace(
    workspace = workspace
  )
  return(python_function_result)
}

#' @title log_model
#'
#' @description Logs a model to this Model Version.

#'
#' workspace : str Verta workspace.
#'
#' @param model r object list with the following fields:
#' @param custom_modules custom modules.
#'
#' @section workspace : str:
#' Verta workspace.
#'
#' @export
verta_log_model <- function(run,model) {
  stopifnot(run, "verta.tracking.entities.ExperimentRun")
  # cl <- get_verta_client()

  python_function_result <- run$log_model(
    workspace = workspace
  )
  return(python_function_result)
}



#' @title set_experiment_run
#'
#' @description Attaches an Experiment Run under the currently active Experiment to this Client.
#'
#' @details If an accessible Experiment Run with name `name` does not already exist under the
#' currently active Experiment, it will be created and initialized with specified metadata
#' parameters. If such a Experiment Run does already exist, it will be retrieved; specifying
#' metadata parameters in this case will raise a warning. Parameters
#' ----------
#' name : str, optional Name of the Experiment Run. If no name is provided, one will be generated.
#' desc : str, optional Description of the Experiment Run.
#' tags : list of str, optional Tags of the Experiment Run.
#' attrs : dict of str to {NULL, bool, float, int, str}, optional Attributes of the Experiment Run.
#' id : str, optional ID of the Experiment Run. This parameter cannot be provided alongside `name`, and other parameters will be ignored. Returns
#' -------
#' :class:`~verta.tracking.entities.ExperimentRun` Raises
#' ------
#' ValueError If an Experiment Run with `name` already exists, but metadata parameters are passed in.
#' AttributeError If an Experiment is not yet in progress.
#'
#' @param name name
#' @param desc desc
#' @param tags tags
#' @param attrs attrs
#' @param id id
#' @param date_created date_created
#'
#' @section name : str, optional:
#' Name of the Experiment Run. If no name is provided, one will be generated. desc : str, optional Description of the Experiment Run. tags : list of str, optional Tags of the Experiment Run. attrs : dict of str to {NULL, bool, float, int, str}, optional Attributes of the Experiment Run. id : str, optional ID of the Experiment Run. This parameter cannot be provided alongside `name`, and other parameters will be ignored.
#'
#' @section desc : str, optional:
#' Description of the Experiment Run. tags : list of str, optional Tags of the Experiment Run. attrs : dict of str to {NULL, bool, float, int, str}, optional Attributes of the Experiment Run. id : str, optional ID of the Experiment Run. This parameter cannot be provided alongside `name`, and other parameters will be ignored.
#'
#' @section tags : list of str, optional:
#' Tags of the Experiment Run. attrs : dict of str to {NULL, bool, float, int, str}, optional Attributes of the Experiment Run. id : str, optional ID of the Experiment Run. This parameter cannot be provided alongside `name`, and other parameters will be ignored.
#'
#' @section attrs : dict of str to {None, bool, float, int, str}, optional:
#' Attributes of the Experiment Run. id : str, optional ID of the Experiment Run. This parameter cannot be provided alongside `name`, and other parameters will be ignored.
#'
#' @section id : str, optional:
#' ID of the Experiment Run. This parameter cannot be provided alongside `name`, and other parameters will be ignored.
#'
#' @export
set_experiment_run <- function(name = NULL, desc = NULL, tags = NULL, attrs = NULL, id = NULL, date_created = NULL) {
 cl <- get_verta_client()
  python_function_result <- cl$set_experiment_run(
    name = name,
    desc = desc,
    tags = tags,
    attrs = attrs,
    id = id,
    date_created = date_created
  )
  return(python_function_result)

}
# Warning




#' @title log_hyperparameters
#'
#' @description Logs potentially multiple hyperparameters to this Experiment Run.
#'
#' @details Parameters
#' ----------
#' hyperparameters : dict of str to {NULL, bool, float, int, str} Hyperparameters.
#' overwrite : bool, default FALSE Whether to allow overwriting an existing hyperparameter with key `key`.
#'
#' @param run The experiment run in question
#' @param hyperparams hyperparams
#' @param overwrite overwrite
#'
#' @section hyperparameters : dict of str to {None, bool, float, int, str}:
#' Hyperparameters. overwrite : bool, default FALSE Whether to allow overwriting an existing hyperparameter with key `key`.
#'
#' @section overwrite : bool, default False:
#' Whether to allow overwriting an existing hyperparameter with key `key`.
#'
#' @export
log_run_hyperparameters <- function(run,hyperparams, overwrite = FALSE) {
  stopifnot(
    is(run,"verta.tracking.entities.ExperimentRun")
  )

  python_function_result <- run$log_hyperparameters(
    hyperparams = hyperparams,
    overwrite = overwrite
  )
  return(python_function_result)

}



#' @title run_log_metric
#'
#' @description Logs a metric to this Experiment Run.
#'
#' @details If the metadatum of interest might recur, :meth:`.log_observation` should be used instead. Parameters
#' ----------
#' key : str Name of the metric.
#' value : one of {NULL, bool, float, int, str} Value of the metric.
#' overwrite : bool, default FALSE Whether to allow overwriting an existing metric with key `key`.
#'
#' @param key run
#' @param key key
#' @param value value
#' @param overwrite overwrite
#'
#' @section key : str:
#' Name of the metric. value : one of {NULL, bool, float, int, str} Value of the metric. overwrite : bool, default FALSE Whether to allow overwriting an existing metric with key `key`.
#'
#' @section value : one of {None, bool, float, int, str}:
#' Value of the metric. overwrite : bool, default FALSE Whether to allow overwriting an existing metric with key `key`.
#'
#' @section overwrite : bool, default False:
#' Whether to allow overwriting an existing metric with key `key`.
#'
#' @export
run_log_metric <- function(run,key, value, overwrite = FALSE) {

  stopifnot(
    is(run,"verta.tracking.entities.ExperimentRun")
  )

  python_function_result <- run$log_metric(
    key = key,
    value = value,
    overwrite = overwrite
  )
  python_function_result

}

#' @title get_best_run_in_experiment
#'
#' @description Sorts the results from this collection by `key`.
#'
#' @details A `key` is a string containing a dot-delimited property such as
#' ``metrics.accuracy``. Parameters
#' ----------
#' key : str Dot-delimited property.
#' descending : bool, default FALSE Order in which to return sorted results. Returns
#' -------
#' The same type of object given in the input. Examples
#' --------
#' .. code-block:: python runs.sort("metrics.accuracy") # <ExperimentRuns containing 3 runs>
#'
#' @param metric metric, written in the experiments. Should be of the form 'metrics.xxx', e.g. 'metrics.auc' or 'metrics.rsq'
#' @param descending descending
#'
#' @section key : metric:
#' Dot-delimited property. descending : bool, default FALSE Order in which to return sorted results.
#'
#' @section descending : bool, default False:
#' Order in which to return sorted results.
#'
#' @export
get_best_run_by_metric <- function(experiment,metric, descending = TRUE) {
  stopifnot(is(experiment,"verta.tracking.entities.Experiment"))
  python_function_result <- experiment$expt_runs$sort(
    key = metric,
    descending = descending
  )
  if(length(python_function_result)>0){
    python_function_result[1]
  }else{
    python_function_result
  }


}


