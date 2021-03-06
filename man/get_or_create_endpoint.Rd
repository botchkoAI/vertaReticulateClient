% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client_api_scaffold.R
\name{get_or_create_endpoint}
\alias{get_or_create_endpoint}
\title{get_or_create_endpoint}
\usage{
get_or_create_endpoint(
  path = NULL,
  description = NULL,
  workspace = NULL,
  public_within_org = NULL,
  visibility = NULL,
  id = NULL
)
}
\arguments{
\item{path}{Path for the endpoint.}

\item{description}{Description of the endpoint.}

\item{workspace}{Workspace under which the endpoint with name name exists. If not provided, the current}

\item{public_within_org}{If creating an endpoint in an organization's workspace: True}

\item{visibility}{Visibility to set when creating this endpoint. If not provided, an}

\item{id}{ID of the endpoint. This parameter cannot be provided alongside name, and other}
}
\value{
:class:`verta.endpoint._endpoint.Endpoint`
}
\description{
Attaches an endpoint to this Client.
}
\details{
If an accessible endpoint with name `path` does not already exist, it will be created
and initialized with specified metadata parameters. If such an endpoint does already exist,
it will be retrieved; specifying metadata parameters in this case will raise a warning.
}
