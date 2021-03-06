% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/client_api_scaffold.R
\name{get_best_run_by_metric}
\alias{get_best_run_by_metric}
\title{get_best_run_in_experiment}
\usage{
get_best_run_by_metric(experiment, metric, descending = TRUE)
}
\arguments{
\item{metric}{metric, written in the experiments. Should be of the form 'metrics.xxx', e.g. 'metrics.auc' or 'metrics.rsq'}

\item{descending}{descending}
}
\description{
Sorts the results from this collection by `key`.
}
\details{
A `key` is a string containing a dot-delimited property such as
``metrics.accuracy``. Parameters
----------
key : str Dot-delimited property.
descending : bool, default FALSE Order in which to return sorted results. Returns
-------
The same type of object given in the input. Examples
--------
.. code-block:: python runs.sort("metrics.accuracy") # <ExperimentRuns containing 3 runs>
}
\section{key }{
 metric:
Dot-delimited property. descending : bool, default FALSE Order in which to return sorted results.
}

\section{descending }{
 bool, default False:
Order in which to return sorted results.
}

