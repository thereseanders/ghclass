% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/team_delete.R
\name{team_delete}
\alias{team_delete}
\title{Delete team}
\usage{
team_delete(org, team, prompt = TRUE)
}
\arguments{
\item{org}{Character. Name of the GitHub organization.}

\item{team}{Character. Name of the GitHub team within that organization.}

\item{prompt}{Logical. Should the user be prompted before deleting repositories. Default \code{true}.}
}
\description{
\code{team_delete} deletes an existing team from a GitHub organization.
}
