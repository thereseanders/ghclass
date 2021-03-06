% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/org_remove.R
\name{org_remove}
\alias{org_remove}
\title{Remove a member from an organization}
\usage{
org_remove(org, user, prompt = TRUE)
}
\arguments{
\item{org}{Character. Name of the GitHub organization.}

\item{user}{Character. Name of one or more GitHub users.}

\item{prompt}{Logical. Should the user be prompted before deleting repositories. Default \code{true}.}
}
\description{
\code{org_remove} removes a user from the organization and all teams within that organzation.
}
