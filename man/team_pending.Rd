% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/team_pending.R
\name{team_pending}
\alias{team_pending}
\alias{get_pending_team_members}
\alias{team_pending_members}
\title{Get pending team members}
\usage{
team_pending(org, team = org_teams(org))
}
\arguments{
\item{org}{character, name of the GitHub organization.}

\item{team}{character or data frame, listing one or more team}
}
\description{
\code{team_pending} returns a data frame of pending team members.
}
\examples{
\dontrun{
team_pending("ghclass",c("team01","team02"))
}

}
