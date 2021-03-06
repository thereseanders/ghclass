% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/user_exists.R
\name{user_exists}
\alias{user_exists}
\alias{check_user_exists}
\title{Check if username(s) exists}
\usage{
user_exists(user)
}
\arguments{
\item{user}{Character. Username(s) to be checked.}
}
\value{
Vector of logical values
}
\description{
\code{user_exists} returns TRUE if the supplied username(s) exist on GitHub and FALSE otherwise.
}
\examples{
\dontrun{
user_exists(c("rundel","hopefullydoesnotexist"))
}

}
