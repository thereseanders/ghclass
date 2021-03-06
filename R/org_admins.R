github_api_org_admins = function(owner){
  gh::gh("GET /orgs/:owner/members",
         owner = owner,
         role = "admin",
         .token = github_get_token(),
         .limit = github_get_api_limit())
}

#' List repository administrators
#'
#' @param org Character. Name of a GitHub organization.
#'
#' @examples
#' \dontrun{
#' org_admins("ghclass-test")
#' org_admins("rundel")
#' }
#'
#' @return A character vector of repository administrators.
#'
#' @aliases get_admin
#'
#' @export
#'
org_admins = function(org) {
  arg_is_chr_scalar(org)

  res = purrr::safely(github_api_org_admins)(owner = org)

  if (failed(res)) {
    if (user_exists(org)) {
      ## In this case it is a user not an org, admin is just that user
      return(org)
    }

    usethis::ui_stop(glue::glue("Failed to retrieve admins for org {usethis::ui_value(org)}."))
  } else {
    purrr::map_chr(result(res), "login")
  }
}
