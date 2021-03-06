github_api_team_delete = function(team_id) {
  gh::gh("DELETE /teams/:team_id",
         team_id = team_id,
         .token = github_get_token())
}

#' Delete team
#'
#' `team_delete` deletes an existing team from a GitHub organization.
#'
#' @param org Character. Name of the GitHub organization.
#' @param team Character. Name of the GitHub team within that organization.
#' @param prompt Logical. Should the user be prompted before deleting repositories. Default `true`.
#'
#' @export
#'
team_delete = function(org, team, prompt = TRUE) {

  arg_is_chr_scalar(org)
  arg_is_chr(team, allow_null = TRUE)
  arg_is_lgl_scalar(prompt)

  if (prompt) {
    delete = usethis::ui_yeah( paste(
      "This command will delete the following teams permanently:",
      "{usethis::ui_value(team)}."
    ) )
    if (!delete) {
      return(invisible())
    }
  }

  team = team_id_lookup(team, org)

  purrr::pwalk(
    team,
    function(team, id) {

      if (is.na(id)) {
        usethis::ui_oops("Team {usethis::ui_value(team)} does not exist in org {usethis::ui_value(org)}.")
        return()
      }

      res = purrr::safely(github_api_team_delete)(id)

      status_msg(
        res,
        glue::glue("Deleted team {usethis::ui_value(team)} from org {usethis::ui_value(org)}."),
        glue::glue("Failed to delete team {usethis::ui_value(team)} from org {usethis::ui_value(org)}.")
      )
    }
  )
}
