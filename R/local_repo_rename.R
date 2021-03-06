#' Rename local directories using a vector of patterns and replacements
#'
#' This is particularly helpful for renaming student repos to include
#' something more useful like `Last, First name` or `netid` so that
#' folder ordering matches student ordering within your LMS.
#'
#' @param repo_dir Character. Vector of repo directories or a single directory containing one or more repos.
#' @param pattern Character. One or more regexp patterns to match to directory names.
#' @param replacement Character.  One or more text strings containing the replacement value for matched patterns.
#'
#' @aliases rename_local_repo
#'
#' @export
#'
local_repo_rename = function(repo_dir, pattern, replacement) {
  arg_is_chr_scalar(repo_dir)
  arg_is_chr(pattern, replacement)

  # If there isn't a regex group, add one for the whole pattern
  if (!any(grepl(pattern = "\\(.*\\)", x = pattern))) {
    pattern = paste0("(", pattern, ")")
  }

  repos = repo_dir_helper(repo_dir)
  cur_repos = repos

  for(i in seq_along(pattern)) {
    repos = sub(pattern[i],replacement[i], repos)
  }

  sub = repos != cur_repos
  purrr::walk2(
    cur_repos[sub], repos[sub],
    function(cur, new) {
      res = purrr::safely(fs::file_move)(cur, new)

      pattern = glue::glue(".*?({repo_dir})")

      cur = sub(pattern, replacement = "\\1", x = cur)
      new = sub(pattern, replacement = "\\1", x = new)

      status_msg(
        res,
        glue::glue("Renaming {usethis::ui_value(cur)} to {usethis::ui_value(new)}."),
        glue::glue("Failed to rename {usethis::ui_value(cur)} to {usethis::ui_value(new)}.")
      )
    }
  )
}
