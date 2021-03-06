#' @rdname local_repo
#' @export
local_repo_commit = function(repo_dir, message) {
  require_gert()
  arg_is_chr(repo_dir, message)
  repo_dir = repo_dir_helper(repo_dir)

  res = purrr::map2(
    repo_dir, message,
    function(dir, message) {
      res = purrr::safely(gert::git_commit)(
        message = message, repo = dir
      )

      repo = fs::path_file(dir)
      status_msg(
        res,
        glue::glue("Committed changes to {usethis::ui_value(repo)}."),
        glue::glue("Failed to commit changes to {usethis::ui_value(repo)}.")
      )

      res
    }
  )

  invisible(res)
}
