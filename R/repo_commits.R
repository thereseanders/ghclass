# github_api_repo_commits_v4 = function(repo, branch = "master") {
#   arg_is_chr_scalar(repo, branch)
#
#   owner = get_repo_owner(repo)
#   name  = get_repo_name(repo)
#
#   query = '
#     query {
#       repository(owner: <graphql_quote(owner)>, name: <graphql_quote(name)>) {
#         object(expression: <graphql_quote(branch)>) {
#           ... on Commit {
#             history(first:100, after: <graphql_quote(cursor)>) {
#               totalCount
#               pageInfo {
#                 hasNextPage
#                 endCursor
#               }
#               nodes {
#                 author {
#                   email
#                   name
#                   user {
#                     login
#                   }
#                 }
#                 changedFiles
#                 message
#                 additions
#                 deletions
#                 committedDate
#               }
#             }
#           }
#         }
#       }
#     }
#   '
#
#   github_api_v4_graphql_paginated(
#     query,
#     vars = list(owner = owner, name = name, branch = branch),
#     page_info = c("repository", "object", "history")
#   )
# }


github_api_repo_commits = function(repo, sha=NULL, path=NULL, author=NULL, since=NULL, until=NULL) {
  args = list(
    endpoint = "GET /repos/:owner/:repo/commits",
    owner = get_repo_owner(repo),
    repo = get_repo_name(repo),
    .token = github_get_token()
  )

  args[["sha"]] = sha
  args[["path"]] = path
  args[["author"]] = author
  args[["since"]] = since
  args[["until"]] = until

  do.call(gh::gh, args)
}

#' Get repository commits
#'
#' `repo_commits` returns a tibble of repositories belonging to a GitHub organization along with some
#' basic statistics about those repositories.
#'
#' @param repo   Character. Address of repository in `owner/name` format.
#' @param branch Character.	Branch to list commits from.
#' @param sha	   Character.	SHA to start listing commits from.
#' @param path	 Character.	Only commits containing this file path will be returned.
#' @param author Character.	GitHub login or email address by which to filter commit author.
#' @param since	 Character.	Only commits after this date will be returned, `YYYY-MM-DDTHH:MM:SSZ` format.
#' @param until	 Character.	Only commits before this date will be returned, `YYYY-MM-DDTHH:MM:SSZ` format.
#'Character.
#' @examples
#' \dontrun{
#' org_repo_stats("ghclass")
#' }
#'
#' @export
#'

repo_commits = function(repo, branch = "master", sha = branch, path = NULL,
                        author = NULL, since = NULL, until = NULL) {

  arg_is_chr(repo)
  arg_is_chr_scalar(branch, sha)
  arg_is_chr_scalar(path, author, since, until, allow_null = TRUE)

  purrr::map_dfr(
    repo,
    function(repo) {
      res = purrr::safely(github_api_repo_commits)(
        repo, sha, path, author, since, until
      )

      # API gives an error if the repo has 0 commits
      res = allow_error(res, message = "Git Repository is empty")

      status_msg(
        res,
        fail = glue::glue("Failed to retrieve commits from {usethis::ui_value(repo)}.")
      )

      commits = result(res)

      if (empty_result(commits)) {
        tibble::tibble(
          repo  = character(),
          name  = character(),
          email = character(),
          login = character(),
          date  = as.POSIXct(character()),
          msg   = character()
        )
      } else {
        tibble::tibble(
          repo  = repo,
          name  = purrr::map_chr(commits, c("commit", "author","name"), .default = NA),
          email = purrr::map_chr(commits, c("commit", "author","email"), .default = NA),
          login = purrr::map_chr(commits, c("author", "login"), .default = NA),
          date  = lubridate::ymd_hms(
            purrr::map_chr(commits, c("commit", "author", "date"), .default = NA)
          ),
          msg   = purrr::map_chr(commits, c("commit", "message"), .default = NA)
        )
      }
    }
  )
}
