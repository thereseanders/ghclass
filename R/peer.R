# The following two functions (peer_anonymize_file and remove_author_rmd) are prob not needed any longer

#' Anonymize file
#'
#' `anonymize_file` removes all identifying student information
#'
#' @param file Character. Local file path.
#'
peer_anonymize_file = function(file){
  remove_author_rmd(file)
}

remove_author_rmd = function(input){
  sub('\\nauthor: \\"[a-zA-Z]+ ([a-zA-Z]+[ \\.]+)?[a-zA-Z]+\"',
      '\\nauthor: \\"Student x"',
      input)
}

#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to reviewers' repositories. The function appends the prefix "author_" to each file. It also removes information from the `author` field in RMarkdown files. To ensure anonymity of the author and subsequent workflow, students should be instructed not to change file names.
#'
#' @param repo1 Character. Address of author repository in "owner/name" format.
#' @param repo2 Character or character vector. Address(es) of author repository or repositories in "owner/name" format.
#' @param filename Character. File name of file in `repo1` to be assigned to `repo2`.
#' @param foldername Character. Folder name of folder to be created in `repo2`.
#' @param message Character. Commit message.
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#'
#' @example
#' \dontrun{
#' peer_toreviewer_file("Sta523-Fa17/hw1-Melissa", "Sta523-Fa17/hw1-Paul", "hw1.Rmd", "Assining HW1 for review")
#' }
#'
#' @export
#'
peer_assign = function(repo1,
                       repo2,
                       filename,
                       foldername,
                       message,
                       branch = "master",
                       overwrite = F){

  # Vectorization over file names for now
  stopifnot(length(repo1) == 1)

  # Force files to be .Rmd for now
  stopifnot(all(grepl("\\.[rR]md$", filename)))

  filename = list(filename)

  purrr::pwalk(list(filename, foldername, repo1, repo2, overwrite, message, branch),
               function(filename, foldername, repo1, repo2, overwrite, message, branch){

                 # Step 1: grab file(s) from author repository
                 purrr::walk(filename,
                             function(filename){

                               content = get_file(repo = repo1, file = filename, branch = branch)

                               if(is.null(content)){
                                 usethis::ui_oops("Cannot locate file {usethis::ui_value(filename)} in repository {usethis::ui_value(repo1)}")
                               } else {

                                 content_anonym = peer_anonymize_file(content)

                                 # Function creates folder per author in the reviewers' repository
                                 filename_new = paste(foldername,
                                                      filename,
                                                      sep = "/")

                                 purrr::walk(repo2,
                                             function(repo2){
                                               if(!file_exists(repo2, filename_new, branch, verbose = F) | overwrite == T){

                                                 put_file(repo = repo2,
                                                          path = filename_new,
                                                          content = content_anonym,
                                                          message = message,
                                                          branch = branch)
                                               } else {
                                                 usethis::ui_oops("Failed to add file {usethis::ui_value(filename_new)} to {usethis::ui_value(repo2)}: already exists.")
                                               }
                                             })
                               }
                             })
               })
}


g = function(j, n) {
  i <- seq_len(n)
  (((i - 1) + (j - 1)) %% n) + 1
}

func_translate = function(vec, id){
  purrr::map2_dbl(vec, id, which(id == ~ .x))
}

#' Create peer review roster
#'
#' `peer_create_roster` creates data frame of random assignments of author files to reviewers. By default, the output is saved to a `.csv` file in the current working directory that incorporates the current date and random seed as part of the file name.
#'
#' @param user Character. A vector of GitHub user names.
#' @param m Numeric. Number of reviews per user. Must be larger than zero and smaller than the number of users.
#' @param seed Numeric. Random seed for assignment, defaults to `12345`.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_create_roster(c("anya", "bruno", "celine", "diego"), 3)
#' }
#'
#' @export
#'
peer_create_roster = function(user,
                              m,
                              seed = 12345,
                              write_csv = T){

  stopifnot(is.numeric(m))
  stopifnot(is.character(user))
  stopifnot(length(user) > 1, m > 0, m < length(user))

  set.seed(seed)
  j = sample(2:length(user), m)

  res = purrr::map(j, ~ g(.x, length(user)))
  res_df = setNames(data.frame(seq_len(length(user)),
                               user,
                               do.call(cbind, purrr::map(res, ~ user[func_translate(.x, seq_len(length(user)))]))),
                    c("id", "user", purrr::map_chr(1:m, ~ paste0("reviewer", .x))))

  if(write_csv){
    fname = paste0(paste("revroster", paste0("seed", seed), sep = "_"), ".csv")
    readr::write_csv(res_df, fname)
  } else {
    res_df
  }
}

#' Create feedback form
#'
#' @param n Numerical. Number of grade fields to be included in .Rmd YAML.
#' @param name Character.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_create_feedback(5, "feedback_blank", "Feedback for HW1")
#' }
#'
peer_create_feedback = function(n,
                                fname = character(),
                                title = character(),
                                output = "github_document",
                                write_rmd = TRUE){

}




