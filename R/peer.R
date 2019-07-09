# The following two functions (peer_anonymize_file and remove_author_rmd) are prob not needed any longer if we decide not to include author as a YAML parameter

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
#' @param file Character. File name of file in `repo1` to be assigned to `repo2`.
#' @param folder Character. Folder name of folder to be created in `repo2`.
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
                       file,
                       folder,
                       message,
                       branch = "master",
                       overwrite = F){

  # Vectorization over file names for now
  stopifnot(length(repo1) == 1)

  # Force files to be .Rmd for now
  stopifnot(all(grepl("\\.[rR]md$", file)))

  file = list(file)

  purrr::pwalk(list(file, folder, repo1, repo2, overwrite, message, branch),
               function(file, folder, repo1, repo2, overwrite, message, branch){

                 # Step 1: grab file(s) from author repository
                 purrr::walk(file,
                             function(file){

                               content = get_file(repo = repo1, file = file, branch = branch)

                               if(is.null(content)){
                                 usethis::ui_oops("Cannot locate file {usethis::ui_value(file)} in repository {usethis::ui_value(repo1)}")
                               } else {

                                 content_anonym = peer_anonymize_file(content)

                                 # Function creates folder per author in the reviewers' repository
                                 file_new = paste(folder,
                                                      file,
                                                      sep = "/")

                                 purrr::walk(repo2,
                                             function(repo2){
                                               if(!file_exists(repo2, file_new, branch, verbose = F) | overwrite == T){

                                                 put_file(repo = repo2,
                                                          path = file_new,
                                                          content = content_anonym,
                                                          message = message,
                                                          branch = branch)
                                               } else {
                                                 usethis::ui_oops("Failed to add file {usethis::ui_value(file_new)} to {usethis::ui_value(repo2)}: already exists.")
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
  arg_is_chr(user)
  stopifnot(length(user) > 1, m > 0, m < length(user))

  set.seed(seed)
  j = sample(2:length(user), m)
  user_random = paste0("author", sample(1:length(user), length(user)))

  res = purrr::map(j, ~ g(.x, length(user)))
  res_df = setNames(data.frame(seq_len(length(user)),
                               user,
                               user_random,
                               do.call(cbind, purrr::map(res, ~ user_random[.x]))),
                    c("id", "user", "user_random", purrr::map_chr(1:m, ~ paste0("reviewer", .x))))

  if(write_csv){
    fname = paste0(paste("roster", paste0("seed", seed), sep = "_"), ".csv")
    readr::write_csv(res_df, fname)
  } else {
    res_df
  }
}

#' Create feedback form
#'
#' @param n Numerical. Number of grade fields to be included in .Rmd YAML.
#' @param title Character. Title of form, defaults to "Feedback form."
#' @param file Character. File name of RMarkdown document, defaults to `feedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_create_feedback(5, "Reviewer feedback for HW1", "feedback_hw1_blank")
#' }
#'
#' @export
#'
peer_create_feedback = function(n,
                                title = "Feedback form",
                                file = "feedback_blank",
                                output = "github_document",
                                write_rmd = TRUE){

  stopifnot(!is.null(file))
  if (grepl("\\s+", file)){
    file = stringr::str_replace_all(file, "\\s", "_")
  }
  if (grepl("\\.Rmd$", file)){
    file = stringr::str_replace_all(file, "\\.Rmd$", "")
  }

  # YAML
  yaml_txt = sprintf("---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n",
                     title,
                     output,
                     paste(purrr::map_chr(1:n, function(x){paste0("  q", x, "_score: NA")}),
                           collapse = "\n"))

  # Body
  resp = "Your response goes here..."
  body_txt = paste("## Instructions",
                   "Enter your feedback for each question below. Please replace `NA`s in the `q*_score` fields in the YAML with the scores you give the author for each question.",
                   "## Feedback",
                   paste0(purrr::map(1:n,
                                      ~ paste0(sprintf("%1$i. Place Question %1$i text here.\n\n", .x),
                                               resp,
                                               collapse = "\n\n")),
                           collapse = "\n\n"),
                   sep = "\n\n"
                   )

  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")


  if(write_rmd){
    cat(doc_txt, file = paste0(file, ".Rmd"))
  } else {
    doc_txt
  }
}

org = "ghclass-test"
prefix = "hw1-"
suffix = ""
file = "feedback_hw1_blank.Rmd"
roster = "/Users/thereseanders/Documents/RStudio/rpkgs/ghclass/roster_seed12345.csv"


#' Extract grades from feedback forms
#'
#' The `peer_collect_grade()` function collects grade information from the YAML of a feedback form within a student's repository. It outputs a new .csv file, with rows specifying individual question grades for each student.
#'
#' @param org Character. Name of the GitHub organization.
#' @param name Character. One or more GitHub user or team name(s).
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param file Character. File name of feedback form (must be .Rmd document).
#' @param roster Character. File path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `reviewer*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#'
#' @export
#'
peer_collect_grade = function(org,
                              prefix = "",
                              suffix = "",
                              file,
                              roster){

  arg_is_chr_scalar(org, prefix, suffix, file, roster)

  # Error messages
  if(!grepl("\\.[rR]md$", file)){
    usethis::ui_stop("Parameter {usethis::ui_field('file')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  # Step 1: Read roster
  file_status = fs::file_exists(roster)
  if (!file_status)
    usethis::ui_stop("Unable to locate the following file: {usethis::ui_value(roster)}")
  if(!grepl("\\.csv$", roster)){
    usethis::ui_stop("Parameter {usethis::ui_field('roster')} must be a {usethis::ui_path('.csv')} file.")
  }
  roster = suppressMessages(readr::read_csv(roster))

  if(!("user_random" %in% names(roster))){
    usethis::ui_stop("{usethis::ui_field('roster')} must contain column {usethis::ui_field('user_random')}")
  }
  if(!(any(grepl("reviewer[0-9]+", names(roster))))){
    usethis::ui_stop("{usethis::ui_field('roster')} must contain at least one column {usethis::ui_field('reviewer*')}")
  }

  m = length(names(roster)[grepl("reviewer[0-9]+", names(roster))])
  author = roster$user
  scores_ls = as.list(author)

  purrr::walk(author,
              function(author){

                # Who are reviewers?
                purrr::walk(1:m,
                            function(x){
                              reviewer_random = as.character(roster[user == author, paste0("reviewer", x)])
                              reviewer = roster$user[which(roster$user_random == reviewer_random)]

                              # Get feedback file
                              repo = paste(org, paste0(prefix, reviewer, suffix), sep = "/")
                              feedback = get_file(repo, file)
                              if(!is.null(feedback)){

                                # Extract scores
                                tempf = tempfile(fileext = ".Rmd")
                                zz = file(tempf, "w")
                                cat(feedback[[1]], file = zz)
                                close(zz)
                                scores = rmarkdown::yaml_front_matter(tempf)$params
                                unlink(tempf)

                                # Change names and collect in
                                rev_ls = list(author = author,
                                              reviewer = reviewer_random,
                                              scores = scores)

                                # Currently does not work as expected
                                scores_ls = append(scores_ls, rev_ls)
                              } else {
                                usethis::ui_oops("Cannot locate file {usethis::ui_value(file)} on repo {usethis::ui_value(repo)}.")
                              }


                            })
              })
}


