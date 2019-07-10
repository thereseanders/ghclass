# The following two functions (peer_anonymize_file and remove_author_rmd) are prob not needed any longer if we decide not to include author as a YAML parameter

#' Anonymize file
#'
#' `anonymize_file` removes all identifying student information
#'
#' @param file Character. Local file path.
#'
peer_anonymize_file = function(file) {
  remove_author_rmd(file)
}

remove_author_rmd = function(input) {
  sub(
    '\\nauthor: \\"[aA-zZ]+ ([aA-zZ]+[ \\.]+)?[aA-zZ]+\"',
    '\\nauthor: \\"Student x"',
    input
  )
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
                       overwrite = F) {
  # Vectorization over file names for now
  stopifnot(length(repo1) == 1)

  # Force files to be .Rmd for now
  stopifnot(all(grepl("\\.[rR]md$", file)))

  file = list(file)

  purrr::pwalk(list(file, folder, repo1, repo2, overwrite, message, branch),
               function(file,
                        folder,
                        repo1,
                        repo2,
                        overwrite,
                        message,
                        branch) {
                 # Step 1: grab file(s) from author repository
                 purrr::walk(file,
                             function(file) {
                               content = get_file(repo = repo1,
                                                  file = file,
                                                  branch = branch)

                               if (is.null(content)) {
                                 usethis::ui_oops(
                                   "Cannot locate file {usethis::ui_value(file)} in repository {usethis::ui_value(repo1)}"
                                 )
                               } else {
                                 content_anonym = peer_anonymize_file(content)

                                 # Function creates folder per author in the reviewers' repository
                                 file_new = paste(folder,
                                                  file,
                                                  sep = "/")

                                 purrr::walk(repo2,
                                             function(repo2) {
                                               if (!file_exists(repo2, file_new, branch, verbose = F) |
                                                   overwrite == T) {
                                                 put_file(
                                                   repo = repo2,
                                                   path = file_new,
                                                   content = content_anonym,
                                                   message = message,
                                                   branch = branch
                                                 )
                                               } else {
                                                 usethis::ui_oops(
                                                   "Failed to add file {usethis::ui_value(file_new)} to {usethis::ui_value(repo2)}: already exists."
                                                 )
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
#' `peer_roster` creates data frame of random assignments of author files to reviewers. By default, the output is saved to a `.csv` file in the current working directory that incorporates the current date and random seed as part of the file name.
#'
#' @param user Character. A vector of GitHub user names.
#' @param m Numeric. Number of reviews per user. Must be larger than zero and smaller than the number of users.
#' @param seed Numeric. Random seed for assignment, defaults to `12345`.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_roster(c("anya", "bruno", "celine", "diego"), 3)
#' }
#'
#' @export
#'
peer_roster = function(m,
                       user,
                       seed = 12345,
                       write_csv = TRUE) {
  stopifnot(is.numeric(m))
  stopifnot(is.numeric(seed))
  arg_is_chr(user)
  stopifnot(length(user) > 1, m > 0, m < length(user))

  set.seed(seed)
  j = sample(2:length(user), m)
  res = purrr::map(j, ~ g(.x, length(user)))

  # Randomizing user names to avoid clustering
  user_random = paste0("a", sample(1:length(user), length(user)))
  df_sort = data.frame(user = user,
                       user_random = as.character(user_random))[order(as.numeric(sub("[aA-zZ]+", "", user_random))), ]

  res_df = setNames(data.frame(df_sort,
                               do.call(
                                 cbind, purrr::map(res, ~ as.character(df_sort$user_random)[.x])
                               )),
                    c("user", "user_random", purrr::map_chr(1:m, ~ paste0("r", .x))))

  if (write_csv) {
    fname = glue::glue("roster_seed{seed}.csv")
    readr::write_csv(res_df, fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    res_df
  }
}

#' Create reviewer feedback form
#'
#' `peer_create_rform` creates blank feedback forms for reviewers based on the user-specified number of questions.
#'
#' @param n Numerical. Number of score fields to be included in .Rmd YAML.
#' @param title Character. Title of form, defaults to "Reviewer feedback form."
#' @param file Character. File name of RMarkdown document, defaults to `rfeedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_create_rform(5, "Reviewer feedback for HW1", "rfeedback_hw1_blank")
#' }
#'
#' @export
#'
peer_create_rform = function(n,
                             title = "Reviewer feedback form",
                             file = "rfeedback_blank",
                             output = "github_document",
                             write_rmd = TRUE) {
  stopifnot(!is.null(file))
  if (grepl("\\s+", file)) {
    file = stringr::str_replace_all(file, "\\s", "_")
  }
  if (grepl("\\.Rmd$", file)) {
    file = stringr::str_replace_all(file, "\\.Rmd$", "")
  }

  # YAML
  yaml_txt = sprintf(
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n",
    title,
    output,
    paste(purrr::map_chr(1:n, function(x) {
      paste0("  q", x, "_score: NA")
    }),
    collapse = "\n")
  )

  # Body
  resp = "Your response goes here..."
  body_txt = paste(
    "## Instructions",
    "Enter your feedback for each question below. Please replace `NA`s in the `q*_score` fields in the YAML with the scores you give the author for each question.",
    "## Feedback",
    paste0(purrr::map(
      1:n,
      ~ paste0(
        sprintf("%1$i. Place Question %1$i text here.\n\n", .x),
        resp,
        collapse = "\n\n"
      )
    ),
    collapse = "\n\n"),
    sep = "\n\n"
  )

  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")


  if (write_rmd) {
    cat(doc_txt, file = paste0(file, ".Rmd"))
  } else {
    doc_txt
  }
}


#' Create author feedback form
#'
#' `peer_create_aform` creates a short feedback form for authors to rate the feedback they got from reviewers.
#'
#' @param category Character. Categories to be included in the feedback form, defaults to `c("helpfulness", "accuracy", "fairness")`.
#' @param title Character. Title of form, defaults to "Author feedback form."
#' @param file Character. File name of RMarkdown document, defaults to `rfeedback_blank`.
#' @param output Character. Output parameter for `.Rmd` file, defaults to `github_document`.
#' @param write_rmd Logical. Whether the feedback form should be saved to a `.Rmd` file in the current working directory, defaults to TRUE.
#'
#' @example
#' \dontrun{
#' peer_create_aform(c("accuracy", "fairness"))
#' }
#'
#' @export
#'
peer_create_aform = function(category = c("helpfulness", "accuracy", "fairness"),
                             title = "Author feedback form",
                             file = "afeedback_blank",
                             output = "github_document",
                             write_rmd = TRUE) {
  stopifnot(!is.null(file))
  if (grepl("\\s+", file)) {
    file = stringr::str_replace_all(file, "\\s", "_")
  }
  if (grepl("\\.Rmd$", file)) {
    file = stringr::str_replace_all(file, "\\.Rmd$", "")
  }

  # YAML
  yaml_txt = sprintf(
    "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n",
    title,
    output,
    paste(paste0("  ", category, ": NA"), collapse = "\n")
  )

  # Instructions
  instruct_txt = sprintf(
    "Please rate the reviewer's feedback based on the categories below on a scale from \"Strongly disagree\" to \"Strongly agree.\" Please replace `NA`s in the %s %s in the YAML with the scores you give the reviewer for each category.",
    if (length(category) == 1) {
      paste0("`", category, "`")
    } else if (length(category) == 2) {
      paste0(paste0("`", category[1], "`"),
             " and ",
             paste0("`", category[2], "`"))
    } else {
      paste0(paste0(purrr::map_chr(category[1:(length(category) - 1)], ~ paste0("`", .x, "`"))
                    , collapse = ", "),
             ", and ",
             paste0("`", category[length(category)], "`"))
    },
    if (length(category) == 1) {
      "field"
    } else {
      "fields"
    }
  )

  category_txt = list(helpfulness = "`helpfulness`: \"The reviewer's feedback was constructive and helpful.\"",
                      accuracy = "`accuracy`: \"The reviewer's assessment accurately describes the quality of my work.\"",
                      fairness = "`fairness`: \"The reviewer's assessment was fair.\"")

  tab_txt = paste(
    "| Score | Rating            |",
    "|-------|-------------------|",
    "| 1     | Strongly disagree |",
    "| 2     | Disagree          |",
    "| 3     | Agree             |",
    "| 4     | Strongly agree    |",
    sep = "\n"
  )

  # Putting it all together
  body_txt = paste("## Instructions",
                   instruct_txt,
                   paste(paste0(
                     1:length(category),
                     ". ",
                     purrr::map_chr(category, ~ paste(category_txt[which(names(category_txt) == .x)]))
                   ),
                   collapse = "\n\n"),
                   tab_txt,
                   sep = "\n\n")


  # Ensure an empty line at the end of the file
  doc_txt = paste0(yaml_txt, body_txt, "\n")

  if (write_rmd) {
    cat(doc_txt, file = paste0(file, ".Rmd"))
  } else {
    doc_txt
  }
}


#' Collect scores from reviewer feedback forms
#'
#' The `peer_collect_rscore()` function collects score information from the YAML of a feedback form within a student's repository. It outputs a new .csv file, with rows specifying individual question scores for each student. Note that in its current version, the function collects scores given by reviewers only. Future versions will include the capability to collect ratings
#'
#' @param org Character. Name of the GitHub organization.
#' @param name Character. One or more GitHub user or team name(s).
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param file Character. File name of feedback form (must be .Rmd document).
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#' @param write_csv Logical. Whether the roster data frame should be saved to a `.csv` file in the current working directory, defaults to TRUE.
#'
#' @export
#'
peer_collect_rscore = function(org,
                               prefix = "",
                               suffix = "",
                               file,
                               roster,
                               write_csv = TRUE) {
  arg_is_chr_scalar(org, prefix, suffix, file, roster)

  # Check that feedback form is .Rmd
  if (!grepl("\\.[rR]md$", file)) {
    usethis::ui_stop("{usethis::ui_field('file')} must be a {usethis::ui_path('.Rmd')} file.")
  }

  # Read and check roster file (could be data frame, tibble, or .csv)
  # Create file names for saving
  res = purrr::safely(fs::file_exists)(roster)
  if (is.null(res$result) & is.data.frame(roster)) {
    rdf = roster
    fname = glue::glue("{prefix}rscores{suffix}.csv")
  } else if (is.null(res$result) & !is.data.frame(roster)) {
    usethis::ui_stop("{usethis::ui_field('roster')} must be a data.frame or .csv file.")
  } else if (!res$result) {
    usethis::ui_stop("Cannot locate file: {usethis::ui_value(roster)}")
  } else if (res$result) {
    rdf = suppressMessages(readr::read_csv(roster))
    path = fs::path_file(roster)
    if (grepl("roster", path)) {
      fname = sub("roster", "rscores", path)
    } else {
      glue::glue("rscores_{path}")
    }
  }

  if (!("user_random" %in% names(rdf))) {
    usethis::ui_stop(
      "{usethis::ui_field('roster')} must contain column {usethis::ui_field('user_random')}"
    )
  }
  if (!(any(grepl("^r[0-9]+$", names(rdf))))) {
    usethis::ui_stop(
      "{usethis::ui_field('roster')} must contain at least one column {usethis::ui_field('r*')}"
    )
  }

  # Extract scores
  m = seq_len(length(names(rdf)[grepl("^r[0-9]+$", names(rdf))]))
  author = rdf$user

  out = purrr::map_dfr(author,
                       function(author) {
                         purrr::map_dfr(m,
                                        function(m) {
                                          reviewer_random = as.character(rdf[rdf$user == author, paste0("r", m)])
                                          reviewer = rdf$user[which(rdf$user_random == reviewer_random)]

                                          # Get feedback file
                                          repo = paste(org, paste0(prefix, reviewer, suffix), sep = "/")
                                          feedback = purrr::safely(get_file)(repo, file)

                                          if (succeeded(feedback)) {
                                            # Extract scores
                                            tempf = tempfile(fileext = ".Rmd")
                                            zz = file(tempf, "w")
                                            cat(feedback[[1]], file = zz)
                                            close(zz)
                                            scores = rmarkdown::yaml_front_matter(tempf)$params
                                            unlink(tempf)

                                            # Change names and save them

                                            setNames(c(author, paste0("r", m), scores),
                                                     c("user", "r_no", paste0("q", 1:length(scores))))
                                          } else {
                                            usethis::ui_oops(
                                              "Cannot locate file {usethis::ui_value(file)} on repo {usethis::ui_value(repo)}."
                                            )
                                          }
                                        })
                       }) %>%

    # Getting data frame in right format
    tidyr::gather(q_name, q_value,-user,-r_no) %>%
    tidyr::unite("q", c("r_no", "q_name")) %>%
    tidyr::spread(q, q_value) %>%
    merge(rdf, all.y = T)

  if (write_csv) {
    readr::write_csv(out[, union(names(rdf), names(out))], fname)
    usethis::ui_done("Saved file {usethis::ui_value(fname)} to working directory.")
  } else {
    out[, union(names(rdf), names(out))]
  }
}

#' #' Collect scores from reviewer feedback forms
#' #'
#' #' @param roster Character. File path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`. Can be either the roster including the reviewer scores (recommended) or original roster file.
#' #'
#' #'
#' #' @export
#' peer_collect_ascore = function(org,
#'                                prefix = "",
#'                                suffix = "",
#'                                file,
#'                                roster,
#'                                write_csv = TRUE) {
#'   # Check that feedback form is .Rmd
#'   if (!grepl("\\.[rR]md$", file)) {
#'     usethis::ui_stop("{usethis::ui_field('file')} must be a {usethis::ui_path('.Rmd')} file.")
#'   }
#'
#'   # Read and check roster file
#'   file_status = fs::file_exists(roster)
#'   if (!file_status)
#'     usethis::ui_stop("Unable to locate the following file: {usethis::ui_value(roster)}")
#'   if (!grepl("\\.csv$", roster)) {
#'     usethis::ui_stop("{usethis::ui_field('roster')} must be a {usethis::ui_path('.csv')} file.")
#'   }
#'   rdf = suppressMessages(readr::read_csv(roster))
#'
#'   if (!("user_random" %in% names(rdf))) {
#'     usethis::ui_stop(
#'       "{usethis::ui_field('roster')} must contain column {usethis::ui_field('user_random')}"
#'     )
#'   }
#'   if (!(any(grepl("^r[0-9]+$", names(rdf))))) {
#'     usethis::ui_stop(
#'       "{usethis::ui_field('roster')} must contain at least one column {usethis::ui_field('r*')}"
#'     )
#'   }
#'
#'   # Extract scores
#'   m = seq_len(length(names(rdf)[grepl("^r[0-9]+$", names(rdf))]))
#'   author = rdf$user
#'
#'   out = purrr::map_dfr(author,
#'                        function(author) {
#'                          purrr::map_dfr(m,
#'                                         function(m) {
#'                                           reviewer_random = as.character(rdf[rdf$user == author, paste0("r", m)])
#'                                           reviewer = rdf$user[which(rdf$user_random == reviewer_random)]
#'
#'                                           # Get feedback file
#'                                           repo = paste(org, paste0(prefix, reviewer, suffix), sep = "/")
#'                                           feedback = purrr::safely(get_file)(repo, file)
#'
#'                                           if (succeeded(feedback)) {
#'                                             # Extract scores
#'                                             tempf = tempfile(fileext = ".Rmd")
#'                                             zz = file(tempf, "w")
#'                                             cat(feedback[[1]], file = zz)
#'                                             close(zz)
#'                                             scores = rmarkdown::yaml_front_matter(tempf)$params
#'                                             unlink(tempf)
#'
#'                                             # Change names and save them
#'
#'                                             setNames(c(author, paste0("r", m), scores),
#'                                                      c("user", "r_no", paste0("q", 1:length(scores))))
#'                                           } else {
#'                                             usethis::ui_oops(
#'                                               "Cannot locate file {usethis::ui_value(file)} on repo {usethis::ui_value(repo)}."
#'                                             )
#'                                           }
#'                                         })
#'                        })
#'
#' }
#'
