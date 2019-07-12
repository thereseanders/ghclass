## !!! Remember to run usethis::use_pipe() before PR !!!

# Helper function for Latin square
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
                       user_random = as.character(user_random))[order(as.numeric(sub("[aA-zZ]+", "", user_random))),]

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
    tibble::as_tibble(purrr::modify_if(res_df, is.factor, as.character))
  }
}


# The following two functions (peer_anonymize_file and remove_author_rmd) are prob not needed any longer if we decide not to include author as a YAML parameter

# If we keep this function, it should just strip the author field from YAML
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

# Reads roster file
peer_read_roster = function(roster) {
  res = purrr::safely(fs::file_exists)(roster)

  if (is.null(res$result) & is.data.frame(roster)) {
    return(tibble::as_tibble(purrr::modify_if(roster, is.factor, as.character)))
  } else if (is.null(res$result) & !is.data.frame(roster)) {
    usethis::ui_stop("{usethis::ui_field('roster')} must be a data.frame or .csv file.")
  } else if (!res$result) {
    usethis::ui_stop("Cannot locate file: {usethis::ui_value(roster)}")
  } else if (res$result) {
    suppressMessages(readr::read_csv(roster))
  }

}

# Checks whether necessary column names are present
peer_check_roster = function(roster) {
  val = c("user", "user_random")
  purrr::walk(val,
              function(val) {
                if (!(val %in% names(roster))) {
                  usethis::ui_oops("{usethis::ui_field('roster')} must contain column {usethis::ui_value(val)}")
                }
              })

  if (!(any(grepl("^r[0-9]+$", names(roster))))) {
    usethis::ui_oops(
      "{usethis::ui_field('roster')} must contain at least one column {usethis::ui_field('r*')}"
    )
  }
}



peer_get_reviewer = function(author, roster, anonym = FALSE) {

  m = seq_len(length(names(roster)[grepl("^r[0-9]+$", names(roster))]))
  reviewer_random = as.character(roster[roster$user == author, paste0("r", m)])
  reviewer = roster$user[purrr::map_int(reviewer_random, ~ which(roster$user_random == .x))]

  if (!anonym) {
    reviewer
  } else {
    reviewer_random
  }
}

#' Assign file to reviewers
#'
#' `peer_assign` adds files from authors' repositories to reviewers' repositories.
#'
#' @param org Character. Name of GitHub Organization.
#' @param roster Character. Data frame or file path of roster file with author-reviewer assignments. Must contain a column `user` with GitHub user names of authors, a column `user_random` with randomized tokens for user names, and one or more `r*` columns that specify review assignments as values of the vector `user_random`. See `peer_create_feedback`.
#' @param file Character. File name or vector of file names to be included.
#' @param prefix Character. Common repository name prefix.
#' @param suffix Character. Common repository name suffix.
#' @param message Character. Commit message, defaults to "Assigning review."
#' @param branch Character. Name of branch the file should be committed to, defaults to `master`.
#' @param overwrite Logical. Whether existing files in reviewers' repositories should be overwritten, defaults to `FALSE`.
#'
#' @example
#' \dontrun{
#' peer_assign(
#' org = "ghclass-test",
#' roster = "hw2_roster_seed12345.csv",
#' file = c("task.Rmd", "iris_data.csv"),
#' prefix = "hw2-"
#' )
#' }
#'
#' @export
#'
peer_assign = function(org,
                       roster,
                       file,
                       prefix = "",
                       suffix = "",
                       message = "Assigning review",
                       branch = "master",
                       overwrite = FALSE) {
  arg_is_chr(org, file, prefix, suffix, branch)
  arg_is_chr(message, allow_null = TRUE)
  arg_is_lgl_scalar(overwrite)

  rdf = peer_read_roster(roster)
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  purrr::walk2(author, author_random,
               function(author, author_random) {
                 repo1 = glue::glue("{org}/{prefix}{author}{suffix}")

                 # First, get file content(s)
                 content = purrr::map(file,
                                      function(file) {
                                        res = purrr::safely(get_file)(repo = repo1,
                                                                      file = file,
                                                                      branch = branch)
                                        if (succeeded(res)) {
                                          return(res$result)
                                        } else {
                                          usethis::ui_oops(
                                            "Cannot locate {usethis::ui_value(file)} in repository {usethis::ui_value(repo1)}"
                                          )
                                          return()
                                        }
                                      })

                 # Grab reviewers
                 reviewer = peer_get_reviewer(author, rdf, anonym = FALSE)

                 # Create folder paths (from perspective of reviewers)
                 path = as.list(glue::glue("{author_random}/{file}"))

                 purrr::walk(reviewer,
                             function(reviewer) {
                               repo2 = glue::glue("{org}/{prefix}{reviewer}{suffix}")
                               purrr::walk2(path, content,
                                            function(path, content) {
                                              if (!file_exists(repo2, path, verbose = FALSE) |
                                                  overwrite == TRUE) {
                                                if (!is.null(content)) {
                                                  put_file(
                                                    repo = repo2,
                                                    path = path,
                                                    content = content,
                                                    message = message,
                                                    branch = branch,
                                                    verbose = TRUE
                                                  )
                                                }
                                              } else {
                                                usethis::ui_oops(
                                                  "Failed to add {usethis::ui_value(path)} to {usethis::ui_value(repo2)}: already exists."
                                                )
                                              }
                                            })
                             })
               })
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
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
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
                             write_rmd = TRUE,
                             overwrite = FALSE) {
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
    fname = paste0(file, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, file = fname)
      usethis::ui_done("Saved file {usethis::ui_value(fname)}")
    } else {
      usethis::ui_oops(
        paste(
          'File {usethis::ui_value(fname)} already exists.',
          'If you want to force save this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
        )
      )
    }
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
#' @param overwrite Logical. Should existing file or files with same name be overwritten, defaults to FALSE.
#' @param author Logical. Should the YAML include an `author` field, defaults to TRUE.
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
                             write_rmd = TRUE,
                             overwrite = FALSE,
                             author = TRUE) {
  stopifnot(!is.null(file))
  if (grepl("\\s+", file)) {
    file = stringr::str_replace_all(file, "\\s", "_")
  }
  if (grepl("\\.Rmd$", file)) {
    file = stringr::str_replace_all(file, "\\.Rmd$", "")
  }

  # YAML
    yaml_txt = sprintf(
      if(author){
        "---\ntitle: \"%s\"\nauthor: \noutput: %s\nparams:\n%s\n---\n\n\n"
      } else {
        "---\ntitle: \"%s\"\noutput: %s\nparams:\n%s\n---\n\n\n"
      },
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
    fname = paste0(file, ".Rmd")
    if (!(fs::file_exists(fname)) | overwrite) {
      cat(doc_txt, file = fname)
      usethis::ui_done("Saved file {usethis::ui_value(fname)}")
    } else {
      usethis::ui_oops(
        paste(
          'File {usethis::ui_value(fname)} already exists.',
          'If you want to force save this file, re-run the command with {usethis::ui_code("overwrite = TRUE")}.'
        )
      )
    }
  } else {
    doc_txt
  }
}

#' Add files to repositories based on roster
#'
#' `peer_add_file()` is the peer review version of `repo_add_file()`. It takes a local file and adds it to author- or reviewer-specific folders in students' repositories based on the peer review roster.
#'
peer_add_file = function(org,
                         roster,
                         file,
                         to = c("r", "a"),
                         prefix = "",
                         suffix = "",
                         message = NULL,
                         branch = "master",
                         overwrite = FALSE) {
  arg_is_chr_scalar(to)
  stopifnot(to %in% c("r", "a"))

  rdf = peer_read_roster(roster)
  peer_check_roster(rdf)

  author = as.list(as.character(rdf$user))
  author_random = as.list(as.character(rdf$user_random))

  purrr::walk2(author, author_random,
               function(author, author_random) {

                 # Grab reviewers
                 reviewer = peer_get_reviewer(author, rdf, anonym = FALSE)

                 purrr::walk2(reviewer, reviewer_random,
                              function(reviewer, reviewer_random) {

                                if (to == "r") {
                                  repo = glue::glue("{org}/{prefix}{reviewer}{suffix}")
                                  folder = author_random
                                } else {
                                  repo = glue::glue("{org}/{prefix}{author}{suffix}")
                                  folder = reviewer_random
                                }

                                repo_add_file(
                                  repo = repo,
                                  file = file,
                                  folder = folder,
                                  preserve_path = FALSE
                                )
                              })
               })
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
    tidyr::gather(q_name, q_value, -user, -r_no) %>%
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
#'                                           reviewer = rdf$user[purrr::map_int(reviewer_random, ~which(rdf$user_random == .x))]
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
