#' @export
get_submissions <- function(key) {
  # key <- Sys.getenv("FORMSTACK_KEY_TEST")
  forms <- get_forms(key)

  submission_ids <- lapply(forms$form_id, get_submission_ids, key = key) %>%
    dplyr::bind_rows()

  submission_data <- lapply(submission_ids$submission_id, get_submission_data, key = key) %>%
    dplyr::bind_rows()

  submissions <- submission_ids %>%
    dplyr::full_join(submission_data, "submission_id")
}

get_submission_ids <- function(form_id, key) {
  # form_id <- forms$form_id[1]
  url_address <- paste0("https://www.formstack.com/api/v2/form/",
                        form_id, "/submission.json")

  submissions <- httr::GET(url_address,
                           httr::add_headers(.headers = c("Accept" = "application/json",
                                                          "Content-Type" = "application/json",
                                                          "Authorization" = paste("Bearer", key))))
  submissions <- httr::content(submissions)

  submissions <- lapply(submissions$submissions, collapse_to_data_frame)

  submissions <- dplyr::bind_rows(submissions)

  columns <- colnames(submissions)

  submissions <- submissions %>%
    dplyr::mutate(form_id = form_id) %>%
    dplyr::select(!!c("form_id", columns)) %>%
    dplyr::rename(submission_id = id)

  submissions
}

get_submission_data <- function(submission_id, key) {
  # submission_id <- submission_ids$submission_id[1]

  url_address <- paste0("https://www.formstack.com/api/v2/submission/",
                        submission_id, ".json")

  submission_data <- httr::GET(url_address,
                               httr::add_headers(.headers = c("Accept" = "application/json",
                                                              "Content-Type" = "application/json",
                                                              "Authorization" = paste("Bearer", key))))

  submission_data <- httr::content(submission_data)$data

  submission_data <- lapply(submission_data, collapse_to_data_frame) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(submission_id = submission_id) %>%
    dplyr::rename(field_id = field)

  submission_data
}
