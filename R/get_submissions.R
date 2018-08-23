# key <- Sys.getenv("FORMSTACK_KEY")
# form_ids <- get_forms(key) %>%
#   dplyr::filter(folder == "79048", submissions != "0") %>%
#   dplyr::pull(form_id)




#' @export
get_submissions <- function(key, form_ids = NA, query_params = NA) {
  # query_params <- list(min_time = "2018-08-01")
  if(is.na(form_ids[1])) {
    forms <- get_forms(key)
    form_ids <- forms$form_id
  }

  submission_ids <- lapply(form_ids, get_submission_ids, key = key,
                           query_params)

  submission_ids <- submission_ids[!sapply(submission_ids, is.null)]

  submission_ids <- dplyr::bind_rows(submission_ids)

  submission_data <- lapply(submission_ids$submission_id, get_submission_data, key = key) %>%
    dplyr::bind_rows()

  submissions <- submission_ids %>%
    dplyr::full_join(submission_data, "submission_id")

  submissions
}

get_submission_ids <- function(form_id, key, query_params) {
  # form_id <- form_ids[5]
  print(form_id)
  url_address <- paste0("https://www.formstack.com/api/v2/form/",
                        form_id, "/submission.json")

  if(is.na(query_params)) {
    submissions <- httr::GET(url_address,
                             httr::add_headers(.headers = c("Accept" = "application/json",
                                                            "Content-Type" = "application/json",
                                                            "Authorization" = paste("Bearer", key))))
  } else {
    submissions <- httr::GET(url_address,
                             httr::add_headers(.headers = c("Accept" = "application/json",
                                                            "Content-Type" = "application/json",
                                                            "Authorization" = paste("Bearer", key))),
                             query = query_params)
  }

  submissions <- httr::content(submissions)

  submissions <- lapply(submissions$submissions, collapse_to_data_frame)

  submissions <- dplyr::bind_rows(submissions)

  if(nrow(submissions) == 0) {
    return(NULL)
  }

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
