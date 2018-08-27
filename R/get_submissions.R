# key <- Sys.getenv("FORMSTACK_KEY")
# form_ids <- get_forms(key) %>%
#   dplyr::filter(folder == "79048", submissions != "0") %>%
#   dplyr::pull(form_id)




#' @export
get_submissions <- function(key, form_ids = NA_character_,
                            query_params = NA_character_) {
  # query_params <- list(min_time = "2018-08-01")
  if(is.na(form_ids[1])) {
    forms <- get_forms(key)
    form_ids <- forms$form_id
  }

  submission_ids <- lapply(form_ids, get_submission_ids, key = key,
                           query_params = query_params)

  submission_ids <- submission_ids[!sapply(submission_ids, is.null)]

  submission_ids <- dplyr::bind_rows(submission_ids)

  submission_data <- lapply(submission_ids$submission_id, get_submission_data, key = key) %>%
    dplyr::bind_rows()

  submissions <- submission_ids %>%
    dplyr::full_join(submission_data, "submission_id")

  submissions
}

get_submission_ids <- function(form_id, key, query_params) {
  # form_id <- form_ids[3]

  url_address_full <- paste0("https://www.formstack.com/api/v2/form/",
                             form_id, "/submission.json")

  submission_ids_full <- extract_submission_ids(url_address_full,
                                                key, query_params)

  submission_ids <- dplyr::data_frame(id = submission_ids_full,
                                      partial = FALSE)

  url_address_partial <- paste0("https://www.formstack.com/api/v2/form/",
                                form_id, "/partialsubmission.json")

  submission_ids_partial <- extract_submission_ids(url_address_partial,
                                                key, query_params)

}


extract_submission_ids <-  function(url, key, query_params) {

  if(class(query_params) != "list") {
    query_params <- list(per_page = 100)
  }  else if(!"per_page" %in% names(query_params)) {
      query_params$per_page <- 100
  }

  pages <- get_formstack_endpoint(url_address_full, key, query = query_params) %>%
    httr::content()
  pages <- pages$pages

  ids <- lapply(1:pages, function(page, key, query_params) {
    # page = 1
    query_params$page <- page
    ids <- get_formstack_endpoint(url_address_full, key, query = query_params) %>%
      httr::content()
    lapply(ids$submissions, function(submission) submission$id) %>%
      unlist()
  }, key = key, query_params = query_params)

  unlist(ids)
}


get_submission_data <- function(submission_id, key, partial = FALSE) {
  # submission_id <- submission_ids$submission_id[1]

  if(partial) {
    partial <- "partial"
  } else {
    partial <- ""
  }

  url_address <- paste0("https://www.formstack.com/api/v2/",
                        partial, "submission/", submission_id, ".json")

  submission_data <- get_formstack_endpoint(url_address, key)

  submission_data <- httr::content(submission_data)$data

  submission_data <- lapply(submission_data, collapse_to_data_frame) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(submission_id = submission_id) %>%
    dplyr::rename(field_id = field)

  submission_data
}
