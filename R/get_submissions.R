

#' @export
get_submissions <- function(key, form_ids = NA_character_,
                            query_params = NA_character_) {
  # query_params <- list(min_time = "2018-08-25")
  if(is.na(form_ids[1])) {
    forms <- get_forms(key)
    form_ids <- forms$form_id
  }

  submission_ids <- lapply(form_ids, get_submission_ids, key = key,
                           query_params = query_params)

  submission_ids <- dplyr::bind_rows(submission_ids)

  submissions <- apply(submission_ids %>%
                         filter(type == ""), 1, function(row, key) {
    print(row["id"])
    print(row["type"])
    get_submission_data(row["id"], row["type"], key) %>%
      dplyr::mutate(form_id = row["form_id"])
  }, key = key)

  submissions <- submissions %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::select(form_id, field_id, submission_id, value)

  submissions
}

get_submission_ids <- function(form_id, key, query_params) {
  # form_id <- form_ids[3]

  url_address_full <- paste0("https://www.formstack.com/api/v2/form/",
                             form_id, "/submission.json")

  submission_ids_full <- extract_submission_ids(url_address_full,
                                                key, query_params)

  submission_ids <- dplyr::data_frame(id = submission_ids_full,
                                      type = "")

  url_address_partial <- paste0("https://www.formstack.com/api/v2/form/",
                                form_id, "/partialsubmission.json")

  submission_ids_partial <- extract_submission_ids(url_address_partial,
                                                   key, query_params)

  submission_ids <- submission_ids %>%
    dplyr::bind_rows(submission_ids,
                     dplyr::data_frame(id = submission_ids_partial,
                                       type = "partial")) %>%
    dplyr::mutate(form_id = form_id)

  submission_ids

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


get_submission_data <- function(submission_id, type, key) {
  # submission_id <- "432899847"; type <- "partial"

  url_address <- paste0("https://www.formstack.com/api/v2/",
                        type, "submission/", submission_id, ".json")

  submission_data <- get_formstack_endpoint(url_address, key)

  submission_data <- httr::content(submission_data)$data

  if(is.null(submission_data)) {
    return(dplyr::data_frame(field_id = character(length = 0),
                             value = character(length = 0),
                             submission_id = character(length = 0)))
  }

  submission_data <- lapply(submission_data, collapse_to_data_frame) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(submission_id = submission_id) %>%
    dplyr::rename(field_id = field)

  submission_data
}
