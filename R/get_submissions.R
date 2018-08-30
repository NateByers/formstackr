# key <- Sys.getenv("FORMSTACK_KEY")
#
# forms <-  get_forms(key) %>%
#   dplyr::filter(folder == "79048", submissions != "0") %>%
#   dplyr::mutate(form_id = as.integer(form_id))
#
# form_ids <- forms %>%
#   dplyr::pull(form_id)
#
# fields <- formstackr::get_fields(key, form_ids) %>%
#   dplyr::mutate(form_id = as.integer(form_id),
#                 field_id = as.integer(field_id))

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

  submission_ids <- dplyr::bind_rows(submission_ids)

  if(nrow(submission_ids) == 0) {
    return(dplyr::data_frame(form_id = character(length = 0),
                             field_id = character(length = 0),
                             submission_id = character(length = 0),
                             timestamp = character(length = 0),
                             value = character(length = 0)))
  }

  submissions <- apply(submission_ids, 1, function(row, key) {
    get_submission_data(row["submission_id"], row["type"], key) %>%
      dplyr::mutate(form_id = row["form_id"],
                    timestamp = row["timestamp"])
  }, key = key)

  submissions <- submissions %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::select(form_id, field_id, submission_id, timestamp, value)

  submissions
}

get_submission_ids <- function(form_id, key, query_params) {
  # form_id <- form_ids[3]
  url_address_full <- paste0("https://www.formstack.com/api/v2/form/",
                             form_id, "/submission.json")

  submission_ids_full <- extract_submission_ids(url_address_full,
                                                key, query_params) %>%
    dplyr::mutate(type = "")

  url_address_partial <- paste0("https://www.formstack.com/api/v2/form/",
                                form_id, "/partialsubmission.json")

  submission_ids_partial <- extract_submission_ids(url_address_partial,
                                                   key, query_params) %>%
    dplyr::mutate(type = "partial")

  submission_ids <- submission_ids_full %>%
    dplyr::bind_rows(submission_ids_partial)  %>%
    dplyr::mutate(form_id = form_id) %>%
    dplyr::filter(!is.na(submission_id))

  submission_ids

}


extract_submission_ids <-  function(url, key, query_params) {
# url <- paste0("https://www.formstack.com/api/v2/form/", form_id, "/submission.json")
  if(class(query_params) != "list") {
    query_params <- list(per_page = 100)
  }  else if(!"per_page" %in% names(query_params)) {
      query_params$per_page <- 100
  }

  pages <- get_formstack_endpoint(url, key, query = query_params) %>%
    httr::content()
  pages <- pages$pages

  if(pages == 0) {
    dplyr::data_frame(id = NA, timestamp = NA)
  }

  ids <- lapply(1:pages, function(page, key, query_params) {
    # page = 1
    query_params$page <- page
    ids <- get_formstack_endpoint(url, key, query = query_params) %>%
      httr::content()
    lapply(ids$submissions, function(submission) {
      dplyr::data_frame(submission_id = submission$id,
                        timestamp = submission$timestamp)
      }) %>%
      dplyr::bind_rows()
  }, key = key, query_params = query_params) %>%
    dplyr::bind_rows()

  ids
}


get_submission_data <- function(submission_id, type, key) {
  # submission_id <- "429051246"; type <- ""

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
