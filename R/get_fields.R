#' @export
get_fields <- function(key) {
  # key <- Sys.getenv("FORMSTACK_KEY_TEST")
  forms <- get_forms(key)

  fields <- lapply(forms$form_id, get_form_fields, key = key) %>%
    dplyr::bind_rows()

  fields
}

get_form_fields <- function(form_id, key) {
  # form_id <- forms$form_id[1]
  url_address <- paste0("https://www.formstack.com/api/v2/form/",
                        form_id, "/field.json")


  fields <- httr::GET(url_address,
                      httr::add_headers(.headers = c("Accept" = "application/json",
                                                     "Content-Type" = "application/json",
                                                     "Authorization" = paste("Bearer", key))))

  fields <- httr::content(fields)

  fields <- lapply(fields, collapse_to_data_frame)

  fields <- dplyr::bind_rows(fields)

  columns <- colnames(fields)

  fields <- fields %>%
    dplyr::mutate(form_id = form_id) %>%
    dplyr::select(!!c("form_id", columns)) %>%
    dplyr::rename(field_id = id)

  fields
}
