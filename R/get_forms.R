#' @export
#' @import httr dplyr
get_forms <- function(key) {

  url_address <- "https://www.formstack.com/api/v2/form.json"

  forms <- get_formstack_endpoint(url_address, key)

  forms <- httr::content(forms)

  forms <- lapply(forms$forms, collapse_to_data_frame)

  forms <- dplyr::bind_rows(forms) %>%
    dplyr::rename(form_id = id)

  forms
}

get_formstack_endpoint <- function(url, key, ...) {
  httr::GET(url,
            httr::add_headers(.headers = c("Accept" = "application/json",
                                           "Content-Type" = "application/json",
                                           "Authorization" = paste("Bearer", key))),
            ...)
}


collapse_to_data_frame <- function(list_content) {
  list_content <- lapply(list_content, function(item) {
    if(class(item) == "list") {
      item <- names(item)[1]
    }
    if(is.null(item)){
      item <- NA
    }
    item
  })

  df <- do.call(cbind.data.frame, list_content)

  for(column in names(df)) {
    df[[column]] <- gsub("\\n", "|", as.character(df[[column]]))
    df[[column]][!grepl("\\S", df[[column]])] <- NA
  }

  df
}

