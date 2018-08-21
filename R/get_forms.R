#' @export
#' @import httr dplyr
get_forms <- function(key) {
  # key <- Sys.getenv("FORMSTACK_KEY_TEST")

  url_address <- "https://www.formstack.com/api/v2/form.json"

  forms <- httr::GET(url_address,
                     httr::add_headers(.headers = c("Accept" = "application/json",
                                                    "Content-Type" = "application/json",
                                                    "Authorization" = paste("Bearer", key))))

  forms <- httr::content(forms)

  x <- lapply(forms$forms, function(form) {
    # form <- forms$forms[[1]]
    form <- lapply()
    do.call(cbind.data.frame, form)
    })
}
