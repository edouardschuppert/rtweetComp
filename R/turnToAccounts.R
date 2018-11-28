#' turnToAccount
#'
#' Turn the 88 columns returned by rtweet into a user account dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @param df The target table
#' @return A user dataset
#' @export

turnToAccount <- function(df) {

  df %>%
    mutate(url_account = paste0("https://twitter.com/", .data$screen_name)) %>%
    select(.data$user_id, .data$screen_name, .data$followers_count, .data$friends_count, .data$listed_count, .data$statuses_count, .data$favourites_count,
           .data$description, .data$url, .data$protected, .data$name, .data$location, .data$account_created_at, .data$verified, .data$profile_url,
           .data$profile_expanded_url, .data$account_lang, .data$profile_banner_url, .data$profile_background_url, .data$profile_image_url)

}
