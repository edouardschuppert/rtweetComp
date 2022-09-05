#' turnToAccount
#'
#' Turn the columns returned by rtweet into a user account dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @importFrom bit64 as.integer64
#' @param df The target table
#' @return A user dataset
#' @export

turnToAccount <- function(df) {

  df %>%
    mutate(
      url_account = paste0("https://twitter.com/", .data$screen_name),
      user_id = as.integer64(.data$id_str),
      account_created_at = .data$created_at,
      profile_image_url = .data$profile_image_url_https,
      profile_url = .data$url,
      profile_expanded_url = NA,
      account_lang = NA,
      profile_background_url = NA
    ) %>%
    select(
      .data$user_id,
      .data$screen_name,
      .data$followers_count,
      .data$friends_count,
      .data$listed_count,
      .data$statuses_count,
      .data$favourites_count,
      .data$description,
      .data$url,
      .data$url_account,
      .data$protected,
      .data$name,
      .data$location,
      .data$account_created_at,
      .data$verified,
      .data$profile_url,
      .data$profile_expanded_url,
      .data$account_lang,
      .data$profile_banner_url,
      .data$profile_background_url,
      .data$profile_image_url
    )

}
