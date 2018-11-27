#' turnToAccount
#'
#' Turn the 88 columns returned by rtweet into a user account dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @param df The target table
#' @return A user dataset
#' @export

turnToAccount <- function(df) {

  df %>%
    mutate(url_account = paste0("https://twitter.com/", screen_name)) %>%
    select(user_id, screen_name, followers_count, friends_count, listed_count, statuses_count, favourites_count, description, url, protected, name, location,
           account_created_at, verified, profile_url, profile_expanded_url, account_lang, profile_banner_url, profile_background_url, profile_image_url)

}
