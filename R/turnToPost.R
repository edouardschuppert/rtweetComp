#' turnToPost
#'
#' Turn the 88 columns returned by rtweet into a posts dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom lubridate as_datetime
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom bit64 as.integer64
#' @importFrom rtweet users_data
#' @import rex
#' @import tibble
#' @param df The target table
#' @param complete_user Run a query to retrieve missing user data. Default set to FALSE
#' @return A posts dataset
#' @export

turnToPost <- function(
    df,
    complete_user = FALSE
) {

  if (complete_user == TRUE) {
    users <- users_data(tweets = df) %>%
      turnToAccount()
  } else {
    users <-
      tibble(
        user_id = NA,
        screen_name = "NaN",
        followers_count = NA
      )
  }

  # processing of tweets globally
  df2 <-
    df %>%
    mutate(
      user_id = users$user_id,
      screen_name = users$screen_name,
      followers_count = users$followers_count,

      status_id = as.integer64(.data$id_str),
      text = .data$full_text,
      engagement = as.numeric(.data$retweet_count) + as.numeric(.data$favorite_count),
      stamp = as.integer(.data$created_at),
      url_post = paste0("https://twitter.com/", .data$screen_name, "/status/", .data$status_id),
      characters = nchar(.data$text),

      is_retweet = str_detect(.data$full_text, rex(start, "RT", space, "@")),
      reply_to_status_id = as.integer64(.data$in_reply_to_status_id_str),
      reply_to_user_id = as.integer64(.data$in_reply_to_user_id_str),
      reply_to_screen_name = as.integer64(.data$in_reply_to_screen_name),

      is_quote = !is.na(.data$quoted_status_id_str),
      quoted_status_id = as.integer64(.data$quoted_status_id_str),

      hashtags = NA,
      quoted_user_id = as.integer64(NA),
      quoted_screen_name = NA,
      quoted_text = NA,
      quoted_created_at = as_datetime(NA),
      quoted_favorite_count = NA,
      quoted_retweet_count = NA,
      quoted_source = NA,
      quoted_url = NA,
      symbols = NA,
      mentions_user_id = NA,
      mentions_screen_name = NA,

      urls_url = NA,
      urls_t.co = NA,
      urls_expanded_url = NA,

      media_url = NA,
      media_t.co = NA,
      media_expanded_url = NA,
      media_type = NA,

      ext_media_url = NA,
      ext_media_t.co = NA,
      ext_media_expanded_url = NA,
      ext_media_type = NA,

      retweet_url = NA,
      retweet_status_id = as.integer64(NA),
      retweet_user_id = as.integer64(NA),
      retweet_screen_name = NA,
      retweet_created_at = as_datetime(NA),
      retweet_favorite_count = NA,
      retweet_retweet_count = NA,
      retweet_engagement = NA,
      retweet_source = NA,
    )

  # processing tweets one by one
  for (i in 1:nrow(df2)) {

    retweets <- df$retweeted_status[i][[1]]

    if (is.data.frame(retweets) == TRUE) {
      retweet_status_id <- as.integer64(retweets$id_str)

      if (is.na(retweet_status_id) == FALSE) {
        df2$retweet_status_id[i] <- retweet_status_id

        retweet_user_id <- as.integer64(retweets$user$id_str)
        df2$retweet_user_id[i] <- retweet_user_id

        retweet_screen_name <- retweets$user$screen_name
        df2$retweet_screen_name[i] <- retweet_screen_name

        retweet_url <- paste0("https://twitter.com/", retweet_screen_name, "/status/", retweet_status_id)
        df2$retweet_url[i] <- retweet_url

        retweet_favorite_count <- retweets$favorite_count
        df2$retweet_favorite_count[i] <- retweet_favorite_count

        retweet_retweet_count <- retweets$retweet_count
        df2$retweet_retweet_count[i] <- retweet_retweet_count

        retweet_engagement <- retweet_favorite_count + retweet_retweet_count
        df2$retweet_engagement[i] <- retweet_engagement

        retweet_source <- retweets$source
        df2$retweet_source[i] <- retweet_source

        retweet_created_at <- convert_date(retweets$created_at)
        df2$retweet_created_at[i] <- retweet_created_at
      }
    }

    hashtags <- paste(df[[7]][[i]][["hashtags"]]$text, collapse = ", ")
    df2$hashtags[i] <- ifelse(hashtags == "NA", NA, hashtags)

    symbols <- paste(df[[7]][[i]][["symbols"]]$text, collapse = ", ")
    df2$symbols[i] <- ifelse(symbols == "NA", NA, symbols)

    user_mentions <- df[[7]][[i]][["user_mentions"]]

    mentions_user_id <- paste(user_mentions$id_str, collapse = ", ")
    df2$mentions_user_id[i] <- ifelse(mentions_user_id == "NA", NA, mentions_user_id)
    mentions_screen_name <- paste(user_mentions$screen_name, collapse = ", ")
    df2$mentions_screen_name[i] <- ifelse(mentions_screen_name == "NA", NA, mentions_screen_name)

    urls <- df[[7]][[i]][["urls"]]

    urls_t.co <- paste(urls$url, collapse = ", ")
    df2$urls_t.co[i] <- ifelse(urls_t.co == "NA", NA, urls_t.co)

    urls_expanded_url <- paste(urls$expanded_url, collapse = ", ")
    df2$urls_expanded_url[i] <- ifelse(urls_expanded_url == "NA", NA, urls_expanded_url)

    quoted <- df[[29]][[i]]
    if (is.data.frame(quoted) == TRUE) {
      quoted_user_id <- as.integer64(quoted$user$id_str)
      df2$quoted_user_id[i] <- quoted_user_id

      quoted_screen_name <- quoted$user$screen_name
      df2$quoted_screen_name[i] <- quoted_screen_name

      quoted_text <- quoted$full_text
      df2$quoted_text[i] <- quoted_text

      quoted_created_at <- quoted$created_at
      df2$quoted_created_at[i] <- convert_date(quoted_created_at)

      quoted_source <- quoted$source
      df2$quoted_source[i] <- quoted_source

      quoted_url <- ifelse(
        is.na(quoted_screen_name) == TRUE,
        NA,
        paste0("https://twitter.com/", quoted_screen_name, "/status/", df$quoted_status_id[i])
      )
      df2$quoted_url[i] <- quoted_url
    }

    rm(
      hashtags,
      quoted,
      quoted_user_id,
      quoted_screen_name,
      quoted_text,
      quoted_created_at,
      quoted_created_at_h,
      quoted_created_at_m,
      quoted_created_at_j,
      quoted_created_at_y,
      quoted_source,
      quoted_url,
      symbols,
      user_mentions,
      mentions_screen_name,
      mentions_user_id,
      urls,
      urls_expanded_url,
      urls_t.co,
      retweets,
      retweet_created_at,
      retweet_engagement,
      retweet_favorite_count,
      retweet_retweet_count,
      retweet_screen_name,
      retweet_source,
      retweet_status_id,
      retweet_url,
      retweet_user_id
    )
  }
  rm(i)

  # column selection
  df2 <-
    df2 %>%
    select(
      .data$user_id,
      .data$status_id,
      .data$created_at,
      .data$screen_name,
      .data$followers_count,
      .data$text,
      .data$favorite_count,
      .data$retweet_count,
      .data$engagement,
      .data$hashtags,
      .data$lang,
      .data$source,
      .data$url_post,
      .data$is_retweet,
      .data$is_quote,
      .data$urls_url,
      .data$urls_t.co,
      .data$urls_expanded_url,
      .data$media_url,
      .data$media_t.co,
      .data$media_expanded_url,
      .data$media_type,
      .data$ext_media_url,
      .data$ext_media_t.co,
      .data$ext_media_expanded_url,
      .data$ext_media_type,
      .data$retweet_url,
      .data$retweet_status_id,
      .data$retweet_user_id,
      .data$retweet_screen_name,
      .data$retweet_created_at,
      .data$retweet_favorite_count,
      .data$retweet_retweet_count,
      .data$retweet_engagement,
      .data$retweet_source,
      .data$quoted_url,
      .data$quoted_status_id,
      .data$quoted_user_id,
      .data$quoted_screen_name,
      .data$quoted_created_at,
      .data$quoted_text,
      .data$quoted_favorite_count,
      .data$quoted_retweet_count,
      .data$quoted_source,
      .data$reply_to_status_id,
      .data$reply_to_user_id,
      .data$reply_to_screen_name,
      .data$mentions_user_id,
      .data$mentions_screen_name,
      .data$stamp,
      .data$display_text_width,
      .data$characters,
      .data$symbols
    ) %>%
    flatlist()

  df2
}
