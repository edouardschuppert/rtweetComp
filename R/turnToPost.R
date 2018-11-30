#' turnToPost
#'
#' Turn the 88 columns returned by rtweet into a posts dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @import rex
#' @param df The target table
#' @return A posts dataset
#' @export

turnToPost <- function(df) {

  df %>%
    mutate(url_post = paste0("https://twitter.com/", .data$screen_name, "/status/", .data$status_id),
           text = ifelse(.data$is_retweet == TRUE, paste0("RT @", .data$retweet_screen_name, ": ", .data$text), .data$text),
           characters = nchar(.data$text),
           stamp = as.integer(.data$created_at),
           engagement = as.numeric(.data$retweet_count) + as.numeric(.data$favorite_count),
           retweet_engagement = as.numeric(.data$retweet_favorite_count) + as.numeric(.data$retweet_retweet_count),
           retweet_url = paste0("https://twitter.com/", .data$retweet_screen_name, "/status/", .data$retweet_status_id),
           quoted_url = paste0("https://twitter.com/", .data$quoted_screen_name, "/status/", .data$quoted_status_id),
           quoted_engagement = as.numeric(.data$quoted_favorite_count) + as.numeric(.data$quoted_retweet_count),
           hashtags = as.character(str_extract_all(.data$text, "(?:(?:^|[[:space:]]+)|(?:[[:punct:]])?)#(?:[^[:blank:]]*|[^[:space:]]*)(?:(?:(?:[[:punct:]])?|[[:space:]])|$)")),
           hashtags = str_remove_all(.data$hashtags, "\""),
           hashtags = str_remove_all(.data$hashtags, rex("c(")),
           hashtags = str_remove_all(.data$hashtags, rex(")")),
           hashtags = str_remove_all(.data$hashtags, ","),
           hashtags = str_remove_all(.data$hashtags, rex(".")),
           hashtags = str_remove_all(.data$hashtags, ":"),
           hashtags = str_remove_all(.data$hashtags, "\n"),
           hashtags = str_replace(.data$hashtags, "^[[:space:]]#", "#")) %>%
    filter(str_detect(.data$hashtags, "^#$") == FALSE) %>%
    mutate(hashtags = ifelse(.data$hashtags == "character(0", NA, .data$hashtags)) %>%
    select(.data$user_id, .data$status_id, .data$created_at, .data$screen_name, .data$followers_count, .data$text, .data$favorite_count, .data$retweet_count, .data$engagement,
           .data$hashtags, .data$lang, .data$source, .data$url_post, .data$is_retweet, .data$is_quote, .data$urls_url, .data$urls_t.co, .data$urls_expanded_url,
           .data$media_url, .data$media_t.co, .data$media_expanded_url, .data$media_type, .data$ext_media_url, .data$ext_media_t.co, .data$ext_media_expanded_url,
           .data$ext_media_type, .data$retweet_url, .data$retweet_status_id, .data$retweet_user_id, .data$retweet_screen_name, .data$retweet_created_at,
           .data$retweet_.data$text, .data$retweet_favorite_count, .data$retweet_retweet_count, .data$retweet_engagement, .data$retweet_source, .data$quoted_url,
           .data$quoted_status_id, .data$quoted_user_id, .data$quoted_screen_name, .data$quoted_created_at, .data$quoted_text, .data$quoted_favorite_count,
           .data$quoted_retweet_count, .data$quoted_source, .data$reply_to_status_id, .data$reply_to_user_id, .data$reply_to_screen_name, .data$mentions_user_id,
           .data$mentions_screen_name, .data$stamp, .data$display_text_width, .data$characters, .data$symbols) %>%
    flatlist()

}

# Colonne avec typologie des retweeters
# Refaire flatlist avec get()



# Ajouts : flatlist

#' flatlist
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @param df Your data frame
#' @return allows you to manage the columns of an array in list format, transforming them into character columns.

flatlist <- function(df) {

  i <- as.numeric(length(df))

  while (i != 0) {

    x <- df %>% select(i)

    stock <- names(x)

    names(x) <- "temp_name"

    if (class(x$temp_name) == "list") {

      x <- x %>%
        mutate(temp_name = as.character(.data$temp_name))

    }

    names(x) <- stock

    if (exists("newtab") == TRUE) {

      newtab <- cbind(x, newtab)

    }

    if (exists("newtab") == FALSE) {

      newtab <- x

    }

    rm(x, stock)

    i <- i - 1
  }

  newtab

}
