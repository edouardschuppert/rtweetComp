#' turnToPost
#'
#' Turn the 88 columns returned by rtweet into a posts dataset
#'
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @param df The target table
#' @return A posts dataset
#' @export

turnToPost <- function(df) {

  df %>%
    mutate(url_post = paste0("https://twitter.com/", screen_name, "/status/", status_id),
           text = ifelse(is_retweet == TRUE, paste0("RT @", retweet_screen_name, ": ", text), text),
           characters = nchar(text),
           stamp = as.integer(created_at),
           engagement = as.numeric(retweet_count) + as.numeric(favorite_count),
           retweet_engagement = as.numeric(retweet_favorite_count) + as.numeric(retweet_retweet_count),
           retweet_url = paste0("https://twitter.com/", retweet_screen_name, "/status/", retweet_status_id),
           quoted_url = paste0("https://twitter.com/", quoted_screen_name, "/status/", quoted_status_id),
           quoted_engagement = as.numeric(quoted_favorite_count) + as.numeric(quoted_retweet_count)) %>%
    select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, engagement, lang, source, url_post,
           is_retweet, is_quote, urls_url, urls_t.co, urls_expanded_url, media_url, media_t.co, media_expanded_url, media_type, ext_media_url,
           ext_media_t.co, ext_media_expanded_url, ext_media_type,
           retweet_url, retweet_status_id, retweet_user_id, retweet_screen_name, retweet_created_at, retweet_text, retweet_favorite_count,
           retweet_retweet_count, retweet_engagement, retweet_source,
           quoted_url, quoted_status_id, quoted_user_id, quoted_screen_name, quoted_created_at, quoted_text, quoted_favorite_count,
           quoted_retweet_count, quoted_source,
           reply_to_status_id, reply_to_user_id, reply_to_screen_name, mentions_user_id, mentions_screen_name,
           stamp, display_text_width, characters, hashtags, symbols) %>%
    flatlist()

}

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

    x <- df %>%select(i)

    stock <- names(x)

    names(x) <- "temp_name"

    if (class(x$temp_name) == "list") {

      x <- x %>%
        mutate(temp_name = as.character(temp_name))

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
