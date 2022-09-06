# Ajouts : flatlist, povertext, convert_date

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

    classe <- as.character(class(x$temp_name))

    if (any(classe == "list")) x <- x %>% mutate(temp_name = as.character(.data$temp_name))

    names(x) <- stock

    if (exists("newtab") == TRUE) newtab <- cbind(x, newtab)
    if (exists("newtab") == FALSE) newtab <- x

    i <- i - 1
  }

  newtab

}

#' povertext
#'
#' Deplete the text value
#' @importFrom stringr str_to_lower
#' @importFrom stringr str_replace_all
#' @param val Your data frame
#' @return Return a text, without any accent or links

povertext <- function(val) {

  val <- str_to_lower(val)

  val <- str_remove_all(val, "http(?:s)?://[^[:space:]]*") # Links are deleted

  val <- str_replace_all(val, "\u00E0", "a")
  val <- str_replace_all(val, "\u00E1", "a")
  val <- str_replace_all(val, "\u00E2", "a")
  val <- str_replace_all(val, "\u00E3", "a")
  val <- str_replace_all(val, "\u00E4", "a")
  val <- str_replace_all(val, "\u00E5", "a")

  val <- str_replace_all(val, "\u00E9", "e")
  val <- str_replace_all(val, "\u00E8", "e")
  val <- str_replace_all(val, "\u00EA", "e")
  val <- str_replace_all(val, "\u00EB", "e")

  val <- str_replace_all(val, "\u00EC", "i")
  val <- str_replace_all(val, "\u00ED", "i")
  val <- str_replace_all(val, "\u00EE", "i")
  val <- str_replace_all(val, "\u00EF", "i")

  val <- str_replace_all(val, "\u00F2", "o")
  val <- str_replace_all(val, "\u00F3", "o")
  val <- str_replace_all(val, "\u00F4", "o")
  val <- str_replace_all(val, "\u00F5", "o")
  val <- str_replace_all(val, "\u00F6", "o")

  val <- str_replace_all(val, "\u00F9", "u")
  val <- str_replace_all(val, "\u00FA", "u")
  val <- str_replace_all(val, "\u00FB", "u")
  val <- str_replace_all(val, "\u00FC", "u")

  val <- str_replace_all(val, "\u00FF", "y")
  val <- str_replace_all(val, "\u00FD", "y")

  val <- str_replace_all(val, "\u00E7", "c")
  val <- str_replace_all(val, "\u00E6", "ae")
  val <- str_replace_all(val, "\u00F1", "n")

  val

}

#' convert_date
#'
#' Convert messy dates as they come from the Twitter API in text format
#' @importFrom stringr str_extract
#' @importFrom stringr str_remove_all
#' @importFrom dplyr recode
#' @importFrom lubridate as_datetime
#' @import rex
#' @param created_at the date string to convert
#' @return Return the date in a datetime format

convert_date <-
  function(created_at) {

    quoted_created_at_h <- str_extract(created_at, rex(numbers, ":", numbers, ":", numbers))
    quoted_created_at_y <- str_extract(created_at, rex(number, number, number, number, end))
    quoted_created_at_j <- str_extract(created_at, rex(space, numbers, numbers, space))
    quoted_created_at_j <- str_remove_all(quoted_created_at_j, rex(spaces))
    quoted_created_at_m <- str_extract(created_at, rex(space, letters, space))
    quoted_created_at_m <- str_remove_all(quoted_created_at_m, rex(spaces))
    quoted_created_at_m <- recode(
      quoted_created_at_m,
      "Jan" = "01",
      "Feb" = "02",
      "Mar" = "03",
      "Apr" = "04",
      "May" = "05",
      "Jun" = "06",
      "Jul" = "07",
      "Aug" = "08",
      "Sep" = "09",
      "Oct" = "10",
      "Nov" = "11",
      "Dec" = "12"
    )

    as_datetime(paste0(quoted_created_at_y, "-", quoted_created_at_m, "-", quoted_created_at_j, " ", quoted_created_at_h))

  }
