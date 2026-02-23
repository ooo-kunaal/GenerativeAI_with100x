# Fetch recent articles from RSS feeds (reads sources from Google Sheets)
library(tidyRSS)
library(dplyr)

LOOKBACK_HOURS <- 168  # 7 days

fetch_rss <- function(sources_df = NULL, lookback_hours = LOOKBACK_HOURS) {
  if (is.null(sources_df)) sources_df <- read_sources()
  feeds <- sources_df[sources_df$type == "rss" & sources_df$active == TRUE, ]
  if (nrow(feeds) == 0) { message("  No active RSS sources"); return(data.frame()) }
  cutoff <- Sys.time() - as.difftime(lookback_hours, units = "hours")
  items <- list()
  for (r in seq_len(nrow(feeds))) {
    feed_name <- feeds$name[r]
    feed_url <- feeds$identifier[r]
    tryCatch({
      message("  Fetching RSS: ", feed_name, "...")
      parsed <- tidyfeed(feed_url)
      if (nrow(parsed) == 0) { message("  \u26A0 No entries from ", feed_name); next }
      date_col <- intersect(c("item_pub_date","entry_published","item_date_published"), names(parsed))
      if (length(date_col) == 0) parsed$pub_date <- Sys.time()
      else parsed$pub_date <- as.POSIXct(parsed[[date_col[1]]], tz = "UTC")
      parsed <- parsed[!is.na(parsed$pub_date) & parsed$pub_date >= cutoff, ]
      if (nrow(parsed) == 0) { message("  \u2713 ", feed_name, ": 0 recent items"); next }
      text_col <- intersect(c("item_description","entry_content","item_content"), names(parsed))
      title_col <- intersect(c("item_title","entry_title"), names(parsed))
      link_col <- intersect(c("item_link","entry_url","entry_id"), names(parsed))
      for (j in seq_len(nrow(parsed))) {
        title <- if (length(title_col) > 0) as.character(parsed[[title_col[1]]][j]) else "Untitled"
        link <- if (length(link_col) > 0) as.character(parsed[[link_col[1]]][j]) else ""
        text <- if (length(text_col) > 0) as.character(parsed[[text_col[1]]][j]) else ""
        text <- strip_html(text)
        if (nchar(text) > 1000) text <- paste0(substr(text, 1, 1000), "...")
        items[[length(items) + 1]] <- data.frame(
          title = title, source = paste0(feed_name, " (RSS)"),
          url = link, text = text, published_at = parsed$pub_date[j],
          item_type = "article", stringsAsFactors = FALSE)
      }
      message("  \u2713 ", feed_name, ": ", nrow(parsed), " recent item(s)")
    }, error = function(e) {
      message("  \u26A0 Error fetching ", feed_name, ": ", e$message, " \u2014 skipping")
    })
  }
  if (length(items) == 0) return(data.frame())
  dplyr::bind_rows(items)
}

strip_html <- function(text) {
  if (is.na(text) || text == "") return("")
  clean <- gsub("<[^>]+>", "", text)
  clean <- gsub("\\s+", " ", trimws(clean))
  clean <- gsub("&amp;", "&", clean, fixed = TRUE)
  clean <- gsub("&lt;", "<", clean, fixed = TRUE)
  clean <- gsub("&gt;", ">", clean, fixed = TRUE)
  clean <- gsub("&quot;", '"', clean, fixed = TRUE)
  clean <- gsub("&#39;", "'", clean, fixed = TRUE)
  clean <- gsub("&nbsp;", " ", clean, fixed = TRUE)
  clean
}