# Fetch recent videos from YouTube channels via Data API v3 (reads sources from Google Sheets)
library(httr2)
library(jsonlite)

YOUTUBE_API_KEY <- Sys.getenv("YOUTUBE_API_KEY", "")
YOUTUBE_SEARCH_URL <- "https://www.googleapis.com/youtube/v3/search"
LOOKBACK_HOURS_YT <- 168  # 7 days

fetch_youtube <- function(sources_df = NULL, api_key = YOUTUBE_API_KEY,
                          lookback_hours = LOOKBACK_HOURS_YT) {
  if (is.null(api_key) || api_key == "") {
    message("  \u26A0 YouTube API key not set \u2014 skipping YouTube sources")
    return(data.frame())
  }
  if (is.null(sources_df)) sources_df <- read_sources()
  channels <- sources_df[sources_df$type == "youtube" & sources_df$active == TRUE, ]
  if (nrow(channels) == 0) { message("  No active YouTube sources"); return(data.frame()) }
  cutoff <- format(Sys.time() - as.difftime(lookback_hours, units = "hours"),
                   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  items <- list()
  for (r in seq_len(nrow(channels))) {
    ch_name <- channels$name[r]
    ch_id <- channels$identifier[r]
    # Resolve URL/handle to channel ID if needed
    ch_id <- resolve_channel_id(ch_id, api_key)
    if (is.null(ch_id) || ch_id == "") {
      message("  \u26A0 Could not resolve channel ID for ", ch_name, " \u2014 skipping")
      next
    }
    tryCatch({
      message("  Fetching YouTube: ", ch_name, "...")
      resp <- request(YOUTUBE_SEARCH_URL) |>
        req_url_query(key = api_key, channelId = ch_id, order = "date",
                      type = "video", maxResults = 20, part = "snippet", publishedAfter = cutoff) |>
        req_timeout(15) |> req_perform()
      data <- resp_body_json(resp)
      videos <- data$items %||% list()
      count <- 0
      for (v in videos) {
        video_id <- v$id$videoId
        if (is.null(video_id)) next
        snippet <- v$snippet
        title <- snippet$title %||% "Untitled"
        desc <- snippet$description %||% ""
        if (nchar(desc) > 1000) desc <- paste0(substr(desc, 1, 1000), "...")
        pub_raw <- snippet$publishedAt %||% ""
        published <- tryCatch(as.POSIXct(pub_raw, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                              error = function(e) Sys.time())
        url <- paste0("https://youtube.com/watch?v=", video_id)
        items[[length(items) + 1]] <- data.frame(
          title = title, source = paste0(ch_name, " (YouTube)"),
          url = url, text = desc, published_at = published,
          item_type = "video", stringsAsFactors = FALSE)
        count <- count + 1
      }
      message("  \u2713 ", ch_name, ": ", count, " recent video(s)")
    }, error = function(e) {
      message("  \u26A0 Error fetching ", ch_name, ": ", e$message, " \u2014 skipping")
    })
  }
  if (length(items) == 0) return(data.frame())
  dplyr::bind_rows(items)
}

# Resolve YouTube URL, handle, or username to a channel ID
resolve_channel_id <- function(identifier, api_key) {
  id <- trimws(identifier)
  # Already a channel ID
  if (grepl("^UC[a-zA-Z0-9_-]{22}$", id)) return(id)
  # Extract handle from URL: youtube.com/@handle
  handle_match <- regmatches(id, regexpr("@[a-zA-Z0-9_.-]+", id))
  if (length(handle_match) == 1) {
    # Use channels API with forHandle
    resolved <- tryCatch({
      resp <- request("https://www.googleapis.com/youtube/v3/channels") |>
        req_url_query(key = api_key, forHandle = handle_match, part = "id") |>
        req_timeout(10) |> req_perform()
      data <- resp_body_json(resp)
      if (length(data$items) > 0) data$items[[1]]$id else NULL
    }, error = function(e) { message("    Handle lookup failed: ", e$message); NULL })
    return(resolved)
  }
  # Extract channel ID from URL: youtube.com/channel/UCxxxx
  uc_match <- regmatches(id, regexpr("UC[a-zA-Z0-9_-]{22}", id))
  if (length(uc_match) == 1) return(uc_match)
  # Fallback: try as forUsername
  tryCatch({
    resp <- request("https://www.googleapis.com/youtube/v3/channels") |>
      req_url_query(key = api_key, forUsername = id, part = "id") |>
      req_timeout(10) |> req_perform()
    data <- resp_body_json(resp)
    if (length(data$items) > 0) data$items[[1]]$id else NULL
  }, error = function(e) NULL)
}
