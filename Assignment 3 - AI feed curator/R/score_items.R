# Score content items using ellmer + Gemini
library(ellmer)
library(jsonlite)
library(dplyr)

GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY", "AIzaSyAIMa3YIeAx5NrnWitATyvkwiiCuC3fC70")
GEMINI_MODEL <- "gemini-2.5-flash"

score_items <- function(items_df, config, topic_profile = NULL) {
  if (nrow(items_df) == 0) return(items_df)
  prompt <- build_scoring_prompt(items_df, config, topic_profile)
  tryCatch({
    chat <- chat_google_gemini(model = GEMINI_MODEL, api_key = GEMINI_API_KEY)
    response <- chat$chat(prompt)
    scores <- parse_scores(response, nrow(items_df))
    items_df$score <- scores$score
    items_df$justification <- scores$justification
    # Apply source reliability multiplier from topic profile
    if (!is.null(topic_profile) && length(topic_profile$source_scores) > 0) {
      for (i in seq_len(nrow(items_df))) {
        src <- items_df$source[i]
        if (src %in% names(topic_profile$source_scores)) {
          reliability <- topic_profile$source_scores[[src]]
          items_df$score[i] <- round(items_df$score[i] * (0.8 + 0.4 * reliability))
          items_df$score[i] <- max(0L, min(10L, items_df$score[i]))
        }
      }
    }
    items_df
  }, error = function(e) {
    message("  \u26A0 Scoring error: ", e$message, " \u2014 using fallback scores")
    items_df$score <- 5L
    items_df$justification <- "Could not score \u2014 Gemini API call failed. Included as a maybe."
    items_df
  })
}

parse_scores <- function(response_text, n_items) {
  text <- trimws(response_text)
  text <- sub("^\\s*```[a-zA-Z]*\\s*\\n?", "", text)
  text <- sub("\\n?\\s*```\\s*$", "", text)
  text <- trimws(text)
  json_match <- regmatches(text, regexpr("\\[\\s*\\{[\\s\\S]*\\}\\s*\\]", text, perl = TRUE))
  if (length(json_match) == 1) text <- json_match
  # Collapse newlines — Gemini wraps justification strings across lines
  text <- gsub("\n", " ", text, fixed = TRUE)
  text <- gsub("\r", " ", text, fixed = TRUE)
  scores_list <- fromJSON(text, simplifyDataFrame = TRUE)
  if (!is.data.frame(scores_list)) scores_list <- bind_rows(scores_list)
  result <- data.frame(score = rep(5L, n_items),
    justification = rep("Item was not scored by Gemini.", n_items), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(scores_list))) {
    idx <- scores_list$index[i] + 1
    if (!is.na(idx) && idx >= 1 && idx <= n_items) {
      result$score[idx] <- max(0L, min(10L, as.integer(scores_list$score[i])))
      result$justification[idx] <- as.character(scores_list$justification[i])
    }
  }
  result
}
