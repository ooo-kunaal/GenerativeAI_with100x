# Google Sheets feedback logging + topic-level learning
library(googlesheets4)

init_feedback_sheet <- function() {
  auth_google_sheets()
  sheets <- tryCatch(sheet_names(SPREADSHEET_ID), error = function(e) character(0))
  if (!"Feedback" %in% sheets) {
    sheet_add(SPREADSHEET_ID, sheet = "Feedback")
    header <- data.frame(
      timestamp = character(0), title = character(0), url = character(0),
      source = character(0), score = integer(0), justification = character(0),
      feedback = character(0), stringsAsFactors = FALSE)
    sheet_write(header, ss = SPREADSHEET_ID, sheet = "Feedback")
    message("Created Feedback sheet")
  }
}

log_feedback <- function(title, url, source, score, justification, feedback) {
  auth_google_sheets()
  row <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    title = title, url = url, source = source,
    score = as.integer(score), justification = justification,
    feedback = feedback, stringsAsFactors = FALSE)
  tryCatch({ sheet_append(SPREADSHEET_ID, data = row, sheet = "Feedback"); TRUE },
           error = function(e) { message("Error logging feedback: ", e$message); FALSE })
}

read_feedback <- function(max_rows = 200) {
  auth_google_sheets()
  tryCatch({
    df <- read_sheet(SPREADSHEET_ID, sheet = "Feedback")
    if (nrow(df) == 0) return(data.frame())
    # Keep most recent N rows
    if (nrow(df) > max_rows) df <- df[(nrow(df) - max_rows + 1):nrow(df), ]
    as.data.frame(df, stringsAsFactors = FALSE)
  }, error = function(e) { message("Error reading feedback: ", e$message); data.frame() })
}

# --- Topic-Level Learning ---
# Extract keyword preferences from feedback history using TF-IDF-like approach
# Inspired by: Cai et al. (2025) "Agentic Feedback Loop Modeling" (SIGIR '25)
# Key insight: feed feedback history into recommendation agent memory to refine preferences
build_topic_profile <- function(feedback_df = NULL) {
  if (is.null(feedback_df) || nrow(feedback_df) == 0) {
    feedback_df <- read_feedback()
  }
  if (nrow(feedback_df) == 0) return(list(liked_topics = character(0), disliked_topics = character(0),
                                          source_scores = list(), summary = ""))
  # Split by feedback type
  useful <- feedback_df[feedback_df$feedback == "useful", ]
  not_useful <- feedback_df[feedback_df$feedback == "not_useful", ]
  # Extract keywords from titles + justifications
  liked_words <- extract_keywords(paste(useful$title, useful$justification, collapse = " "))
  disliked_words <- extract_keywords(paste(not_useful$title, not_useful$justification, collapse = " "))
  # Remove overlap — keep only distinctive preferences
  overlap <- intersect(names(liked_words), names(disliked_words))
  for (w in overlap) {
    if (liked_words[w] > disliked_words[w]) {
      disliked_words <- disliked_words[names(disliked_words) != w]
    } else if (disliked_words[w] > liked_words[w]) {
      liked_words <- liked_words[names(liked_words) != w]
    } else {
      liked_words <- liked_words[names(liked_words) != w]
      disliked_words <- disliked_words[names(disliked_words) != w]
    }
  }
  # Top N keywords
  liked_top <- head(sort(liked_words, decreasing = TRUE), 15)
  disliked_top <- head(sort(disliked_words, decreasing = TRUE), 10)
  # Source reliability: useful_count / total_count per source (Bayesian smoothing)
  source_scores <- list()
  all_sources <- unique(feedback_df$source)
  for (src in all_sources) {
    src_fb <- feedback_df[feedback_df$source == src, ]
    n_useful <- sum(src_fb$feedback == "useful")
    n_total <- nrow(src_fb)
    # Bayesian: add 1 useful + 1 not_useful as prior (starts at 0.5)
    source_scores[[src]] <- round((n_useful + 1) / (n_total + 2), 2)
  }
  # Build summary text for injection into prompt
  summary_parts <- c()
  if (length(liked_top) > 0)
    summary_parts <- c(summary_parts, paste0(
      "Topics you've found USEFUL: ", paste(names(liked_top), collapse = ", ")))
  if (length(disliked_top) > 0)
    summary_parts <- c(summary_parts, paste0(
      "Topics you've found NOT USEFUL: ", paste(names(disliked_top), collapse = ", ")))
  if (length(source_scores) > 0) {
    good_src <- names(source_scores)[unlist(source_scores) >= 0.6]
    bad_src <- names(source_scores)[unlist(source_scores) < 0.4]
    if (length(good_src) > 0)
      summary_parts <- c(summary_parts, paste0("Reliable sources: ", paste(good_src, collapse = ", ")))
    if (length(bad_src) > 0)
      summary_parts <- c(summary_parts, paste0("Less relevant sources: ", paste(bad_src, collapse = ", ")))
  }
  list(liked_topics = names(liked_top), disliked_topics = names(disliked_top),
       source_scores = source_scores,
       summary = paste(summary_parts, collapse = "\n"),
       n_feedback = nrow(feedback_df),
       precision = if (nrow(feedback_df) > 0) round(sum(feedback_df$feedback == "useful") / nrow(feedback_df), 3) else NA)
}

# Simple keyword extraction: tokenize, remove stopwords, count
extract_keywords <- function(text) {
  if (is.na(text) || text == "") return(setNames(integer(0), character(0)))
  text <- tolower(text)
  text <- gsub("[^a-z0-9 ]", " ", text)
  words <- unlist(strsplit(text, "\\s+"))
  words <- words[nchar(words) > 2]
  # Stopwords (minimal set)
  stops <- c("the","and","for","are","but","not","you","all","can","had","her","was",
             "one","our","out","has","his","how","its","may","new","now","old","see","way",
             "who","did","get","let","say","she","too","use","that","this","with","will",
             "from","have","been","your","more","about","would","make","like","just","over",
             "such","into","than","them","some","could","what","there","when","which","their",
             "other","each","very","most","also","item","score","content","relevant","useful",
             "because","article","video","helps","directly","related","covers","aligns",
             "learning","goals","project","build","based","read","watch")
  words <- words[!words %in% stops]
  if (length(words) == 0) return(setNames(integer(0), character(0)))
  sort(table(words), decreasing = TRUE)
}

# --- Precision Monitoring ---
get_precision_stats <- function(feedback_df = NULL) {
  if (is.null(feedback_df) || nrow(feedback_df) == 0) {
    feedback_df <- read_feedback()
  }
  if (nrow(feedback_df) == 0) return(list(overall = NA, recent = NA, by_source = list(), trend = "insufficient_data"))
  n <- nrow(feedback_df)
  overall <- sum(feedback_df$feedback == "useful") / n
  # Recent 20 items
  recent_n <- min(20, n)
  recent_df <- feedback_df[(n - recent_n + 1):n, ]
  recent <- sum(recent_df$feedback == "useful") / recent_n
  # By source
  by_source <- list()
  for (src in unique(feedback_df$source)) {
    src_df <- feedback_df[feedback_df$source == src, ]
    by_source[[src]] <- list(
      precision = round(sum(src_df$feedback == "useful") / nrow(src_df), 3),
      count = nrow(src_df))
  }
  # Trend: compare first half vs second half
  trend <- "insufficient_data"
  if (n >= 10) {
    mid <- n %/% 2
    first_half <- sum(feedback_df$feedback[1:mid] == "useful") / mid
    second_half <- sum(feedback_df$feedback[(mid+1):n] == "useful") / (n - mid)
    trend <- if (second_half > first_half + 0.05) "improving"
    else if (second_half < first_half - 0.05) "declining"
    else "stable"
  }
  list(overall = round(overall, 3), recent = round(recent, 3),
       by_source = by_source, trend = trend, total = n)
}