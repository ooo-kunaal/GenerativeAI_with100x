# send_digest.R — Headless pipeline: fetch → dedup → score → email
# Run via: Rscript send_digest.R
# Env vars: GEMINI_API_KEY, YOUTUBE_API_KEY, RESEND_API_KEY, GOOGLE_SA_JSON (or file)

cat("=== OPT Daily Digest ===\n")
cat("Time:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n")

# Write service account JSON from env var if file doesn't exist
if (!file.exists("service-account.json")) {
  sa_json <- Sys.getenv("GOOGLE_SA_JSON", "")
  if (nchar(sa_json) > 0) {
    writeLines(sa_json, "service-account.json")
    cat("Wrote service-account.json from env var\n")
  } else {
    stop("No service-account.json and GOOGLE_SA_JSON env var not set")
  }
}

# Source helpers
source("R/config.R")
source("R/feedback.R")
source("R/fetch_rss.R")
source("R/fetch_youtube.R")
source("R/fetch_github.R")
source("R/dedup.R")
source("R/prompt.R")
source("R/score_items.R")
source("R/Logging.R")
source("R/email.R")

MUST_READ_THRESHOLD <- 8
FILTERED_THRESHOLD <- 5
TOP_N <- 15

# --- Pipeline ---
tryCatch({
  # Init Google Sheets
  init_config_sheet()
  init_feedback_sheet()
  init_sources_sheet()

  # Read config + sources
  config <- read_config()
  sources_df <- read_sources()
  cat("Config loaded. Email:", config$email_address %||% "(not set)", "\n")

  # Check email is enabled
  if (tolower(config$email_enabled %||% "FALSE") != "true") {
    cat("Email disabled in config. Exiting.\n")
    quit(status = 0)
  }

  # Step 1: Fetch
  cat("\n--- Fetching ---\n")
  rss_items <- tryCatch(fetch_rss(sources_df), error = function(e) {
    log_warn("RSS", e$message); data.frame() })
  yt_items <- tryCatch(fetch_youtube(sources_df), error = function(e) {
    log_warn("YOUTUBE", e$message); data.frame() })
  gh_items <- tryCatch(fetch_github_trending(), error = function(e) {
    log_warn("GITHUB", e$message); data.frame() })
  items <- dplyr::bind_rows(rss_items, yt_items, gh_items)
  cat("Fetched:", nrow(items), "items\n")
  if (nrow(items) == 0) {
    cat("No content found. Exiting.\n")
    quit(status = 0)
  }

  # Step 2: Dedup
  items <- deduplicate(items)
  cat("After dedup:", nrow(items), "items\n")

  # Step 3: Topic profile from feedback
  tp <- tryCatch(build_topic_profile(), error = function(e) NULL)
  if (!is.null(tp) && tp$n_feedback > 0) {
    cat("Topic profile loaded:", tp$n_feedback, "past ratings\n")
  }

  # Step 4: Score
  cat("\n--- Scoring ---\n")
  scored <- score_items(items, config, tp)
  scored <- scored[order(-scored$score), ]
  scored <- scored[scored$score >= FILTERED_THRESHOLD, ]
  if (nrow(scored) > TOP_N) scored <- scored[1:TOP_N, ]
  cat("Scored:", nrow(scored), "items above threshold\n")
  if (nrow(scored) == 0) {
    cat("No items passed filter. Exiting.\n")
    quit(status = 0)
  }

  # Step 5: Email
  cat("\n--- Sending email ---\n")
  ok <- send_digest_email(scored, config, tp)
  if (ok) {
    cat("\u2713 Digest sent successfully!\n")
    log_info("EMAIL", paste("Digest sent to", config$email_address, "-", nrow(scored), "items"))
  } else {
    cat("\u26A0 Email send failed.\n")
    log_error("EMAIL", "send_digest_email returned FALSE")
    quit(status = 1)
  }
}, error = function(e) {
  cat("\u26A0 Fatal error:", e$message, "\n")
  quit(status = 1)
})
