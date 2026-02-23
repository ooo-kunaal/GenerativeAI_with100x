# Structured logging for OPT pipeline
LOG_FILE <- "opt_pipeline.log"

opt_log <- function(level, component, msg) {
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- paste0("[", ts, "] [", toupper(level), "] [", component, "] ", msg)
  message(line)
  tryCatch(cat(line, "\n", file = LOG_FILE, append = TRUE), error = function(e) NULL)
}

log_info <- function(component, msg) opt_log("INFO", component, msg)
log_warn <- function(component, msg) opt_log("WARN", component, msg)
log_error <- function(component, msg) opt_log("ERROR", component, msg)

# Log pipeline run summary
log_pipeline_summary <- function(n_fetched, n_deduped, n_scored, n_filtered, duration_secs) {
  log_info("PIPELINE", sprintf(
    "Run complete: %d fetched -> %d deduped -> %d scored -> %d displayed (%.1fs)",
    n_fetched, n_deduped, n_scored, n_filtered, duration_secs))
}
