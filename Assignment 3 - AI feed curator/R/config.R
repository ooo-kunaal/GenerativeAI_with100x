# Google Sheets config read/write + sources management
library(googlesheets4)
library(gargle)
library(jsonlite)

SPREADSHEET_ID <- "1h7FIx1zUNt_XgKuy5RMRo_ZjJzmMjyaX8GkaM8Qqp3c"
SERVICE_ACCOUNT_JSON <- "service-account.json"

# Resolve service account path relative to app directory
resolve_sa_path <- function() {
  for (p in c(SERVICE_ACCOUNT_JSON, "xpersonalcontentai-012de0e8bd72.json")) {
    if (file.exists(p)) return(normalizePath(p))
  }
  SERVICE_ACCOUNT_JSON
}

DEFAULT_CONFIG <- list(
  learning_goals = "Build AI agents using Claude API and MCP. Understand tool calling, agentic workflows, and multi-step reasoning patterns.",
  project_context = "Building a research agent with web search and summarization. Deadline: end of March 2026.",
  skill_MCP = "beginner",
  skill_RAG = "intermediate",
  skill_LLM_fundamentals = "intermediate",
  skill_production_deployment = "beginner",
  learning_style = "build_first",
  depth_preference = "mixed",
  consumption_habits = "mixed",
  email_address = "kunal86970@gmail.com",
  email_enabled = "TRUE"
)

DEFAULT_SOURCES <- data.frame(
  type = c("rss","rss","rss","rss","rss","rss","youtube","youtube","youtube"),
  name = c("Ben's Bites","Latent Space","Epoch AI","Dwarkesh Podcast","Peter Steinberger","HuggingFace Blog","AI Engineer","Andrej Karpathy","Dwarkesh Podcast"),
  identifier = c(
    "https://www.bensbites.com/feed","https://www.latent.space/feed",
    "https://epochai.substack.com/feed","https://www.dwarkesh.com/feed",
    "https://steipete.me/rss.xml","https://huggingface.co/blog/feed.xml",
    "UCLKPca3kwwd-B59HNr-_lvA","UCXUPKJO5MZQN11PqgIvyuvQ","UCXl4i9dYBrFOabk0xGmbkRA"),
  active = rep(TRUE, 9),
  stringsAsFactors = FALSE
)

auth_google_sheets <- function() {
  sa_path <- resolve_sa_path()
  if (!file.exists(sa_path)) {
    stop("Service account file not found: ", sa_path, " (wd: ", getwd(), ")")
  }
  # Validate JSON structure before passing to gargle
  json <- tryCatch(jsonlite::fromJSON(sa_path), error = function(e) NULL)
  if (is.null(json) || json$type != "service_account") {
    stop("Invalid service account JSON at: ", sa_path)
  }
  gs4_auth(path = sa_path)
}

init_config_sheet <- function() {
  auth_google_sheets()
  sheets <- tryCatch(sheet_names(SPREADSHEET_ID), error = function(e) character(0))
  if (!"Config" %in% sheets) {
    sheet_add(SPREADSHEET_ID, sheet = "Config")
    df <- data.frame(field = names(DEFAULT_CONFIG), value = unlist(DEFAULT_CONFIG), stringsAsFactors = FALSE)
    sheet_write(df, ss = SPREADSHEET_ID, sheet = "Config")
    message("Created Config sheet with defaults")
  }
}

init_sources_sheet <- function() {
  auth_google_sheets()
  sheets <- tryCatch(sheet_names(SPREADSHEET_ID), error = function(e) character(0))
  if (!"Sources" %in% sheets) {
    sheet_add(SPREADSHEET_ID, sheet = "Sources")
    sheet_write(DEFAULT_SOURCES, ss = SPREADSHEET_ID, sheet = "Sources")
    message("Created Sources sheet with defaults")
  }
}

read_config <- function() {
  auth_google_sheets()
  tryCatch({
    df <- read_sheet(SPREADSHEET_ID, sheet = "Config", col_types = "cc")
    if (nrow(df) == 0) return(DEFAULT_CONFIG)
    config <- as.list(setNames(df$value, df$field))
    for (nm in names(DEFAULT_CONFIG)) {
      if (is.null(config[[nm]]) || is.na(config[[nm]])) config[[nm]] <- DEFAULT_CONFIG[[nm]]
    }
    config
  }, error = function(e) { message("Error reading config: ", e$message); DEFAULT_CONFIG })
}

write_config <- function(config) {
  auth_google_sheets()
  df <- data.frame(field = names(config), value = unlist(config), stringsAsFactors = FALSE)
  tryCatch({ sheet_write(df, ss = SPREADSHEET_ID, sheet = "Config"); TRUE },
           error = function(e) { message("Error writing config: ", e$message); FALSE })
}

read_sources <- function() {
  auth_google_sheets()
  tryCatch({
    df <- read_sheet(SPREADSHEET_ID, sheet = "Sources")
    if (nrow(df) == 0) return(DEFAULT_SOURCES)
    df$active <- as.logical(df$active)
    as.data.frame(df, stringsAsFactors = FALSE)
  }, error = function(e) { message("Error reading sources: ", e$message); DEFAULT_SOURCES })
}

write_sources <- function(sources_df) {
  auth_google_sheets()
  tryCatch({ sheet_write(sources_df, ss = SPREADSHEET_ID, sheet = "Sources"); TRUE },
           error = function(e) { message("Error writing sources: ", e$message); FALSE })
}

add_source <- function(type, name, identifier) {
  sources <- read_sources()
  new_row <- data.frame(type = type, name = name, identifier = identifier, active = TRUE, stringsAsFactors = FALSE)
  sources <- rbind(sources, new_row)
  write_sources(sources)
}

remove_source <- function(row_index) {
  sources <- read_sources()
  if (row_index >= 1 && row_index <= nrow(sources)) {
    sources <- sources[-row_index, ]
    write_sources(sources)
  }
}

get_skill_levels <- function(config) {
  skill_keys <- grep("^skill_", names(config), value = TRUE)
  setNames(as.list(unlist(config[skill_keys])), gsub("^skill_", "", skill_keys))
}
