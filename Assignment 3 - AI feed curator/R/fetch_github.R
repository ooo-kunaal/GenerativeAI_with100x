# Scrape GitHub Trending repos
library(httr2)
library(dplyr)

GITHUB_TRENDING_URL <- "https://github.com/trending"

fetch_github_trending <- function(language = "", since = "weekly") {
  tryCatch({
    message("  Fetching GitHub Trending...")
    url <- GITHUB_TRENDING_URL
    if (nchar(language) > 0) url <- paste0(url, "/", language)
    resp <- request(url) |>
      req_url_query(since = since) |>
      req_headers("User-Agent" = "OPT-Learning-Feed/1.0") |>
      req_timeout(15) |>
      req_perform()
    html <- resp_body_string(resp)
    # Parse repo entries from HTML
    items <- parse_trending_html(html)
    message("  \u2713 GitHub Trending: ", nrow(items), " repo(s)")
    items
  }, error = function(e) {
    message("  \u26A0 Error fetching GitHub Trending: ", e$message, " \u2014 skipping")
    data.frame()
  })
}

parse_trending_html <- function(html) {
  # Extract repo blocks using regex (avoids rvest dependency)
  # Each repo is in an <article class="Box-row">
  articles <- regmatches(html, gregexpr("<article[^>]*>.*?</article>", html, perl = TRUE))[[1]]
  if (length(articles) == 0) return(data.frame())
  items <- list()
  for (article in articles) {
    # Extract repo path: <a href="/owner/repo" ...> or <h2...><a href="/owner/repo">
    href_match <- regmatches(article, regexpr('href="(/[^/]+/[^"]+)"', article, perl = TRUE))
    if (length(href_match) == 0) next
    repo_path <- gsub('href="|"', "", href_match)
    repo_path <- trimws(repo_path)
    repo_url <- paste0("https://github.com", repo_path)
    repo_name <- gsub("^/", "", repo_path)
    # Extract description: <p class="...">...</p>
    desc_match <- regmatches(article, regexpr("<p[^>]*>([^<]*)</p>", article, perl = TRUE))
    desc <- if (length(desc_match) > 0) gsub("<[^>]+>", "", desc_match) else ""
    desc <- trimws(desc)
    # Extract stars today: "X stars today"
    stars_match <- regmatches(article, regexpr("[0-9,]+ stars today", article, perl = TRUE))
    stars_info <- if (length(stars_match) > 0) stars_match else ""
    text <- paste0(desc, if (nchar(stars_info) > 0) paste0(" (", stars_info, ")") else "")
    items[[length(items) + 1]] <- data.frame(
      title = repo_name, source = "GitHub Trending",
      url = repo_url, text = text, published_at = Sys.time(),
      item_type = "repo", stringsAsFactors = FALSE
    )
  }
  if (length(items) == 0) return(data.frame())
  dplyr::bind_rows(items)
}
