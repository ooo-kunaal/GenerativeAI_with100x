# Deduplication: URL-based + fuzzy title matching
library(dplyr)

deduplicate <- function(items) {
  if (nrow(items) == 0) return(items)
  # Phase 1: Exact URL dedup
  items$url_normalized <- tolower(trimws(gsub("/$", "", items$url)))
  deduped <- items[!duplicated(items$url_normalized), ]
  url_removed <- nrow(items) - nrow(deduped)
  # Phase 2: Fuzzy title dedup (catch same story across sources)
  if (nrow(deduped) > 1) {
    titles_norm <- tolower(gsub("[^a-z0-9 ]", "", deduped$title))
    keep <- rep(TRUE, nrow(deduped))
    for (i in 2:nrow(deduped)) {
      if (!keep[i]) next
      for (j in 1:(i - 1)) {
        if (!keep[j]) next
        sim <- title_similarity(titles_norm[i], titles_norm[j])
        if (sim > 0.7) {
          # Keep the one with longer text (more detail)
          if (nchar(deduped$text[i]) >= nchar(deduped$text[j])) keep[j] <- FALSE
          else { keep[i] <- FALSE; break }
        }
      }
    }
    fuzzy_removed <- sum(!keep)
    deduped <- deduped[keep, ]
  } else { fuzzy_removed <- 0 }
  deduped$url_normalized <- NULL
  total_removed <- url_removed + fuzzy_removed
  if (total_removed > 0) message("  Removed ", total_removed, " duplicate(s) (",
                                 url_removed, " URL, ", fuzzy_removed, " fuzzy)")
  deduped
}

# Jaccard similarity on word sets
title_similarity <- function(a, b) {
  words_a <- unique(unlist(strsplit(a, "\\s+")))
  words_b <- unique(unlist(strsplit(b, "\\s+")))
  words_a <- words_a[nchar(words_a) > 2]
  words_b <- words_b[nchar(words_b) > 2]
  if (length(words_a) == 0 || length(words_b) == 0) return(0)
  inter <- length(intersect(words_a, words_b))
  union <- length(union(words_a, words_b))
  inter / union
}