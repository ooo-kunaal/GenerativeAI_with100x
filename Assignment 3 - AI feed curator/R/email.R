# Email digest via Resend API
library(httr2)

RESEND_API_KEY <- Sys.getenv("RESEND_API_KEY", "")
RESEND_FROM <- "OPT Digest <onboarding@resend.dev>"

send_digest_email <- function(scored_df, config, topic_profile = NULL) {
  to <- config$email_address %||% ""
  if (to == "") { message("No email address configured"); return(FALSE) }
  api_key <- RESEND_API_KEY
  if (api_key == "") { message("RESEND_API_KEY not set"); return(FALSE) }
  html <- compose_digest_html(scored_df, topic_profile)
  date_str <- format(Sys.Date(), "%b %d, %Y")
  subject <- paste0("\U0001F4E1 OPT Digest — ", date_str,
    " (", nrow(scored_df), " items)")
  tryCatch({
    resp <- request("https://api.resend.com/emails") |>
      req_headers(Authorization = paste("Bearer", api_key),
                  `Content-Type` = "application/json") |>
      req_body_json(list(from = RESEND_FROM, to = list(to),
                         subject = subject, html = html)) |>
      req_timeout(15) |> req_perform()
    status <- resp_status(resp)
    if (status >= 200 && status < 300) {
      message("\u2713 Digest emailed to ", to)
      TRUE
    } else {
      message("\u26A0 Resend returned status ", status)
      FALSE
    }
  }, error = function(e) {
    message("\u26A0 Email send failed: ", e$message)
    FALSE
  })
}

compose_digest_html <- function(scored_df, topic_profile = NULL) {
  must_read <- scored_df[scored_df$score >= 8, ]
  worth_time <- scored_df[scored_df$score >= 5 & scored_df$score < 8, ]
  n_total <- nrow(scored_df)
  n_sources <- length(unique(scored_df$source))
  date_str <- format(Sys.Date(), "%B %d, %Y")
  # Precision info
  prec_line <- ""
  if (!is.null(topic_profile) && !is.na(topic_profile$precision)) {
    pct <- round(topic_profile$precision * 100)
    prec_line <- paste0('<span style="color:#6B6B6B;font-size:12px;"> &bull; Precision: ', pct, '%</span>')
  }
  # Build item rows
  item_html <- function(row) {
    score_color <- if (row$score >= 8) "#2E7D32" else if (row$score >= 6) "#C17A1A" else "#6B6B6B"
    score_bg <- if (row$score >= 8) "#E8F5E9" else if (row$score >= 6) "#FFF8E1" else "#F5F5F5"
    type_icon <- switch(as.character(row$item_type), video = "\U0001F4FA", repo = "\U0001F4E6", "\U0001F4F0")
    paste0(
      '<tr><td style="padding:12px 16px;border-bottom:1px solid #E8E5E0;">',
      '<div style="display:flex;justify-content:space-between;align-items:flex-start;">',
      '<div style="flex:1;">',
      '<a href="', row$url, '" style="color:#1A1A1A;font-weight:600;font-size:14px;text-decoration:none;">',
      htmltools::htmlEscape(row$title), '</a>',
      '<div style="color:#9B9B9B;font-size:12px;margin-top:3px;">', type_icon, ' ', htmltools::htmlEscape(row$source), '</div>',
      '</div>',
      '<span style="background:', score_bg, ';color:', score_color,
      ';font-size:11px;font-weight:700;padding:3px 8px;border-radius:4px;white-space:nowrap;margin-left:12px;">',
      row$score, '/10</span>',
      '</div>',
      '<div style="color:#6B6B6B;font-size:13px;margin-top:6px;font-style:italic;">',
      '\u2192 ', htmltools::htmlEscape(row$justification), '</div>',
      '</td></tr>')
  }
  # Sections
  must_read_section <- ""
  if (nrow(must_read) > 0) {
    rows <- paste(apply(must_read, 1, item_html), collapse = "")
    must_read_section <- paste0(
      '<tr><td style="padding:20px 16px 8px;font-size:11px;font-weight:700;color:#C62828;',
      'text-transform:uppercase;letter-spacing:1px;">',
      '\U0001F525 MUST READ (', nrow(must_read), ')</td></tr>', rows)
  }
  worth_section <- ""
  if (nrow(worth_time) > 0) {
    rows <- paste(apply(worth_time, 1, item_html), collapse = "")
    worth_section <- paste0(
      '<tr><td style="padding:20px 16px 8px;font-size:11px;font-weight:700;color:#C17A1A;',
      'text-transform:uppercase;letter-spacing:1px;">',
      '\U0001F4DA WORTH YOUR TIME (', nrow(worth_time), ')</td></tr>', rows)
  }
  paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width,initial-scale=1"></head>',
    '<body style="margin:0;padding:0;background:#FAF9F7;font-family:-apple-system,BlinkMacSystemFont,',
    '\'Segoe UI\',sans-serif;">',
    '<table width="100%" cellpadding="0" cellspacing="0" style="max-width:600px;margin:0 auto;background:#FFFFFF;">',
    # Header
    '<tr><td style="padding:24px 20px;border-bottom:1px solid #E8E5E0;">',
    '<span style="font-size:20px;font-weight:700;color:#C96442;">OPT</span>',
    '<span style="color:#9B9B9B;font-size:13px;margin-left:8px;">Learning Feed</span>',
    '</td></tr>',
    # Stats
    '<tr><td style="padding:14px 16px;font-size:12px;color:#9B9B9B;">',
    '\U0001F4C5 ', date_str, ' &bull; ', n_total, ' items &bull; ', n_sources, ' sources',
    prec_line, '</td></tr>',
    # Content
    must_read_section, worth_section,
    # Footer
    '<tr><td style="padding:20px 16px;text-align:center;font-size:11px;color:#9B9B9B;border-top:1px solid #E8E5E0;">',
    'Sent by OPT \u2014 Update your preferences in the app to refine results.',
    '</td></tr>',
    '</table></body></html>')
}
