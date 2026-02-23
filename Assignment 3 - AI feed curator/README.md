# OPT — AI-Powered Learning Feed Curator

A personalized AI learning feed curator built with R Shiny. Aggregates content from RSS feeds and YouTube, scores items against your learning context using Gemini LLM, and delivers a ranked daily digest via email.

## How It Works

1. **Ingest** — Fetches recent content from RSS feeds (Ben's Bites, Latent Space, Epoch AI, etc.) and YouTube channels (AI Engineer, Andrej Karpathy, Dwarkesh Podcast) with a 48-hour lookback window
2. **Deduplicate** — URL-based + fuzzy title matching (Jaccard similarity, threshold 0.7)
3. **Score** — Sends all items to Gemini 2.5 Flash via `ellmer`'s structured output extraction. Scores 0–10 based on your learning goals, skill levels, and preferences. Past feedback refines scoring through topic profiles and source reliability multipliers
4. **Deliver** — Tiered display in Shiny dashboard (Must Read ≥8, Worth Your Time 5–7) with feedback buttons. Daily HTML email via Resend API, automated by GitHub Actions

## Quick Start

```bash
# 1. Install R packages (run once)
install.packages(c("shiny", "bslib", "googlesheets4", "gargle", "httr2",
                    "jsonlite", "dplyr", "tidyRSS", "htmltools", "ellmer"))

# 2. Create .Renviron in project root
writeLines(c(
  "GEMINI_API_KEY=your_gemini_key",
  "YOUTUBE_API_KEY=your_youtube_key",
  "RESEND_API_KEY=your_resend_key"
), ".Renviron")

# 3. Place service-account.json in project root
# (GCP service account with Google Sheets API access)

# 4. Run the app
shiny::runApp()
```

## Project Structure

```
Assignment 3 - AI feed curator/
├── app.R                     # Shiny app (UI + server)
├── send_digest.R             # Headless pipeline for GitHub Actions
├── service-account.json      # GCP service account key (gitignored)
├── .Renviron                 # API keys for local dev (gitignored)
├── .gitignore
├── www/styles.css            # Custom light/dark theme
├── R/
│   ├── config.R              # Google Sheets config + sources management
│   ├── feedback.R            # Feedback logging + topic profile builder
│   ├── fetch_rss.R           # RSS ingestion (tidyRSS, 48h lookback)
│   ├── fetch_youtube.R       # YouTube Data API v3 (publishedAfter filter)
│   ├── dedup.R               # URL + fuzzy title deduplication
│   ├── prompt.R              # Gemini scoring prompt builder
│   ├── score_items.R         # ellmer structured scoring + source reliability
│   ├── email.R               # Resend API email composition + sending
│   └── logging.R             # Structured logging
└── .github/workflows/
    └── daily-digest.yml      # Cron: 2:30 AM UTC (8:00 AM IST) daily
```

## APIs & Services

| Service | Purpose | Auth |
|---------|---------|------|
| Google Sheets | Config, feedback, sources persistence | Service account JSON |
| Gemini 2.5 Flash | LLM content scoring via ellmer | API key (env var) |
| YouTube Data API v3 | Video ingestion | API key (env var) |
| Resend | Daily digest email | API key (env var) |

## Google Sheets

[Spreadsheet](https://docs.google.com/spreadsheets/d/1h7FIx1zUNt_XgKuy5RMRo_ZjJzmMjyaX8GkaM8Qqp3c/edit)

Auto-created sheets on first run:
- **Config** — Learning goals, skill levels, preferences (key-value pairs)
- **Feedback** — Append-only log of useful/not-useful clicks with timestamps
- **Sources** — Dynamic source list (type, name, identifier, active toggle)

## GitHub Actions (Daily Email)

The workflow at `.github/workflows/daily-digest.yml` runs `send_digest.R` daily at 8 AM IST.

**Required repository secrets** (Settings → Secrets → Actions):
- `GEMINI_API_KEY`
- `YOUTUBE_API_KEY`
- `RESEND_API_KEY`
- `GOOGLE_SA_JSON` (full contents of service-account.json)

The workflow installs R + dependencies, writes the service account JSON from secrets, sets API keys as env vars, and runs the headless pipeline.

## Feedback Learning

User feedback drives scoring refinement:
- Topic profiles built from tokenized titles/justifications of liked vs disliked items
- Source reliability uses Bayesian smoothing: `(useful + 1) / (total + 2)`
- Precision monitoring tracks overall, recent-20, and per-source accuracy
- Feedback preferences injected into scoring prompt with 20% weight

## License

Academic project for 100x Engineers Generative AI course.
