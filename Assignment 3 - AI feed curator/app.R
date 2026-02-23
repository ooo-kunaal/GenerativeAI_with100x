library(shiny)
library(dplyr)
library(htmltools)

# Source helper files
source("R/config.R")
source("R/feedback.R")
source("R/fetch_rss.R")
source("R/fetch_youtube.R")
source("R/fetch_github.R")
source("R/dedup.R")
source("R/prompt.R")
source("R/score_items.R")
source("R/logging.R")
source("R/email.R")

TOP_N <- 15
MUST_READ_THRESHOLD <- 8
FILTERED_THRESHOLD <- 5

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css")
  ),
  # Header
  tags$div(class = "opt-header",
           tags$div(class = "header-left",
                    tags$h1(class = "logo", "OPT"),
                    tags$span(class = "subtitle", "Learning Feed")
           ),
           tags$div(class = "header-right",
                    tags$button(id = "theme_toggle", class = "btn-icon", onclick = "toggleTheme()",
                                tags$i(class = "fa-solid fa-moon", id = "theme-icon"))
           )
  ),
  # Generate + Email buttons
  tags$div(class = "btn-row",
    actionButton("generate_btn", label = tagList(icon("bolt"), " Generate Digest"),
                 class = "btn-generate"),
    actionButton("email_btn", label = tagList(icon("envelope"), " Email Digest"),
                 class = "btn-email")
  ),
  # Settings toggle
  tags$button(id = "settings_toggle", class = "btn-settings-toggle",
              icon("gear"), " Settings"),
  # Collapsible settings panel
  tags$div(id = "settings_panel", class = "settings-panel",
           # Learning context
           tags$div(class = "settings-grid",
                    tags$div(class = "setting-group",
                             tags$label("Learning Goals"),
                             textAreaInput("learning_goals", label = NULL, rows = 3, width = "100%")
                    ),
                    tags$div(class = "setting-group",
                             tags$label("Project Context"),
                             textAreaInput("project_context", label = NULL, rows = 3, width = "100%")
                    )
           ),
           tags$div(class = "settings-grid",
                    tags$div(class = "setting-group",
                             tags$label("Learning Style"),
                             selectInput("learning_style", label = NULL, width = "100%",
                                         choices = c("Build First" = "build_first", "Theory First" = "theory_first", "Video Learner" = "video_learner"))
                    ),
                    tags$div(class = "setting-group",
                             tags$label("Depth Preference"),
                             selectInput("depth_preference", label = NULL, width = "100%",
                                         choices = c("Quick Wins" = "quick_wins", "Deep Dives" = "deep_dives", "Mixed" = "mixed"))
                    ),
                    tags$div(class = "setting-group",
                             tags$label("Content Format"),
                             selectInput("consumption_habits", label = NULL, width = "100%",
                                         choices = c("Mixed" = "mixed", "Long-form articles" = "long_form",
                                                     "Threads/short" = "threads", "Videos" = "videos"))
                    )
           ),
           # Skills
           tags$div(class = "settings-grid-4",
                    tags$div(class = "setting-group",
                             tags$label("MCP"),
                             selectInput("skill_MCP", label = NULL, width = "100%",
                                         choices = c("beginner","intermediate","advanced"))
                    ),
                    tags$div(class = "setting-group",
                             tags$label("RAG"),
                             selectInput("skill_RAG", label = NULL, width = "100%",
                                         choices = c("beginner","intermediate","advanced"))
                    ),
                    tags$div(class = "setting-group",
                             tags$label("LLM Fundamentals"),
                             selectInput("skill_LLM_fundamentals", label = NULL, width = "100%",
                                         choices = c("beginner","intermediate","advanced"))
                    ),
                    tags$div(class = "setting-group",
                             tags$label("Production"),
                             selectInput("skill_production_deployment", label = NULL, width = "100%",
                                         choices = c("beginner","intermediate","advanced"))
                    )
           ),
           # Sources management
           tags$div(class = "section-divider", "Sources"),
           uiOutput("sources_ui"),
           tags$div(class = "add-source-row",
                    selectInput("new_source_type", label = NULL, width = "120px",
                                choices = c("rss","youtube")),
                    textInput("new_source_name", label = NULL, placeholder = "Name", width = "180px"),
                    textInput("new_source_id", label = NULL, placeholder = "URL or Channel ID", width = "280px"),
                    actionButton("add_source_btn", icon("plus"), class = "btn-add-source")
           ),
           # Email settings
           tags$div(class = "section-divider", "Email Digest"),
           tags$div(class = "settings-grid",
             tags$div(class = "setting-group",
               tags$label("Email Address"),
               textInput("email_address", label = NULL, width = "100%", placeholder = "you@example.com")
             ),
             tags$div(class = "setting-group",
               tags$label("Daily Email"),
               selectInput("email_enabled", label = NULL, width = "100%",
                 choices = c("Enabled" = "TRUE", "Disabled" = "FALSE"))
             )
           ),
           # Save + precision
           tags$div(class = "settings-actions",
                    actionButton("save_settings", label = tagList(icon("floppy-disk"), " Save Settings"),
                                 class = "btn-save"),
                    uiOutput("precision_badge")
           )
  ),
  # Digest output
  uiOutput("digest_ui"),
  # Content viewer modal
  tags$div(id = "content-modal", class = "content-modal", onclick = "if(event.target===this)closeModal()",
           tags$div(class = "modal-container", id = "modal-container",
                    tags$div(class = "modal-header",
                             tags$span(id = "modal-title", class = "modal-title", ""),
                             tags$div(class = "modal-actions",
                                      tags$a(id = "modal-external", href = "#", target = "_blank", class = "btn-modal-action",
                                             icon("arrow-up-right-from-square"), " Open in source"),
                                      tags$button(class = "btn-modal-close", onclick = "closeModal()", icon("xmark"))
                             )
                    ),
                    tags$div(id = "modal-body", class = "modal-body")
           )
  ),
  # Footer
  tags$div(class = "opt-footer", "OPT \u2014 Update your goals in Settings to refine results."),
  # JS
  tags$script(HTML("
    $(document).on('click', '#settings_toggle', function() {
      $('#settings_panel').toggleClass('show');
      $(this).toggleClass('open');
    });
    Shiny.addCustomMessageHandler('vote', function(msg) {
      var btn = document.getElementById(msg.id);
      if (btn) btn.classList.add(msg.cls);
    });
    function getYouTubeId(url) {
      var m = url.match(/(?:youtube\\.com\\/watch\\?v=|youtu\\.be\\/)([a-zA-Z0-9_-]{11})/);
      return m ? m[1] : null;
    }
    function openModal(url, title, source) {
      var modal = document.getElementById('content-modal');
      var container = document.getElementById('modal-container');
      var body = document.getElementById('modal-body');
      document.getElementById('modal-title').textContent = title;
      document.getElementById('modal-external').href = url;
      container.className = 'modal-container';
      var ytId = getYouTubeId(url);
      if (ytId) {
        container.classList.add('modal-video');
        body.innerHTML = '<iframe src=\"https://www.youtube.com/embed/' + ytId +
          '?autoplay=1&rel=0\" class=\"yt-embed\" allow=\"autoplay; encrypted-media; picture-in-picture\" allowfullscreen></iframe>';
      } else {
        container.classList.add('modal-article');
        var icon = url.indexOf('github.com') !== -1 ? '\\uD83D\\uDCE6' :
                   url.indexOf('substack.com') !== -1 ? '\\u2709\\uFE0F' : '\\uD83D\\uDCF0';
        body.innerHTML = '<div class=\"article-preview\"><div class=\"preview-icon\">' + icon +
          '</div><div class=\"preview-source\">' + (source || '') +
          '</div><a href=\"' + url + '\" target=\"_blank\" class=\"btn-read-article\">' +
          '<i class=\"fa-solid fa-book-open\"></i> Read Article</a></div>';
      }
      modal.classList.add('open');
      document.body.style.overflow = 'hidden';
    }
    function closeModal() {
      document.getElementById('modal-body').innerHTML = '';
      document.getElementById('modal-container').className = 'modal-container';
      document.getElementById('content-modal').classList.remove('open');
      document.body.style.overflow = '';
    }
    document.addEventListener('keydown', function(e) { if (e.key === 'Escape') closeModal(); });
    // Theme toggle
    function toggleTheme() {
      document.body.classList.toggle('dark');
      var icon = document.getElementById('theme-icon');
      icon.className = document.body.classList.contains('dark') ? 'fa-solid fa-sun' : 'fa-solid fa-moon';
      localStorage.setItem('opt-theme', document.body.classList.contains('dark') ? 'dark' : 'light');
    }
    // Load saved theme
    (function() {
      if (localStorage.getItem('opt-theme') === 'dark') {
        document.body.classList.add('dark');
        var icon = document.getElementById('theme-icon');
        if (icon) icon.className = 'fa-solid fa-sun';
      }
    })();
  "))
)

# --- Server ---
server <- function(input, output, session) {
  rv <- reactiveValues(scored = NULL, feedback_given = list(), sources = NULL, topic_profile = NULL)
  
  # Init: load config + sources
  observe({
    # Always populate with defaults first
    cfg <- DEFAULT_CONFIG
    updateTextAreaInput(session, "learning_goals", value = cfg$learning_goals)
    updateTextAreaInput(session, "project_context", value = cfg$project_context)
    updateSelectInput(session, "learning_style", selected = cfg$learning_style)
    updateSelectInput(session, "depth_preference", selected = cfg$depth_preference)
    updateSelectInput(session, "consumption_habits", selected = cfg$consumption_habits %||% "mixed")
    updateSelectInput(session, "skill_MCP", selected = cfg$skill_MCP)
    updateSelectInput(session, "skill_RAG", selected = cfg$skill_RAG)
    updateSelectInput(session, "skill_LLM_fundamentals", selected = cfg$skill_LLM_fundamentals)
    updateSelectInput(session, "skill_production_deployment", selected = cfg$skill_production_deployment)
    updateTextInput(session, "email_address", value = cfg$email_address %||% "kunal86970@gmail.com")
    updateSelectInput(session, "email_enabled", selected = cfg$email_enabled %||% "TRUE")
    rv$sources <- DEFAULT_SOURCES
    # Try to overlay with Google Sheets data
    tryCatch({
      init_config_sheet()
      init_feedback_sheet()
      init_sources_sheet()
      cfg <- read_config()
      updateTextAreaInput(session, "learning_goals", value = cfg$learning_goals)
      updateTextAreaInput(session, "project_context", value = cfg$project_context)
      updateSelectInput(session, "learning_style", selected = cfg$learning_style)
      updateSelectInput(session, "depth_preference", selected = cfg$depth_preference)
      updateSelectInput(session, "consumption_habits", selected = cfg$consumption_habits %||% "mixed")
      updateSelectInput(session, "skill_MCP", selected = cfg$skill_MCP)
      updateSelectInput(session, "skill_RAG", selected = cfg$skill_RAG)
      updateSelectInput(session, "skill_LLM_fundamentals", selected = cfg$skill_LLM_fundamentals)
      updateSelectInput(session, "skill_production_deployment", selected = cfg$skill_production_deployment)
      updateTextInput(session, "email_address", value = cfg$email_address %||% "kunal86970@gmail.com")
      updateSelectInput(session, "email_enabled", selected = cfg$email_enabled %||% "TRUE")
      rv$sources <- read_sources()
      rv$topic_profile <- tryCatch(build_topic_profile(), error = function(e) NULL)
      log_info("INIT", "App initialized with Google Sheets data")
    }, error = function(e) {
      showNotification(paste("Google Sheets auth failed — using defaults:", e$message), type = "warning", duration = 5)
      log_error("INIT", e$message)
    })
  }) |> bindEvent(TRUE)
  
  # Save settings
  observeEvent(input$save_settings, {
    cfg <- list(
      learning_goals = input$learning_goals, project_context = input$project_context,
      learning_style = input$learning_style, depth_preference = input$depth_preference,
      consumption_habits = input$consumption_habits,
      skill_MCP = input$skill_MCP, skill_RAG = input$skill_RAG,
      skill_LLM_fundamentals = input$skill_LLM_fundamentals,
      skill_production_deployment = input$skill_production_deployment,
      email_address = input$email_address, email_enabled = input$email_enabled)
    ok <- write_config(cfg)
    showNotification(if (ok) "\u2705 Settings saved!" else "\u274C Failed to save.", type = "message", duration = 2)
  })
  
  # Sources UI
  output$sources_ui <- renderUI({
    sources <- rv$sources
    if (is.null(sources) || nrow(sources) == 0) return(tags$p(class = "text-muted", "No sources configured."))
    tags$div(class = "sources-list",
             lapply(seq_len(nrow(sources)), function(i) {
               s <- sources[i, ]
               tags$div(class = paste("source-row", if (!s$active) "inactive"),
                        tags$span(class = "source-type-badge", toupper(s$type)),
                        tags$span(class = "source-name", s$name),
                        tags$span(class = "source-id-preview", substr(s$identifier, 1, 40)),
                        actionButton(paste0("toggle_src_", i),
                                     label = if (s$active) icon("eye") else icon("eye-slash"),
                                     class = "btn-icon-sm"),
                        actionButton(paste0("rm_src_", i), label = icon("trash"), class = "btn-icon-sm btn-danger-sm")
               )
             })
    )
  })
  
  # Source toggle/remove observers
  observe({
    sources <- rv$sources
    if (is.null(sources)) return()
    for (i in seq_len(nrow(sources))) {
      local({
        idx <- i
        observeEvent(input[[paste0("toggle_src_", idx)]], {
          rv$sources$active[idx] <- !rv$sources$active[idx]
          write_sources(rv$sources)
        }, ignoreInit = TRUE)
        observeEvent(input[[paste0("rm_src_", idx)]], {
          rv$sources <- rv$sources[-idx, ]
          write_sources(rv$sources)
        }, ignoreInit = TRUE)
      })
    }
  })
  
  # Add source
  observeEvent(input$add_source_btn, {
    if (input$new_source_name == "" || input$new_source_id == "") {
      showNotification("Name and identifier required.", type = "warning"); return()
    }
    new_row <- data.frame(type = input$new_source_type, name = input$new_source_name,
                          identifier = input$new_source_id, active = TRUE, stringsAsFactors = FALSE)
    rv$sources <- rbind(rv$sources, new_row)
    write_sources(rv$sources)
    updateTextInput(session, "new_source_name", value = "")
    updateTextInput(session, "new_source_id", value = "")
    showNotification("\u2705 Source added!", type = "message", duration = 2)
  })
  
  # Precision badge
  output$precision_badge <- renderUI({
    tp <- rv$topic_profile
    if (is.null(tp) || is.na(tp$precision)) return(NULL)
    pct <- round(tp$precision * 100)
    cls <- if (pct >= 80) "prec-good" else if (pct >= 60) "prec-ok" else "prec-low"
    tags$div(class = paste("precision-badge", cls),
             tags$span(class = "prec-label", "Precision"),
             tags$span(class = "prec-value", paste0(pct, "%")),
             tags$span(class = "prec-count", paste0("(", tp$n_feedback, " ratings)"))
    )
  })
  
  # Generate digest
  observeEvent(input$generate_btn, {
    config <- list(
      learning_goals = input$learning_goals, project_context = input$project_context,
      learning_style = input$learning_style, depth_preference = input$depth_preference,
      consumption_habits = input$consumption_habits,
      skill_MCP = input$skill_MCP, skill_RAG = input$skill_RAG,
      skill_LLM_fundamentals = input$skill_LLM_fundamentals,
      skill_production_deployment = input$skill_production_deployment)
    rv$scored <- NULL
    rv$feedback_given <- list()
    start_time <- Sys.time()
    
    withProgress(message = "Generating digest...", value = 0, {
      sources_df <- rv$sources
      if (is.null(sources_df) || nrow(sources_df) == 0) sources_df <- DEFAULT_SOURCES
      # Step 1: Ingest
      setProgress(0.1, detail = "Fetching RSS feeds...")
      rss_items <- tryCatch(fetch_rss(sources_df), error = function(e) {
        log_warn("RSS", e$message); data.frame() })
      setProgress(0.3, detail = "Fetching YouTube...")
      yt_items <- tryCatch(fetch_youtube(sources_df), error = function(e) {
        log_warn("YOUTUBE", e$message); data.frame() })
      setProgress(0.4, detail = "Fetching GitHub Trending...")
      gh_items <- tryCatch(fetch_github_trending(), error = function(e) {
        log_warn("GITHUB", e$message); data.frame() })
      items <- dplyr::bind_rows(rss_items, yt_items, gh_items)
      n_fetched <- nrow(items)
      if (n_fetched == 0) {
        showNotification("No content found from any source.", type = "warning"); return()
      }
      setProgress(0.5, detail = paste0(n_fetched, " items. Deduplicating..."))
      items <- deduplicate(items)
      n_deduped <- nrow(items)
      
      # Step 2: Build topic profile from feedback
      setProgress(0.55, detail = "Loading feedback preferences...")
      tp <- tryCatch(build_topic_profile(), error = function(e) NULL)
      rv$topic_profile <- tp
      
      # Step 3: Score
      setProgress(0.6, detail = "Scoring with Gemini...")
      scored <- score_items(items, config, tp)
      n_scored <- nrow(scored)
      setProgress(0.9, detail = "Ranking...")
      scored <- scored[order(-scored$score), ]
      scored <- scored[scored$score >= FILTERED_THRESHOLD, ]
      if (nrow(scored) > TOP_N) scored <- scored[1:TOP_N, ]
      rv$scored <- scored
      n_filtered <- nrow(scored)
      duration <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      log_pipeline_summary(n_fetched, n_deduped, n_scored, n_filtered, duration)
      setProgress(1, detail = "Done!")
    })
  })
  
  # Email digest
  observeEvent(input$email_btn, {
    scored <- rv$scored
    if (is.null(scored) || nrow(scored) == 0) {
      showNotification("Generate a digest first.", type = "warning"); return()
    }
    config <- list(email_address = input$email_address, email_enabled = input$email_enabled)
    tp <- rv$topic_profile
    ok <- send_digest_email(scored, config, tp)
    showNotification(
      if (ok) "\u2705 Digest emailed!" else "\u274C Email failed. Check RESEND_API_KEY.",
      type = "message", duration = 3)
  })

  # Render digest
  output$digest_ui <- renderUI({
    scored <- rv$scored
    if (is.null(scored)) {
      return(tags$div(class = "empty-state",
                      tags$div(class = "icon", "\U0001F4E1"),
                      tags$h3("No digest yet"),
                      tags$p("Hit Generate Digest to fetch your personalized feed.")))
    }
    if (nrow(scored) == 0) {
      return(tags$div(class = "empty-state",
                      tags$div(class = "icon", "\U0001F50D"),
                      tags$h3("Nothing passed the filter"),
                      tags$p("Try updating your learning goals in Settings.")))
    }
    total <- nrow(scored)
    must_read <- scored[scored$score >= MUST_READ_THRESHOLD, ]
    worth_time <- scored[scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD, ]
    n_sources <- length(unique(scored$source))
    tp <- rv$topic_profile
    cards <- list(
      tags$div(class = "stats-bar",
               tags$span(icon("layer-group"), paste0(total, " items")),
               tags$span(icon("database"), paste0(n_sources, " sources")),
               tags$span(icon("calendar"), format(Sys.Date(), "%b %d, %Y")),
               if (!is.null(tp) && nchar(tp$summary) > 0)
                 tags$span(icon("brain"), "Personalized") else NULL
      ))
    if (nrow(must_read) > 0) {
      cards <- c(cards, list(tags$div(class = "section-label must-read",
                                      paste0("\U0001F525 MUST READ (", nrow(must_read), ")"))))
      for (i in seq_len(nrow(must_read)))
        cards <- c(cards, list(build_item_card(must_read[i, ], paste0("mr_", i))))
    }
    if (nrow(worth_time) > 0) {
      cards <- c(cards, list(tags$div(class = "section-label worth-time",
                                      paste0("\U0001F4DA WORTH YOUR TIME (", nrow(worth_time), ")"))))
      for (i in seq_len(nrow(worth_time)))
        cards <- c(cards, list(build_item_card(worth_time[i, ], paste0("wt_", i))))
    }
    tagList(cards)
  })
  
  # Feedback observers
  observe({
    scored <- rv$scored
    if (is.null(scored)) return()
    all_ids <- c(
      if (any(scored$score >= MUST_READ_THRESHOLD))
        paste0("mr_", seq_len(sum(scored$score >= MUST_READ_THRESHOLD))),
      if (any(scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD))
        paste0("wt_", seq_len(sum(scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD))))
    for (id in all_ids) {
      local({
        local_id <- id
        observeEvent(input[[paste0("up_", local_id)]], {
          handle_feedback(local_id, "useful", rv, session)
        }, ignoreInit = TRUE, once = TRUE)
        observeEvent(input[[paste0("down_", local_id)]], {
          handle_feedback(local_id, "not_useful", rv, session)
        }, ignoreInit = TRUE, once = TRUE)
      })
    }
  })
}

# --- Build item card ---
build_item_card <- function(row, card_id) {
  score <- row$score
  score_class <- if (score >= 8) "score-high" else if (score >= 6) "score-mid" else "score-low"
  type_icon <- switch(as.character(row$item_type),
                      video = "\U0001F4FA", repo = "\U0001F4E6", tweet = "\U0001F426", "\U0001F4F0")
  safe_title <- gsub("'", "\\\\'", row$title)
  safe_source <- gsub("'", "\\\\'", row$source)
  tags$div(class = "item-card",
           tags$div(class = "item-top",
                    tags$div(class = "item-title-wrap",
                             tags$a(class = "item-title", href = "#",
                                    onclick = paste0("event.preventDefault();openModal('", row$url, "','", safe_title, "','", safe_source, "')"),
                                    row$title),
                             tags$div(class = "item-meta",
                                      tags$span(class = "source-icon", type_icon),
                                      tags$span(row$source))
                    ),
                    tags$div(style = "display:flex;align-items:center;gap:8px;",
                             tags$span(class = paste("score-badge", score_class), paste0(score, "/10")),
                             tags$div(class = "feedback-wrap",
                                      actionButton(paste0("up_", card_id), label = NULL, icon = icon("thumbs-up"), class = "btn-feedback"),
                                      actionButton(paste0("down_", card_id), label = NULL, icon = icon("thumbs-down"), class = "btn-feedback"))
                    )
           ),
           tags$div(class = "item-why", paste0("\u2192 ", row$justification))
  )
}

# --- Handle feedback ---
handle_feedback <- function(card_id, feedback_type, rv, session) {
  if (!is.null(rv$feedback_given[[card_id]])) return()
  scored <- rv$scored
  if (is.null(scored)) return()
  parts <- strsplit(card_id, "_")[[1]]
  prefix <- parts[1]; idx <- as.integer(parts[2])
  subset <- if (prefix == "mr") scored[scored$score >= MUST_READ_THRESHOLD, ]
  else scored[scored$score >= FILTERED_THRESHOLD & scored$score < MUST_READ_THRESHOLD, ]
  if (idx > nrow(subset)) return()
  row <- subset[idx, ]
  log_feedback(row$title, row$url, row$source, row$score, row$justification, feedback_type)
  rv$feedback_given[[card_id]] <- feedback_type
  btn_id <- if (feedback_type == "useful") paste0("up_", card_id) else paste0("down_", card_id)
  vote_class <- if (feedback_type == "useful") "voted-up" else "voted-down"
  session$sendCustomMessage("vote", list(id = btn_id, cls = vote_class))
  showNotification(
    if (feedback_type == "useful") "\u2705 Marked useful" else "\u274C Marked not useful",
    type = "message", duration = 2)
}

shinyApp(ui, server)
