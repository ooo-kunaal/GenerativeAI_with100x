# Build scoring prompt for Gemini — with feedback-driven topic learning
build_scoring_prompt <- function(items_df, config, topic_profile = NULL) {
  skills <- get_skill_levels(config)
  skill_lines <- paste(paste0("  - ", names(skills), ": ", unlist(skills)), collapse = "\n")
  items_block <- build_items_block(items_df)
  # Feedback learning section
  feedback_section <- ""
  if (!is.null(topic_profile) && nchar(topic_profile$summary) > 0) {
    feedback_section <- paste0(
      "\nFEEDBACK-BASED PREFERENCES (learned from ", topic_profile$n_feedback, " past ratings):\n",
      topic_profile$summary, "\n",
      "Use these preferences to ADJUST scores: boost items matching liked topics, ",
      "penalize items matching disliked topics. Weight this signal at ~20% of final score.\n")
  }
  paste0(
    "You are a personalized learning content curator.\n\n",
    "USER CONTEXT:\n",
    "- Goals: ", config$learning_goals, "\n",
    "- Project: ", config$project_context, "\n",
    "- Skill Levels:\n", skill_lines, "\n",
    "- Learning Style: ", config$learning_style, "\n",
    "- Depth Preference: ", config$depth_preference, "\n",
    "- Content Format Preference: ", config$consumption_habits %||% "mixed", "\n",
    feedback_section, "\n",
    "CONTENT ITEMS TO SCORE:\n", items_block, "\n\n",
    "For EACH item, score relevance 0-10 and write exactly one sentence explaining ",
    "why it's relevant (or not) to this user's goals. Use \"you/your\" to make it personal.\n\n",
    "Scoring weights:\n",
    "- Relevance to goals & project (40%)\n",
    "- Feedback-learned preferences (20%)\n",
    "- Skill level alignment (15%)\n",
    "- Learning style & format match (15%)\n",
    "- Actionability (10%)\n\n",
    "Rules:\n",
    "- Borderline items (score 5-6): include them, let user decide\n",
    "- Don't penalize recent content\n",
    "- Be honest \u2014 score 0-2 if truly irrelevant\n",
    "- If feedback preferences conflict with explicit goals, prioritize goals\n\n",
    "CRITICAL: Respond with ONLY a raw JSON array. No markdown, no commentary.\n",
    "Example: [{\"index\": 0, \"score\": 8, \"justification\": \"...\"}]")
}

build_items_block <- function(items_df) {
  lines <- vapply(seq_len(nrow(items_df)), function(i) {
    row <- items_df[i, ]
    text_preview <- if (nchar(row$text) > 300) paste0(substr(row$text, 1, 300), "...") else row$text
    paste0("[", i - 1, "] Title: ", row$title, "\n",
           "    Source: ", row$source, "\n",
           "    Type: ", row$item_type, "\n",
           "    Description: ", text_preview)
  }, character(1))
  paste(lines, collapse = "\n\n")
}