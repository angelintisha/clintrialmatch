# ================================================================
#  ClinTrialMatch – Clinical Trial Site Recommender
#  BANA 6780 | AI for Business
#  Pure R scoring — no API needed
# ================================================================

library(shiny)

# ── Site Database ─────────────────────────────────────────────
sites <- data.frame(
  id   = paste0("SITE-", sprintf("%02d", 1:20)),
  name = c(
    "MD Anderson Cancer Center",
    "Mayo Clinic – Rochester",
    "Johns Hopkins Medical Center",
    "Massachusetts General Hospital",
    "UCSF Medical Center",
    "Duke University Medical Center",
    "Cleveland Clinic",
    "Stanford University Medical Center",
    "Memorial Sloan Kettering",
    "UCLA Medical Center",
    "Emory University Hospital",
    "University of Michigan Health",
    "Northwestern Memorial Hospital",
    "Vanderbilt University Medical Center",
    "University of Pittsburgh Medical Center",
    "Cedars-Sinai Medical Center",
    "NYU Langone Health",
    "University of Colorado Anschutz",
    "University of Washington Medical Center",
    "Penn Medicine"
  ),
  region = c(
    "South","Midwest","East","East","West",
    "East","Midwest","West","East","West",
    "South","Midwest","Midwest","South","East",
    "West","East","West","West","East"
  ),
  phases = c(
    "I,II,III","I,II,III,IV","I,II,III","I,II,III","I,II,III",
    "I,II,III,IV","II,III,IV","I,II,III","II,III","II,III,IV",
    "I,II,III","II,III,IV","I,II,III,IV","I,II,III","II,III",
    "II,III","II,III","I,II,III","I,II,III","I,II,III,IV"
  ),
  ta = c(
    "Oncology",
    "Oncology,Cardiology,Neurology,Rare Disease",
    "Oncology,Immunology,Rare Disease",
    "Cardiology,Neurology,Rare Disease",
    "Oncology,Neurology,Rare Disease",
    "Oncology,Cardiology,Immunology",
    "Cardiology,Metabolic,Neurology",
    "Oncology,Metabolic,Immunology",
    "Oncology",
    "Oncology,Neurology,Metabolic",
    "Oncology,Immunology,Metabolic",
    "Cardiology,Neurology,Metabolic",
    "Neurology,Cardiology,Rare Disease",
    "Metabolic,Oncology,Immunology",
    "Cardiology,Oncology,Neurology",
    "Oncology,Cardiology,Metabolic",
    "Neurology,Immunology,Rare Disease",
    "Oncology,Immunology,Rare Disease",
    "Oncology,Immunology,Metabolic",
    "Oncology,Cardiology,Immunology"
  ),
  enrollment_rate     = c(95,92,88,89,90,87,85,91,93,86,83,88,87,90,84,85,86,89,88,91),
  screen_failure_rate = c(14,18,22,21,20,19,25,17,15,24,28,22,21,19,26,23,24,18,20,17),
  competing_trials    = c(2,3,5,2,3,4,4,3,2,3,6,3,3,2,5,4,4,3,3,4),
  pi_experience       = c(98,95,92,93,94,90,88,96,97,87,85,91,90,93,86,88,89,92,91,94),
  infrastructure      = c(97,95,92,93,94,90,88,96,96,87,82,91,90,93,86,87,89,92,91,94),
  stringsAsFactors    = FALSE
)

# ── Scoring function ──────────────────────────────────────────
get_recommendations <- function(phase, ta, region, urgency) {

  n <- nrow(sites)

  # Step 1: Content filtering — must match phase AND therapeutic area
  phase_ok <- sapply(sites$phases, function(p) grepl(phase, p, fixed = TRUE))
  ta_ok    <- sapply(sites$ta,     function(t) grepl(ta,    t, fixed = TRUE))
  qualifies <- phase_ok & ta_ok

  # Step 2: Similarity scoring across 4 dimensions (0-100 each)
  s_enroll <- sites$enrollment_rate                        # higher = better
  s_sf     <- 100 - sites$screen_failure_rate              # lower fail = better
  s_pi     <- sites$pi_experience                          # higher = better
  s_infra  <- sites$infrastructure                         # higher = better

  # Step 3: Enrollment weight increases if urgency is Accelerated
  w_enroll <- ifelse(urgency == "Accelerated", 0.35, 0.20)
  w_sf     <- 0.25
  w_pi     <- 0.25
  w_infra  <- 1 - w_enroll - w_sf - w_pi

  composite <- w_enroll * s_enroll +
               w_sf     * s_sf     +
               w_pi     * s_pi     +
               w_infra  * s_infra

  # Step 4: Region bonus — collaborative filtering proxy
  region_bonus <- ifelse(region == "No Preference" | sites$region == region, 5, 0)

  composite <- composite + region_bonus

  # Step 5: Sites that fail hard filter get capped at 25
  composite <- ifelse(qualifies, composite, pmin(composite, 25))

  # Clamp to 0-100
  composite <- pmin(100, pmax(0, composite))

  sites$score      <- round(composite)
  sites$qualifies  <- qualifies

  # Return top 3 qualifying sites by score
  qualifying <- sites[sites$qualifies == TRUE, ]
  qualifying <- qualifying[order(-qualifying$score), ]

  if (nrow(qualifying) == 0) return(NULL)

  head(qualifying, 3)
}

# ── Helpers ───────────────────────────────────────────────────
score_color <- function(score) {
  if      (score >= 85) "#16a34a"
  else if (score >= 70) "#0077b6"
  else if (score >= 55) "#d97706"
  else                  "#dc2626"
}

strength_text <- function(s) {
  if      (s$enrollment_rate >= 92)    paste0("Exceptional enrollment rate of ", s$enrollment_rate, "%")
  else if (s$screen_failure_rate <= 17) paste0("Very low screen failure rate (", s$screen_failure_rate, "%)")
  else if (s$competing_trials   <= 2)  "Minimal competing trial burden"
  else if (s$pi_experience      >= 95) paste0("Highly experienced PI team (", s$pi_experience, "/100)")
  else                                  paste0("Strong infrastructure score (", s$infrastructure, "/100)")
}

caution_text <- function(s) {
  if      (s$competing_trials    >= 5)  paste0(s$competing_trials, " active competing trials may strain site capacity")
  else if (s$screen_failure_rate >= 23) paste0("Screen failure rate of ", s$screen_failure_rate, "% may slow enrollment")
  else if (s$enrollment_rate     <  87) paste0("Enrollment rate of ", s$enrollment_rate, "% is below network average")
  else                                  "No major red flags identified for this trial profile"
}

# ── UI ────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$title("ClinTrialMatch"),
    tags$link(rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("
      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

      body {
        font-family: 'Inter', sans-serif;
        background: #f0f4f8;
        color: #1a202c;
        min-height: 100vh;
      }

      /* Header */
      .ctm-header {
        background: #0a2540;
        padding: 0 40px;
        height: 64px;
        display: flex;
        align-items: center;
        justify-content: space-between;
      }
      .ctm-logo { display: flex; align-items: center; gap: 12px; }
      .ctm-logo-mark {
        width: 34px; height: 34px;
        background: linear-gradient(135deg, #00b4d8, #0077b6);
        border-radius: 8px;
        display: flex; align-items: center; justify-content: center;
        font-size: 16px; font-weight: 700; color: #fff;
      }
      .ctm-logo-name {
        font-size: 17px; font-weight: 600; color: #fff; letter-spacing: -0.3px;
      }
      .ctm-logo-name span { color: #00b4d8; }
      .ctm-logo-sub  { font-size: 11px; color: #64748b; margin-top: 1px; }
      .ctm-badge {
        font-family: 'JetBrains Mono', monospace;
        font-size: 10px; color: #475569;
        background: #0f3460; padding: 4px 10px;
        border-radius: 4px; letter-spacing: 0.5px;
      }

      /* Layout */
      .ctm-body {
        display: grid;
        grid-template-columns: 300px 1fr;
        min-height: calc(100vh - 64px);
      }

      /* Sidebar */
      .ctm-sidebar {
        background: #fff;
        border-right: 1px solid #e2e8f0;
        padding: 24px 20px 32px;
      }
      .sidebar-heading {
        font-size: 10px; font-weight: 600;
        letter-spacing: 1.5px; text-transform: uppercase;
        color: #94a3b8; margin-bottom: 18px;
        padding-bottom: 8px; border-bottom: 1px solid #e2e8f0;
      }
      .field-group { margin-bottom: 16px; }
      .field-label {
        display: block; font-size: 12px; font-weight: 500;
        color: #475569; margin-bottom: 5px;
      }
      select.ctm-select {
        width: 100%;
        border: 1.5px solid #e2e8f0; border-radius: 8px;
        font-family: 'Inter', sans-serif;
        font-size: 13px; color: #1a202c;
        padding: 9px 12px; background: #f8fafc;
        appearance: none; -webkit-appearance: none; outline: none;
        transition: border-color 0.15s;
        cursor: pointer;
      }
      select.ctm-select:focus {
        border-color: #0077b6;
        box-shadow: 0 0 0 3px rgba(0,119,182,0.12);
        background: #fff;
      }
      select.ctm-select.unselected { color: #94a3b8; }

      .ctm-run-btn {
        width: 100%; margin-top: 20px;
        background: linear-gradient(135deg, #0077b6, #0096c7);
        color: #fff; border: none; border-radius: 10px;
        font-family: 'Inter', sans-serif;
        font-size: 14px; font-weight: 600;
        padding: 13px 0; cursor: pointer;
        box-shadow: 0 4px 14px rgba(0,119,182,0.3);
        transition: opacity 0.15s, transform 0.1s;
      }
      .ctm-run-btn:hover  { opacity: 0.9; }
      .ctm-run-btn:active { transform: scale(0.98); }

      /* Main panel */
      .ctm-main { padding: 28px 32px; background: #f0f4f8; }

      /* Idle / error states */
      .state-box {
        display: flex; flex-direction: column;
        align-items: center; justify-content: center;
        min-height: 380px; gap: 12px;
        color: #94a3b8; text-align: center;
      }
      .state-icon { font-size: 48px; opacity: 0.5; }
      .state-msg  { font-size: 14px; max-width: 280px; line-height: 1.6; }
      .state-err  { color: #dc2626; font-size: 14px; font-weight: 500; }

      /* Results */
      .results-bar {
        display: flex; align-items: center;
        justify-content: space-between; margin-bottom: 20px;
      }
      .results-title {
        font-size: 19px; font-weight: 700;
        color: #0a2540; letter-spacing: -0.3px;
      }
      .results-meta {
        font-family: 'JetBrains Mono', monospace;
        font-size: 11px; color: #64748b;
        background: #e2e8f0; padding: 4px 10px; border-radius: 4px;
      }

      /* Site card */
      .site-card {
        background: #fff; border: 1px solid #e2e8f0;
        border-radius: 14px; padding: 22px 24px;
        margin-bottom: 14px; position: relative;
      }
      .card-rank {
        position: absolute; top: 18px; right: 20px;
        font-family: 'JetBrains Mono', monospace;
        font-size: 10px; font-weight: 600;
        letter-spacing: 1px; text-transform: uppercase;
      }
      .rank-1 { color: #b45309; }
      .rank-2 { color: #6b7280; }
      .rank-3 { color: #92400e; }

      .card-id   { font-family: 'JetBrains Mono', monospace; font-size: 10px; color: #94a3b8; margin-bottom: 3px; }
      .card-name { font-size: 16px; font-weight: 700; color: #0a2540; margin-bottom: 10px; }

      /* Pills */
      .card-pills { display: flex; gap: 6px; flex-wrap: wrap; margin-bottom: 14px; }
      .pill {
        font-size: 10px; font-weight: 500;
        padding: 3px 9px; border-radius: 99px; letter-spacing: 0.2px;
      }
      .pill-region { background: #ede9fe; color: #5b21b6; }
      .pill-ta     { background: #e0f2fe; color: #0369a1; }
      .pill-phase  { background: #fce7f3; color: #9d174d; }

      /* Score bar */
      .score-row { display: flex; align-items: center; gap: 10px; margin-bottom: 14px; }
      .score-lbl { font-size: 11px; font-weight: 500; color: #64748b; width: 82px; flex-shrink: 0; }
      .score-track { flex: 1; height: 8px; background: #e2e8f0; border-radius: 99px; overflow: hidden; }
      .score-fill  { height: 100%; border-radius: 99px; }
      .score-num   { font-family: 'JetBrains Mono', monospace; font-size: 13px; font-weight: 600; width: 42px; text-align: right; }

      /* Metrics */
      .card-metrics {
        display: grid; grid-template-columns: repeat(4, 1fr);
        gap: 8px; margin-bottom: 14px;
      }
      .metric {
        background: #f8fafc; border: 1px solid #e2e8f0;
        border-radius: 8px; padding: 8px; text-align: center;
      }
      .metric-val { font-family: 'JetBrains Mono', monospace; font-size: 14px; font-weight: 600; color: #0a2540; }
      .metric-lbl { font-size: 9px; color: #94a3b8; text-transform: uppercase; letter-spacing: 0.8px; margin-top: 2px; }

      /* Narrative */
      .card-narrative {
        font-size: 13px; color: #475569; line-height: 1.6;
        border-top: 1px solid #f1f5f9; padding-top: 12px;
      }
      .ntag {
        display: inline-block; font-size: 9px; font-weight: 700;
        letter-spacing: 1px; text-transform: uppercase;
        padding: 2px 7px; border-radius: 4px; margin-right: 5px; vertical-align: middle;
      }
      .ntag-s { background: #dcfce7; color: #166534; }
      .ntag-w { background: #fef9c3; color: #854d0e; }

      @media (max-width: 720px) {
        .ctm-body { grid-template-columns: 1fr; }
        .card-metrics { grid-template-columns: repeat(2, 1fr); }
        .ctm-main { padding: 16px; }
      }
    "))
  ),

  # Header
  div(class = "ctm-header",
    div(class = "ctm-logo",
      div(class = "ctm-logo-mark", "C"),
      div(
        div(class = "ctm-logo-name", "Clin", tags$span("Trial"), "Match"),
        div(class = "ctm-logo-sub",  "Investigator Site Recommendation Engine")
      )
    ),
    div(class = "ctm-badge", "BANA 6780 · AI for Business")
  ),

  div(class = "ctm-body",

    # Sidebar
    div(class = "ctm-sidebar",
      div(class = "sidebar-heading", "Trial Parameters"),

      div(class = "field-group",
        tags$label(class = "field-label", "Trial Phase"),
        tags$select(id = "phase", class = "ctm-select unselected",
          tags$option("-- Select phase --", value = ""),
          tags$option("Phase I",            value = "I"),
          tags$option("Phase II",           value = "II"),
          tags$option("Phase III",          value = "III"),
          tags$option("Phase IV",           value = "IV")
        )
      ),

      div(class = "field-group",
        tags$label(class = "field-label", "Therapeutic Area"),
        tags$select(id = "ta", class = "ctm-select unselected",
          tags$option("-- Select area --",  value = ""),
          tags$option("Oncology",           value = "Oncology"),
          tags$option("Cardiology",         value = "Cardiology"),
          tags$option("Neurology",          value = "Neurology"),
          tags$option("Immunology",         value = "Immunology"),
          tags$option("Metabolic",          value = "Metabolic"),
          tags$option("Rare Disease",       value = "Rare Disease")
        )
      ),

      div(class = "field-group",
        tags$label(class = "field-label", "Region Preference"),
        tags$select(id = "region", class = "ctm-select unselected",
          tags$option("-- Select region --", value = ""),
          tags$option("No Preference",       value = "No Preference"),
          tags$option("East",                value = "East"),
          tags$option("West",                value = "West"),
          tags$option("Midwest",             value = "Midwest"),
          tags$option("South",               value = "South")
        )
      ),

      div(class = "field-group",
        tags$label(class = "field-label", "Patient Population"),
        tags$select(id = "pop", class = "ctm-select unselected",
          tags$option("-- Select population --", value = ""),
          tags$option("Adult (18+)",             value = "Adult"),
          tags$option("Pediatric (<18)",         value = "Pediatric"),
          tags$option("Geriatric (65+)",         value = "Geriatric"),
          tags$option("Mixed / All ages",        value = "Mixed")
        )
      ),

      div(class = "field-group",
        tags$label(class = "field-label", "Enrollment Urgency"),
        tags$select(id = "urgency", class = "ctm-select unselected",
          tags$option("-- Select urgency --", value = ""),
          tags$option("Standard",             value = "Standard"),
          tags$option("Accelerated",          value = "Accelerated"),
          tags$option("Flexible",             value = "Flexible")
        )
      ),

      tags$button(
        id      = "run",
        class   = "ctm-run-btn",
        onclick = "Shiny.setInputValue('run_click', Math.random())",
        "▶  Find Best Sites"
      )
    ),

    # Main output
    div(class = "ctm-main",
      uiOutput("results")
    )
  )
)

# ── Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  output$results <- renderUI({

    # Idle state — nothing clicked yet
    if (is.null(input$run_click) || input$run_click == 0) {
      return(div(class = "state-box",
        div(class = "state-icon", "🏥"),
        div(class = "state-msg",
          "Select your trial parameters and click ",
          tags$strong("Find Best Sites"), "."
        )
      ))
    }

    # Validate — all fields must be filled
    missing <- c()
    if (input$phase   == "") missing <- c(missing, "Trial Phase")
    if (input$ta      == "") missing <- c(missing, "Therapeutic Area")
    if (input$region  == "") missing <- c(missing, "Region Preference")
    if (input$pop     == "") missing <- c(missing, "Patient Population")
    if (input$urgency == "") missing <- c(missing, "Enrollment Urgency")

    if (length(missing) > 0) {
      return(div(class = "state-box",
        div(class = "state-icon", "⚠️"),
        div(class = "state-err",
          paste0("Please select: ", paste(missing, collapse = ", "))
        )
      ))
    }

    # Run scoring
    top3 <- get_recommendations(
      phase   = input$phase,
      ta      = input$ta,
      region  = input$region,
      urgency = input$urgency
    )

    if (is.null(top3) || nrow(top3) == 0) {
      return(div(class = "state-box",
        div(class = "state-icon", "🔍"),
        div(class = "state-err",
          "No qualifying sites found for this combination. Try changing the phase or therapeutic area."
        )
      ))
    }

    rank_labels <- c("🥇 RANK 1", "🥈 RANK 2", "🥉 RANK 3")
    rank_cls    <- c("card-rank rank-1", "card-rank rank-2", "card-rank rank-3")

    cards <- lapply(seq_len(nrow(top3)), function(i) {
      s  <- top3[i, ]
      sc <- s$score[1]
      bc <- score_color(sc)

      div(class = "site-card",
        div(class = rank_cls[i], rank_labels[i]),
        div(class = "card-id",   s$id[1]),
        div(class = "card-name", s$name[1]),

        div(class = "card-pills",
          span(class = "pill pill-region", s$region[1]),
          span(class = "pill pill-ta",     input$ta),
          span(class = "pill pill-phase",  paste("Phase", input$phase))
        ),

        div(class = "score-row",
          div(class = "score-lbl", "Match Score"),
          div(class = "score-track",
            div(class = "score-fill",
              style = paste0("width:", sc, "%; background:", bc, ";"))
          ),
          div(class = "score-num", style = paste0("color:", bc, ";"),
            paste0(sc, "/100"))
        ),

        div(class = "card-metrics",
          div(class = "metric",
            div(class = "metric-val", paste0(s$enrollment_rate[1], "%")),
            div(class = "metric-lbl", "Enrollment")
          ),
          div(class = "metric",
            div(class = "metric-val", paste0(s$screen_failure_rate[1], "%")),
            div(class = "metric-lbl", "Screen Fail")
          ),
          div(class = "metric",
            div(class = "metric-val", as.character(s$competing_trials[1])),
            div(class = "metric-lbl", "Competing")
          ),
          div(class = "metric",
            div(class = "metric-val", paste0(s$pi_experience[1], "/100")),
            div(class = "metric-lbl", "PI Score")
          )
        ),

        div(class = "card-narrative",
          span(class = "ntag ntag-s", "Strength"), strength_text(s),
          tags$br(), tags$br(),
          span(class = "ntag ntag-w", "Watch For"), caution_text(s)
        )
      )
    })

    tagList(
      div(class = "results-bar",
        div(class = "results-title", "Top Recommended Sites"),
        div(class = "results-meta",
          paste0(input$ta, " · Phase ", input$phase, " · ", input$region))
      ),
      tagList(cards)
    )
  }) |> bindEvent(input$run_click, ignoreNULL = FALSE)

}

shinyApp(ui, server)
