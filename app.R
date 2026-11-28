# =============================================================================
# STA304 Group Project: Interactive website
# "The Impact of Music on Performance During Intellectually Demanding Tasks"
#
# Run locally: open this file in RStudio and click "Run App".
# Deploy: see README.md (Posit Connect Cloud, from the public GitHub repo).
#
# The app has two halves that talk to each other:
#   - ui:     what the visitor sees (sidebar, tabs, plots, text)
#   - server: the R code that filters the data and redraws every output
#             whenever an input changes (this is "reactivity")
# =============================================================================

library(shiny)
library(bslib)        # page layout, cards and theme
library(ggplot2)      # plots
library(dplyr)        # data cleaning and summaries
library(tidyr)        # unnest() for the multi-select genre question
library(DT)           # searchable, sortable data table
library(colourpicker) # colour picker inputs


# =============================================================================
# 1. DATA (same cleaning as our EDA report)
# =============================================================================
# 79 responses; the duplicate submission was already removed. Student IDs and
# timestamps were deleted from this copy because the GitHub repo is public.
raw <- read.csv("data/music_survey.csv")

# Short code names (codebook) are for code only, never for what visitors see
names(raw) <- c("sta256_perf", "music_sta256", "music_quant", "genres",
                "music_retention", "study_hours", "pct_music", "session_min",
                "music_volume", "music_concentration")

genre_codes <- c(A = "Ambient", L = "Lo-fi", C = "Classical", J = "Jazz",
                 P = "Pop/R&B", H = "Hip-hop", O = "Other", N = "None")

survey <- raw |>
  mutate(
    # Volume: codebook says 1 = "N/A" (doesn't listen), so make it missing
    music_volume = na_if(music_volume, 1L),
    # Music group: readable categories, always in the order No, Sometimes, Yes
    music_sta256 = factor(music_sta256, levels = c("N", "S", "Y"),
                          labels = c("No", "Sometimes", "Yes")),
    # Genres: "None" and "N" both mean no music; use the codebook code N
    genres = if_else(genres == "None", "N", genres)
  )
# Genres is a "pick all that apply" question, so keep a list of codes per student
survey$genre_list  <- strsplit(survey$genres, ", ")
survey$genre_names <- vapply(survey$genre_list,
                             function(g) paste(genre_codes[g], collapse = ", "),
                             character(1))

n_total <- nrow(survey)


# =============================================================================
# 2. READABLE LABELS (used on every axis, legend, table and sentence)
# =============================================================================
var_labels <- c(
  sta256_perf         = "STA256 performance level",
  music_sta256        = "Listened to music while studying for STA256",
  music_quant         = "Music while studying quantitative courses",
  genres              = "Genres listened to most while studying",
  music_retention     = "Agrees music improves retention",
  study_hours         = "Study hours per week (STA256)",
  pct_music           = "Percent of STA256 study time with music",
  session_min         = "Study session length before a break (minutes)",
  music_volume        = "Music volume while studying",
  music_concentration = "Agrees music helps concentration"
)

# The same variables, worded to sit inside a sentence ("the highest average ...")
sentence_labels <- c(
  sta256_perf         = "STA256 performance level",
  music_sta256        = "music group",
  music_quant         = "frequency of listening to music when studying quantitative courses",
  genres              = "genres",
  music_retention     = "agreement that music improves retention",
  study_hours         = "weekly study hours",
  pct_music           = "percent of study time spent with music",
  session_min         = "study session length",
  music_volume        = "music volume",
  music_concentration = "agreement that music helps concentration"
)
capitalise <- function(s) paste0(toupper(substr(s, 1, 1)), substring(s, 2))

agree_scale <- c("1" = "Strongly disagree", "2" = "Disagree",
                 "3" = "Slightly disagree", "4" = "Neutral",
                 "5" = "Slightly agree", "6" = "Agree", "7" = "Strongly agree")

# Answer labels for the ordinal (ranked-category) questions, from the codebook
level_labels <- list(
  sta256_perf = c("1" = "Below 50%", "2" = "51-60%", "3" = "61-70%",
                  "4" = "71-80%", "5" = "81-90%", "6" = "91-100%"),
  music_quant = c("1" = "Never", "2" = "Rarely", "3" = "Occasionally",
                  "4" = "Sometimes", "5" = "Frequently", "6" = "Usually",
                  "7" = "Always"),
  music_retention     = agree_scale,
  music_concentration = agree_scale,
  music_volume = c("2" = "Very quiet", "3" = "Quiet", "4" = "Moderate",
                   "5" = "Loud", "6" = "Very loud")
)
ordinal_vars    <- names(level_labels)
continuous_vars <- c("study_hours", "pct_music", "session_min")
units <- c(study_hours = "hours", pct_music = "%", session_min = "minutes")

# "4 (71-80%)" style labels for a numeric axis, "4\n71-80%" for bar charts
axis_levels <- function(var) {
  paste0(names(level_labels[[var]]), " (", level_labels[[var]], ")")
}
bar_levels <- function(var) {
  paste0(names(level_labels[[var]]), "\n", level_labels[[var]])
}

# Dropdown choices: names are what the visitor sees, values are code names
choice <- function(vars) setNames(vars, var_labels[vars])

explore_choices <- list(
  "Performance"          = choice("sta256_perf"),
  "Music habits"         = choice(c("music_sta256", "music_quant", "genres",
                                    "pct_music", "music_volume")),
  "Study habits"         = choice(c("study_hours", "session_min")),
  "Opinions about music" = choice(c("music_concentration", "music_retention"))
)

# For the regression sentence: "each X more ..." in natural units
x_step <- c(pct_music = 10, study_hours = 1, session_min = 15,
            music_quant = 1, music_concentration = 1, music_retention = 1,
            music_volume = 1)
x_step_text <- c(
  pct_music           = "10 percentage points more study time with music",
  study_hours         = "each extra hour of studying per week",
  session_min         = "study sessions that are 15 minutes longer",
  music_quant         = "each step up in how often they listen (e.g. Sometimes to Frequently)",
  music_concentration = "each step more agreement that music helps concentration",
  music_retention     = "each step more agreement that music improves retention",
  music_volume        = "each step louder (e.g. Quiet to Moderate)"
)

# Short notes shown under the Explore plot (they describe the full survey)
question_notes <- c(
  sta256_perf = "Performance is self-reported in 10-percentage-point grade bands, so it reflects what students remember about their STA256 grade.",
  music_sta256 = "Students answered No, Sometimes or Yes. These three groups are compared in the Music vs. silence tab.",
  music_quant = "This asks about quantitative courses in general, not only STA256.",
  genres = "Students could pick more than one genre, so the percentages add up to more than 100%.",
  pct_music = "In the full survey, students tend to sit at the extremes: many rarely study with music and many almost always do, with fewer in between.",
  music_volume = "Only students who listen to music are included. Students who don't listen answered 'N/A', so they are left out.",
  study_hours = "In the full survey, most students study a few hours a week but a handful study 15 to 28 hours. Those few long values pull the average above the median, so the median describes a typical student better.",
  session_min = "Many students picked round numbers such as 30, 60 or 90 minutes, which is why some bars are much taller than their neighbours.",
  music_concentration = "Answers are on a 7-point scale from Strongly disagree to Strongly agree.",
  music_retention = "Answers are on a 7-point scale from Strongly disagree to Strongly agree."
)


# =============================================================================
# 3. COLOURS AND PLOT STYLE
# =============================================================================
# Each palette lists well-separated colours first (the 3 music groups use 1-3)
palettes <- list(
  "Colour-blind safe (Okabe-Ito)" = c("#E69F00", "#56B4E9", "#009E73", "#CC79A7",
                                      "#0072B2", "#D55E00", "#F0E442", "#999999"),
  "Viridis (colour-blind safe)"   = c("#440154", "#21908C", "#FDE725", "#3B528B",
                                      "#5DC863", "#2C728E", "#AADC32", "#472D7B"),
  "Sunset"                        = c("#264653", "#E76F51", "#E9C46A", "#2A9D8F",
                                      "#F4A261", "#9C3D54", "#8AB17D", "#B5838D"),
  "Bold"                          = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                                      "#66A61E", "#E6AB02", "#A6761D", "#666666"),
  "Greyscale (for printing)"      = c("#1A1A1A", "#7A7A7A", "#C8C8C8", "#4D4D4D",
                                      "#A3A3A3", "#2E2E2E", "#8F8F8F", "#E0E0E0")
)
group_colours <- function(pal) {
  setNames(palettes[[pal]][1:3], levels(survey$music_sta256))
}

# Whole-number legend steps for "Number of students" (no "2.5 students")
count_breaks <- function(lim) {
  b <- unique(round(pretty(lim, n = 4)))
  b[b > 0]
}

theme_survey <- function() {
  theme_minimal(base_size = 15) +
    theme(plot.title = element_text(face = "bold", size = 17),
          plot.title.position = "plot",
          plot.subtitle = element_text(colour = "grey35"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          axis.title = element_text(colour = "grey25"))
}

# ggplot2 4.0 renamed coord_trans() to coord_transform(); use whichever exists
coord_log_x <- function(xlim, ylim) {
  f <- if (exists("coord_transform", asNamespace("ggplot2"))) {
    ggplot2::coord_transform
  } else {
    ggplot2::coord_trans
  }
  f(x = "log10", xlim = xlim, ylim = ylim)
}


# =============================================================================
# 4. SMALL HELPERS FOR PLAIN-LANGUAGE RESULTS
# =============================================================================
fmt_p <- function(p) if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
fmt   <- function(x, d = 2) formatC(x, format = "f", digits = d)
pct   <- function(x) paste0(round(100 * x), "%")
pct1  <- function(x) paste0(formatC(100 * x, format = "f", digits = 1), "%")

# Overall results for the "Start here" tab, using all 79 students
full_means <- tapply(survey$sta256_perf, survey$music_sta256, mean)
full_aov   <- summary(aov(sta256_perf ~ music_sta256, data = survey))[[1]]
full_lm    <- summary(lm(sta256_perf ~ pct_music, data = survey))


# =============================================================================
# 5. USER INTERFACE
# =============================================================================
app_theme <- bs_theme(
  version = 5,
  bg = "#FBFAF7", fg = "#1E2330",
  primary = "#2F5D8A", secondary = "#5B6770",
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI",
                              "Roboto", "Helvetica Neue", "Arial", "sans-serif")
) |>
  # Blue top bar with white tab names; the open tab gets a gold underline
  bs_add_rules("
    :root { --bslib-navbar-default-bg: #2F5D8A; --bslib-navbar-inverse-bg: #2F5D8A; }
    body nav.navbar, body nav.navbar.navbar-default, body nav.navbar.navbar-inverse {
      background-color: #2F5D8A !important; }
    .navbar .navbar-brand { color: #FFFFFF !important; font-weight: 600; }
    .navbar .nav-link { color: rgba(255, 255, 255, 0.8) !important; }
    .navbar .nav-link:hover { color: #FFFFFF !important; }
    .navbar .nav-link.active { color: #FFFFFF !important;
                               box-shadow: inset 0 -3px 0 #F2C14E; }
    .card-header { font-weight: 600; }
    table.dataTable { font-size: 0.9rem; }
  ")

hours_range   <- c(floor(min(survey$study_hours)), ceiling(max(survey$study_hours)))
session_range <- range(survey$session_min)

ui <- page_navbar(
  title = "Music & Studying: STA304 Survey Explorer",
  theme = app_theme,
  window_title = "Music & Studying | STA304",
  fillable = FALSE,  # let the page scroll so cards are never cut off

  # ---- Sidebar: shared by every tab ----
  sidebar = sidebar(
    width = 330,
    accordion(
      open = c("Choose which students to include", "Colours"),
      accordion_panel(
        "Choose which students to include",
        p(class = "small text-muted",
          "Every plot, table and test on the site updates to use only the",
          "students you pick here."),
        checkboxGroupInput("f_group", var_labels[["music_sta256"]],
                           choices = levels(survey$music_sta256),
                           selected = levels(survey$music_sta256),
                           inline = TRUE),
        sliderInput("f_hours", "Study hours per week",
                    min = hours_range[1], max = hours_range[2],
                    value = hours_range, step = 0.5),
        sliderInput("f_session", "Typical study session (minutes)",
                    min = session_range[1], max = session_range[2],
                    value = session_range, step = 5),
        selectizeInput("f_genre", "Listens to any of these genres",
                       choices = setNames(names(genre_codes), genre_codes),
                       multiple = TRUE,
                       options = list(placeholder = "Any genre (no filter)")),
        actionButton("reset", "Reset filters",
                     class = "btn-outline-secondary btn-sm"),
        uiOutput("n_selected"),
        p(class = "small text-muted mt-2",
          "Our survey did not ask demographic questions such as age or",
          "gender, so the filters use study habits instead.")
      ),
      accordion_panel(
        "Colours",
        selectInput("palette", "Colours for the three music groups",
                    choices = names(palettes)),
        colourInput("main_col", "Colour for bars and dots", value = "#2F5D8A"),
        colourInput("line_col", "Colour for the trend line", value = "#D55E00")
      )
    )
  ),

  # ---- Tab 1: overview ----
  nav_panel(
    "Start here",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Students included", uiOutput("vb_n")),
      value_box("Listened to music for STA256 (Yes or Sometimes)",
                textOutput("vb_music")),
      value_box("Average STA256 performance level", textOutput("vb_perf"),
                textOutput("vb_perf_band"))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("What is this site?"),
        p("In October 2026 we surveyed students in STA304 (Surveys, Sampling",
          "and Observational Data) at UTM about how they use music while",
          "studying, and how they did in STA256, an earlier statistics course."),
        p("We focus on two questions:"),
        tags$ol(
          tags$li(strong("Music vs. silence: "),
                  "did students who listened to music while studying for STA256",
                  "do better or worse than students who studied in silence?"),
          tags$li(strong("Time with music: "),
                  "is spending more of your study time with music related to",
                  "how well you did?")
        ),
        p("Each question has its own tab. The ", strong("Explore answers"),
          " tab shows every survey question, and ", strong("All responses"),
          " lists the raw answers.")
      ),
      card(
        card_header("How to use it"),
        tags$ul(
          tags$li("Use the sidebar to include only the students you care",
                  "about, for example only students who study 5+ hours a",
                  "week, or only lo-fi listeners. Everything updates."),
          tags$li("Each plot has its own controls next to it: what to plot,",
                  "the plot style and the axis range."),
          tags$li("Change the colours in the sidebar. The first two schemes",
                  "are colour-blind safe."),
          tags$li("Below each plot, a short paragraph explains what it shows",
                  "in plain language. It rewrites itself when you change the",
                  "filters.")
        )
      )
    ),
    card(
      card_header("What we found (all 79 students)"),
      p(strong("Music vs. silence. "),
        sprintf(paste(
          "Students who listened to music 'Sometimes' had the highest average",
          "performance level (%s out of 6) and students who said 'Yes' the",
          "lowest (%s). The gap is under half a level, small enough to be due",
          "to chance (ANOVA, %s)."),
          fmt(full_means[["Sometimes"]]), fmt(full_means[["Yes"]]),
          fmt_p(full_aov[["Pr(>F)"]][1]))),
      p(strong("Time with music. "),
        sprintf(paste(
          "Students who spent more of their study time with music reported",
          "slightly lower performance, but the trend line is almost flat and",
          "the relationship is not statistically significant (%s). Study time",
          "with music explains only %s of the differences in performance."),
          fmt_p(coef(full_lm)["pct_music", "Pr(>|t|)"]),
          pct1(full_lm$r.squared))),
      p(strong("Bottom line. "),
        "This is an observational study: we asked students about their habits",
        "rather than assigning them to study with or without music. So any",
        "pattern here is an association, not proof that music changes grades.",
        "An experiment would be the next step to test that.")
    )
  ),

  # ---- Tab 2: RQ3, compare the three music groups ----
  nav_panel(
    "Music vs. silence",
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("Did listening to music go with different results?"),
        plotOutput("rq3_plot", height = "470px")
      ),
      card(
        card_header("Plot controls"),
        selectInput("rq3_y", "Compare the groups on",
                    choices = choice(c("sta256_perf", "music_concentration",
                                       "music_retention", "music_quant"))),
        radioButtons("rq3_style", "Plot style",
                     choices = c("Box plot" = "box",
                                 "Bubbles: number of students at each level" = "count",
                                 "Stacked bars: percent at each level" = "stack")),
        conditionalPanel(
          "input.rq3_style != 'stack'",
          checkboxInput("rq3_means", "Mark each group's average (white diamond)",
                        value = TRUE),
          sliderInput("rq3_ylim", "y-axis range (zoom in or out)",
                      min = 1, max = 6, value = c(1, 6), step = 1)
        )
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Summary by group"), tableOutput("rq3_table"),
           p(class = "small text-muted",
             "Spread (SD) is the standard deviation: roughly how far a typical",
             "student is from the group average.")),
      card(card_header("What does this tell us?"),
           uiOutput("rq3_text"), p_value_note)
    )
  ),

  # ---- Tab 3: RQ4, performance against time with music ----
  nav_panel(
    "Time with music",
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("Is more study time with music related to performance?"),
        plotOutput("rq4_plot", height = "470px")
      ),
      card(
        card_header("Plot controls"),
        selectInput("rq4_x", "Compare STA256 performance with",
                    choices = choice(c("pct_music", "study_hours", "session_min",
                                       "music_quant", "music_concentration",
                                       "music_retention", "music_volume"))),
        radioButtons("rq4_points", "Students with identical answers",
                     choices = c("Bigger dot = more students" = "count",
                                 "See-through dots that darken when stacked" = "alpha")),
        checkboxInput("rq4_line", "Show the fitted straight line", value = TRUE),
        checkboxInput("rq4_band", "Show the 95% confidence band", value = TRUE),
        sliderInput("rq4_xlim", "x-axis range", min = 0, max = 100,
                    value = c(0, 100)),
        sliderInput("rq4_ylim", "y-axis range (performance level)",
                    min = 1, max = 6, value = c(1, 6), step = 1),
        conditionalPanel(
          "input.rq4_x == 'study_hours' || input.rq4_x == 'session_min'",
          checkboxInput("rq4_log", "Log scale for the x-axis (spreads out the many small values)")
        )
      )
    ),
    card(card_header("What does this tell us?"), uiOutput("rq4_text"),
         p_value_note)
  ),

  # ---- Tab 4: explore any question ----
  nav_panel(
    "Explore answers",
    layout_columns(
      col_widths = c(8, 4),
      card(
        full_screen = TRUE,
        card_header("How did students answer?"),
        plotOutput("ex_plot", height = "470px")
      ),
      card(
        card_header("Plot controls"),
        selectInput("ex_var", "Survey question", choices = explore_choices),
        radioButtons("ex_scale", "Show", inline = TRUE,
                     choices = c("Number of students" = "n",
                                 "Percent of students" = "pct")),
        checkboxInput("ex_split", "Split by music group", value = FALSE),
        conditionalPanel(
          "['study_hours', 'pct_music', 'session_min'].includes(input.ex_var)",
          sliderInput("ex_bins", "Number of bars (bins)", min = 4, max = 30,
                      value = 12),
          conditionalPanel(
            "input.ex_var != 'pct_music'",
            checkboxInput("ex_log", "Log scale for the x-axis")
          )
        )
      )
    ),
    card(card_header("Reading this plot"), uiOutput("ex_text"))
  ),

  # ---- Tab 5: data table ----
  nav_panel(
    "All responses",
    card(
      card_header("Survey responses (with the sidebar filters applied)"),
      p(class = "small text-muted",
        "Click a column name to sort, or type in the boxes under the column",
        "names to search. Volume is blank for students who don't listen to music."),
      DTOutput("table")
    )
  ),

  # ---- Tab 6: about ----
  nav_panel(
    "About",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("The survey"),
        # EDIT: check that this matches how your group actually collected data
        p("We ran a Google Form survey of Fall 2026 STA304H5 students at UTM",
          "between October 5 and October 15, 2026. It asked 10 questions about",
          "music and study habits and about each student's STA256 grade",
          "(reported in grade bands, 1 = below 50% up to 6 = 91-100%)."),
        p("We received 80 submissions. One student submitted twice, so we kept",
          "their later answer, leaving 79 students."),
        p("We planned a simple random sample of STA304 students across lecture",
          "sections. To encourage people to respond, we approached classmates,",
          "offered to fill out their group's survey in exchange, and gave out",
          "small treats.")
      ),
      card(
        card_header("Things to keep in mind"),
        tags$ul(
          tags$li(strong("Not a truly random sample. "),
                  "Only students we reached, and who chose to answer, are",
                  "included, so they may not represent all STA304 students."),
          tags$li(strong("Self-reported answers. "),
                  "Grades and study habits are what students remember, not",
                  "official records."),
          tags$li(strong("Observational, not an experiment. "),
                  "We did not assign anyone to study with music, so results",
                  "show associations, not cause and effect."),
          tags$li(strong("Small subgroups. "),
                  "Filters can leave very few students. Results based on a",
                  "handful of students are unreliable, and checking many",
                  "subgroups makes a 'significant' result by luck more likely."),
          tags$li(strong("No demographic questions. "),
                  "We did not ask about age, gender or program, so we cannot",
                  "break results down that way."),
          tags$li(strong("Test assumptions. "),
                  "Performance is recorded in whole levels, so the residuals",
                  "are not normally distributed. As permitted in this course we",
                  "still report the ANOVA and regression, and rank-based checks",
                  "(Kruskal-Wallis and Spearman) are shown alongside them.")
        )
      )
    ),
    card(
      card_header("Who made this"),
      p("STA304 group project, Fall 2026: Saad Hussain, Calvin, Khalid",
        "Abdullayev, Lu Hoang-Nguyen, Ashritha Tangella and Divjot Sidhar."),
      p(class = "small text-muted",
        "Built with R Shiny. The code and data are in our GitHub repository.")
    )
  )
)


# =============================================================================
# 6. SERVER
# =============================================================================
server <- function(input, output, session) {

  # ---- The filtered data: every output below uses filtered() ----
  filtered <- reactive({
    d <- survey |>
      filter(music_sta256 %in% input$f_group,
             study_hours >= input$f_hours[1], study_hours <= input$f_hours[2],
             session_min >= input$f_session[1], session_min <= input$f_session[2])
    if (length(input$f_genre) > 0) {
      keep <- vapply(d$genre_list, function(g) any(g %in% input$f_genre),
                     logical(1))
      d <- d[keep, ]
    }
    d
  })

  observeEvent(input$reset, {
    updateCheckboxGroupInput(session, "f_group",
                             selected = levels(survey$music_sta256))
    updateSliderInput(session, "f_hours", value = hours_range)
    updateSliderInput(session, "f_session", value = session_range)
    updateSelectizeInput(session, "f_genre", selected = character(0))
  })

  output$n_selected <- renderUI({
    n <- nrow(filtered())
    msg <- if (n < 20 && n > 0) {
      span(class = "text-danger", " Small group: results are less reliable.")
    }
    p(class = "mt-3 mb-0", "Showing ", strong(n), " of ", n_total,
      " students.", msg)
  })

  need_students <- function(d, min_n = 1) {
    validate(need(nrow(d) >= min_n,
                  "Not enough students match the sidebar filters. Try widening them."))
  }

  # ---- Start here: value boxes ----
  output$vb_n <- renderUI(paste(nrow(filtered()), "of", n_total))
  output$vb_music <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("-")
    pct(mean(d$music_sta256 != "No"))
  })
  output$vb_perf <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("-")
    paste(fmt(mean(d$sta256_perf), 1), "of 6")
  })
  output$vb_perf_band <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("")
    paste0("about ", level_labels$sta256_perf[[as.character(round(mean(d$sta256_perf)))]],
           " in STA256")
  })

  # ===========================================================================
  # Tab 2: Music vs. silence (RQ3)
  # ===========================================================================
  # When the variable changes, reset the y-axis slider to that variable's scale.
  # freezeReactiveValue stops the plot drawing once with the old range.
  observeEvent(input$rq3_y, {
    k <- max(as.integer(names(level_labels[[input$rq3_y]])))
    freezeReactiveValue(input, "rq3_ylim")
    updateSliderInput(session, "rq3_ylim", min = 1, max = k, value = c(1, k))
  })

  rq3_data <- reactive({
    d <- filtered()
    d$y <- d[[input$rq3_y]]
    d <- d[!is.na(d$y), ]
    d$music_sta256 <- droplevels(d$music_sta256)
    d
  })

  output$rq3_plot <- renderPlot({
    d <- rq3_data()
    need_students(d)
    yvar  <- input$rq3_y
    cols  <- group_colours(input$palette)
    sizes <- table(d$music_sta256)
    x_lab <- function(l) paste0(l, "\n(n = ", sizes[l], ")")
    lv    <- as.integer(names(level_labels[[yvar]]))

    if (input$rq3_style == "stack") {
      # Percent of each group at each answer level
      tab <- d |>
        count(music_sta256, y) |>
        group_by(music_sta256) |>
        mutate(share = 100 * n / sum(n)) |>
        ungroup() |>
        mutate(level = factor(y, levels = rev(lv),
                              labels = rev(axis_levels(yvar))),
               txt_col = ifelse(y > median(lv), "white", "grey15"))
      shades <- setNames(colorRampPalette(c("#EEEEEE", input$main_col))(length(lv)),
                         axis_levels(yvar))
      ggplot(tab, aes(music_sta256, share, fill = level)) +
        geom_col(width = 0.65, colour = "white") +
        geom_text(aes(label = ifelse(share >= 7, paste0(round(share), "%"), ""),
                      colour = txt_col, group = level),
                  position = position_stack(vjust = 0.5), size = 4.2) +
        scale_colour_identity() +
        scale_fill_manual(values = shades) +
        scale_x_discrete(labels = x_lab) +
        labs(title = paste(var_labels[[yvar]], "by music group"),
             subtitle = "Each bar adds up to 100% of that group",
             x = var_labels[["music_sta256"]],
             y = "Percent of students in the group", fill = NULL) +
        theme_survey() +
        guides(fill = guide_legend(ncol = 1)) +
        theme(legend.position = "right")
    } else {
      p <- ggplot(d, aes(music_sta256, y))
      if (input$rq3_style == "box") {
        p <- p + geom_boxplot(aes(fill = music_sta256), width = 0.55,
                              colour = "grey20", alpha = 0.9, linewidth = 0.7) +
          scale_fill_manual(values = cols, guide = "none")
      } else {
        # geom_count draws one dot per answer, sized by how many gave it,
        # so no point is moved away from its real value
        p <- p + geom_count(aes(colour = music_sta256)) +
          scale_size_area(max_size = 16, name = "Number of students",
                          breaks = count_breaks) +
          scale_colour_manual(values = cols, guide = "none")
      }
      if (isTRUE(input$rq3_means)) {
        p <- p + stat_summary(fun = mean, geom = "point", shape = 23,
                              size = 5, fill = "white", colour = "black")
      }
      p + scale_y_continuous(breaks = lv, labels = axis_levels(yvar)) +
        scale_x_discrete(labels = x_lab) +
        coord_cartesian(ylim = input$rq3_ylim + c(-0.3, 0.3)) +
        labs(title = paste(var_labels[[yvar]], "by music group"),
             subtitle = if (isTRUE(input$rq3_means)) "White diamonds mark each group's average",
             x = var_labels[["music_sta256"]], y = var_labels[[yvar]]) +
        theme_survey()
    }
  }, alt = reactive(paste("Plot of", var_labels[[input$rq3_y]],
                          "for students who did and did not listen to music")))

  output$rq3_table <- renderTable({
    d <- rq3_data()
    need_students(d)
    d |>
      group_by(music_sta256) |>
      summarise(Students = n(), Average = mean(y), Median = median(y),
                `Spread (SD)` = sd(y), .groups = "drop") |>
      rename(`Listened to music` = music_sta256)
  }, digits = 2, striped = TRUE, width = "100%")

  rq3_test <- reactive({
    d <- rq3_data()
    sizes <- table(d$music_sta256)
    if (length(sizes) < 2) {
      return(list(ok = FALSE, msg = "Tick at least two music groups in the sidebar to compare them."))
    }
    if (any(sizes < 3)) {
      return(list(ok = FALSE, msg = "At least one group has fewer than 3 students with these filters, which is too few for a test. Try widening the filters."))
    }
    kw <- kruskal.test(y ~ music_sta256, data = d)$p.value
    if (length(sizes) == 2) {
      tt <- t.test(y ~ music_sta256, data = d)
      list(ok = TRUE, type = "t", p = tt$p.value, kw = kw,
           stat = sprintf("t(%s) = %s", fmt(tt$parameter, 1), fmt(tt$statistic)))
    } else {
      fit <- aov(y ~ music_sta256, data = d)
      tab <- summary(fit)[[1]]
      eta <- tab[["Sum Sq"]][1] / sum(tab[["Sum Sq"]])
      pw  <- pairwise.t.test(d$y, d$music_sta256, p.adjust.method = "bonferroni")
      list(ok = TRUE, type = "aov", p = tab[["Pr(>F)"]][1], kw = kw, eta = eta,
           pairwise = pw$p.value,
           stat = sprintf("F(%d, %d) = %s", tab$Df[1], tab$Df[2],
                          fmt(tab[["F value"]][1])))
    }
  })

  output$rq3_text <- renderUI({
    d <- rq3_data()
    need_students(d)
    yvar <- input$rq3_y
    lab  <- sentence_labels[[yvar]]
    k    <- max(as.integer(names(level_labels[[yvar]])))
    m    <- tapply(d$y, d$music_sta256, mean)
    gap  <- max(m) - min(m)

    if (length(m) < 2) {
      return(tagList(
        p(sprintf("The %d students shown are all in the %s group, with an average %s of %s.",
                  nrow(d), names(m), lab, fmt(m[[1]]))),
        p(em("Tick at least two music groups in the sidebar to compare them."))))
    }
    gap_pts <- round(gap * 10)

    describe <- p(sprintf(
      "Among the %d students shown, the %s group had the highest average %s (%s) and the %s group the lowest (%s), a gap of %s levels on a 1 to %d scale.",
      nrow(d), names(which.max(m)), lab, fmt(max(m)), names(which.min(m)),
      fmt(min(m)), fmt(gap), k),
      if (yvar == "sta256_perf") sprintf(
        "Each level is a 10-percentage-point grade band, so the gap is roughly %d percentage point%s.",
        gap_pts, if (gap_pts == 1) "" else "s"))

    res <- rq3_test()
    if (!res$ok) return(tagList(describe, p(em(res$msg))))

    test_name <- if (res$type == "aov") "A one-way ANOVA" else "A two-sample t-test"
    result <- p(test_name, " checks whether the group averages differ by more",
                " than chance alone would explain. Result: ",
                strong(paste0(res$stat, ", ", fmt_p(res$p)), .noWS = "after"), ".")

    if (res$p >= 0.05) {
      verdict <- p("Because p is above 0.05, the differences between the groups",
                   "are small enough that they could easily come from which",
                   "students happened to answer our survey. We do not have",
                   "evidence that the groups really differ.",
                   if (res$type == "aov") "Because the overall test found no difference, we don't go on to compare pairs of groups.")
    } else {
      verdict <- p("Because p is below 0.05, the differences are larger than we",
                   "would expect by chance, so there is evidence that the groups",
                   "differ on average. This is an observational survey, so this",
                   "is a pattern (an association), not proof that music causes",
                   "the difference.")
      if (res$type == "aov") {
        pw <- res$pairwise
        pairs <- character(0)
        for (i in rownames(pw)) for (j in colnames(pw)) {
          if (!is.na(pw[i, j])) pairs <- c(pairs, sprintf("%s vs. %s: %s", i, j, fmt_p(pw[i, j])))
        }
        verdict <- tagList(verdict,
          p("To see which groups differ, we compared each pair with a",
            "Bonferroni correction (which accounts for making several",
            "comparisons):", paste(pairs, collapse = "; "), "."))
      }
    }

    extras <- tagList(
      if (res$type == "aov") p(sprintf(
        "Size of the effect: music group accounts for about %s of the differences in %s between students.",
        pct(res$eta), lab)),
      p(class = "small", sprintf(
        "Check that doesn't assume normally distributed residuals (Kruskal-Wallis): %s.",
        fmt_p(res$kw))),
      if (nrow(d) < n_total) p(class = "small text-muted",
        "You are looking at a subgroup. The more subgroups you test, the more",
        "likely one will look 'significant' by luck alone.")
    )
    tagList(describe, result, verdict, extras)
  })

  # ===========================================================================
  # Tab 3: Time with music (RQ4)
  # ===========================================================================
  observeEvent(input$rq4_x, {
    x <- input$rq4_x
    r <- if (x %in% continuous_vars) {
      range(survey[[x]])
    } else {
      range(as.integer(names(level_labels[[x]])))
    }
    step <- if (x == "study_hours") 0.5 else if (x %in% c("pct_music", "session_min")) 5 else 1
    freezeReactiveValue(input, "rq4_xlim")
    updateSliderInput(session, "rq4_xlim", min = r[1], max = r[2],
                      value = r, step = step)
  })

  rq4_data <- reactive({
    d <- filtered()
    d$x <- d[[input$rq4_x]]
    d[!is.na(d$x), ]
  })

  output$rq4_plot <- renderPlot({
    d <- rq4_data()
    need_students(d)
    xvar <- input$rq4_x

    p <- ggplot(d, aes(x, sta256_perf))
    if (input$rq4_line) {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = input$rq4_band,
                           colour = input$line_col, fill = input$line_col,
                           alpha = 0.15, linewidth = 1.3)
    }
    # Points are plotted at the values students reported (no jitter)
    if (input$rq4_points == "count") {
      p <- p + geom_count(colour = input$main_col, alpha = 0.85) +
        scale_size_area(max_size = 12, name = "Number of students",
                        breaks = count_breaks)
    } else {
      p <- p + geom_point(colour = input$main_col, alpha = 0.35, size = 4)
    }
    if (xvar %in% ordinal_vars) {
      p <- p + scale_x_continuous(breaks = as.integer(names(level_labels[[xvar]])),
                                  labels = bar_levels(xvar))
    }
    # coord_* zooms the view without dropping students, so the fitted line
    # (and the test below) always uses everyone shown in the sidebar
    ylim <- input$rq4_ylim + c(-0.3, 0.3)
    use_log <- isTRUE(input$rq4_log) && xvar %in% c("study_hours", "session_min")
    if (use_log) {
      p <- p + scale_x_continuous(breaks = if (xvar == "study_hours")
        c(0.5, 1, 2, 5, 10, 20) else c(10, 20, 30, 60, 120, 180))
    }
    p <- p + if (use_log) coord_log_x(input$rq4_xlim, ylim) else
      coord_cartesian(xlim = input$rq4_xlim, ylim = ylim)

    p + scale_y_continuous(breaks = 1:6, labels = axis_levels("sta256_perf")) +
      labs(title = paste("STA256 performance vs.", sentence_labels[[xvar]]),
           subtitle = if (use_log) "x-axis on a log scale: the straight-line fit appears curved",
           x = var_labels[[xvar]], y = var_labels[["sta256_perf"]]) +
      theme_survey()
  }, alt = reactive(paste("Scatterplot of STA256 performance against",
                          var_labels[[input$rq4_x]])))

  output$rq4_text <- renderUI({
    d <- rq4_data()
    need_students(d)
    xvar <- input$rq4_x
    if (nrow(d) < 5 || sd(d$x) == 0) {
      return(p(em("Too few students (or everyone gave the same answer) to fit a line. Try widening the filters.")))
    }
    fit <- summary(lm(sta256_perf ~ x, data = d))
    co  <- coef(fit)["x", ]
    change <- co[["Estimate"]] * x_step[[xvar]]
    sp  <- suppressWarnings(cor.test(d$x, d$sta256_perf, method = "spearman",
                                     exact = FALSE))
    direction <- if (change >= 0) "higher" else "lower"

    tagList(
      p(sprintf(
        "The line is the straight-line (simple linear regression) fit for the %d students shown. For %s, the predicted STA256 performance level is %s levels %s (roughly %s percentage points, since each level is a 10-point grade band).",
        nrow(d), x_step_text[[xvar]], fmt(abs(change)), direction,
        fmt(abs(change) * 10, 1))),
      p(sprintf(
        "%s explains %s of the differences in performance between these students%s.",
        capitalise(sentence_labels[[xvar]]), pct1(fit$r.squared),
        if (fit$r.squared < 0.1) ", which is very little" else "")),
      p("Testing whether the true slope could be zero (a flat line): ",
        strong(sprintf("t(%d) = %s, %s", fit$df[2], fmt(co[["t value"]]),
                       fmt_p(co[["Pr(>|t|)"]])), .noWS = "after"), "."),
      if (co[["Pr(>|t|)"]] >= 0.05) {
        p("Because p is above 0.05, a tilt this small could easily appear by",
          "chance even if there were no relationship. We do not have evidence",
          "that", sentence_labels[[xvar]], "is related to performance.")
      } else {
        p("Because p is below 0.05, there is evidence of a relationship. This is",
          "an observational survey, so this is an association: it does not show",
          "that", sentence_labels[[xvar]], "causes higher or lower performance.")
      },
      p(class = "small", sprintf(
        "Check that doesn't assume normally distributed residuals (Spearman rank correlation): rho = %s, %s.",
        fmt(sp$estimate), fmt_p(sp$p.value))),
      if (xvar %in% ordinal_vars) p(class = "small text-muted",
        "This question has ranked answer categories, so a straight line is only",
        "a rough summary of the pattern.")
    )
  })

  # ===========================================================================
  # Tab 4: Explore answers
  # ===========================================================================
  output$ex_plot <- renderPlot({
    d <- filtered()
    need_students(d)
    v <- input$ex_var
    d$music_sta256 <- droplevels(d$music_sta256)
    cols  <- group_colours(input$palette)
    split <- isTRUE(input$ex_split)
    show_pct <- input$ex_scale == "pct"
    y_lab <- if (show_pct) {
      if (split) "Percent of students in the group" else "Percent of students"
    } else "Number of students"
    title <- var_labels[[v]]

    if (v %in% continuous_vars) {
      # Histogram for numeric answers
      # density * width = share of the group in each bar, so with the split
      # each panel shows percent of that music group
      p <- if (show_pct) {
        ggplot(d, aes(.data[[v]], y = after_stat(100 * density * width)))
      } else {
        ggplot(d, aes(.data[[v]]))
      }
      if (split) {
        p <- p + geom_histogram(aes(fill = music_sta256), bins = input$ex_bins,
                                colour = "white") +
          facet_wrap(~ music_sta256, ncol = 1) +
          scale_fill_manual(values = cols, guide = "none")
      } else {
        p <- p + geom_histogram(bins = input$ex_bins, fill = input$main_col,
                                colour = "white")
      }
      if (isTRUE(input$ex_log) && v != "pct_music") {
        p <- p + scale_x_log10(breaks = if (v == "study_hours")
          c(0.5, 1, 2, 5, 10, 20) else c(10, 20, 30, 60, 120, 180))
        title <- paste(title, "(log scale)")
      }
      p <- p + labs(x = var_labels[[v]], y = y_lab)
    } else {
      # Bar chart for categories
      if (v == "genres") {
        long <- d |> select(music_sta256, genre_list) |> unnest(genre_list) |>
          mutate(category = factor(genre_codes[genre_list], levels = genre_codes))
        group_n <- table(d$music_sta256)
        tab <- long |> count(music_sta256, category, .drop = FALSE)
        if (!split) tab <- tab |> group_by(category) |> summarise(n = sum(n))
        denom <- if (split) as.numeric(group_n[as.character(tab$music_sta256)]) else nrow(d)
        tab$share <- 100 * tab$n / denom
        # Most popular genre at the top of the (flipped) bar chart
        tab$category <- reorder(tab$category, tab$n, sum)
      } else {
        lv_labels <- if (v == "music_sta256") levels(d$music_sta256) else bar_levels(v)
        d$category <- if (v == "music_sta256") d$music_sta256 else
          factor(d[[v]], levels = as.integer(names(level_labels[[v]])), labels = lv_labels)
        d <- d[!is.na(d$category), ]
        tab <- if (split) count(d, music_sta256, category, .drop = FALSE) else
          count(d, category, .drop = FALSE)
        if (split) tab <- tab |> group_by(music_sta256)
        tab <- tab |> mutate(share = 100 * n / sum(n)) |> ungroup()
      }
      tab$value <- if (show_pct) tab$share else tab$n

      p <- ggplot(tab, aes(category, value))
      if (split) {
        p <- p + geom_col(aes(fill = music_sta256), position = position_dodge(0.8),
                          width = 0.75) +
          scale_fill_manual(values = cols, name = var_labels[["music_sta256"]])
      } else if (v == "music_sta256") {
        p <- p + geom_col(aes(fill = category), width = 0.6) +
          scale_fill_manual(values = cols, guide = "none")
      } else {
        p <- p + geom_col(fill = input$main_col, width = 0.7)
      }
      if (v == "genres") p <- p + coord_flip()
      p <- p + labs(x = NULL, y = y_lab)
    }
    p + labs(title = title, subtitle = paste(nrow(d), "students")) + theme_survey()
  }, alt = reactive(paste("Chart of answers to:", var_labels[[input$ex_var]])))

  output$ex_text <- renderUI({
    d <- filtered()
    need_students(d)
    v <- input$ex_var
    n <- nrow(d)

    summary_text <- if (v %in% continuous_vars) {
      x <- d[[v]]
      with_unit <- function(val) {
        if (v == "pct_music") paste0(fmt(val, 1), "%") else paste(fmt(val, 1), units[[v]])
      }
      q <- quantile(x, c(0.25, 0.5, 0.75))
      sprintf(
        "Among the %d students shown, the median is %s: half of them are below it and half above. The middle half of students fall between %s and %s, and the average is %s.",
        n, with_unit(q[2]), with_unit(q[1]), with_unit(q[3]), with_unit(mean(x)))
    } else if (v == "genres") {
      counts <- sort(table(factor(unlist(d$genre_list), levels = names(genre_codes))),
                     decreasing = TRUE)
      sprintf("The most popular genre among these %d students was %s (%s of them), followed by %s (%s) and %s (%s).",
              n, genre_codes[[names(counts)[1]]], pct(counts[[1]] / n),
              genre_codes[[names(counts)[2]]], pct(counts[[2]] / n),
              genre_codes[[names(counts)[3]]], pct(counts[[3]] / n))
    } else if (v == "music_sta256") {
      sh <- prop.table(table(d$music_sta256))
      sprintf("Of the %d students shown, %s said Yes, %s said Sometimes and %s said No.",
              n, pct(sh[["Yes"]]), pct(sh[["Sometimes"]]), pct(sh[["No"]]))
    } else {
      x <- d[[v]][!is.na(d[[v]])]
      labs_v <- level_labels[[v]]
      mode_lv <- names(which.max(table(factor(x, levels = names(labs_v)))))
      base <- sprintf("The most common answer among these %d students was '%s' (%s).",
                      length(x), labs_v[[mode_lv]], pct(mean(x == as.integer(mode_lv))))
      extra <- switch(v,
        sta256_perf = sprintf("%s reported 81%% or higher (levels 5-6) and %s reported 60%% or lower (levels 1-2).",
                              pct(mean(x >= 5)), pct(mean(x <= 2))),
        music_quant = sprintf("%s listen to music at least 'Frequently' when studying quantitative courses, while %s never do.",
                              pct(mean(x >= 5)), pct(mean(x == 1))),
        music_volume = sprintf("%s keep the music quiet or very quiet, and %s play it loud or very loud.",
                               pct(mean(x <= 3)), pct(mean(x >= 5))),
        sprintf("%s at least slightly agree, %s are neutral and %s at least slightly disagree.",
                pct(mean(x >= 5)), pct(mean(x == 4)), pct(mean(x <= 3))))
      paste(base, extra)
    }
    tagList(p(summary_text), p(class = "small text-muted", question_notes[[v]]))
  })

  # ===========================================================================
  # Tab 5: data table
  # ===========================================================================
  output$table <- renderDT({
    d <- filtered()
    level_text <- function(var) {
      x <- d[[var]]
      ifelse(is.na(x), NA, paste0(x, ": ", level_labels[[var]][as.character(x)]))
    }
    shown <- data.frame(
      level_text("sta256_perf"), d$music_sta256, level_text("music_quant"),
      d$genre_names, d$pct_music, d$study_hours, d$session_min,
      level_text("music_volume"), level_text("music_concentration"),
      level_text("music_retention")
    )
    # Shorter (but still readable) column names so the headers stay compact
    names(shown) <- c("STA256 performance", "Music for STA256",
                      "Music for quantitative courses", "Genres",
                      "% of study time with music", "Study hours per week",
                      "Session length (min)", "Music volume",
                      "Music helps concentration", "Music improves retention")
    datatable(shown, rownames = FALSE, filter = "top",
              class = "compact stripe hover",
              options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui, server)
