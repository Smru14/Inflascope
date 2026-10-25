library(shiny)
library(bslib)
library(plotly)
library(DT)
library(inflascope)

# ---- Brand and chart tokens -------------------------------------------------
brand <- list(sienna = "#CB4B2F", zinc = "#18181B", bg = "#FAFAFA", muted = "#71717A",
              grid = "#E4E4E7", card = "#FFFFFF")
sector_col <- c(Rural = "#CB4B2F", Urban = "#2F6DB5", Combined = "#71717A")
category_col <- c(Food = "#CB4B2F", `Non-food` = "#2F6DB5")
scenario_col <- c(Low = "#8A8A1F", Moderate = "#2F6DB5", `Food shock` = "#CB4B2F",
                  `Non-food shock` = "#7A4FA3", Custom = "#18181B")
seq_scale <- list(c(0, "#FBEAE5"), c(0.5, "#E08A72"), c(1, "#8E2F1B"))

theme <- bs_theme(
  version = 5, bg = brand$bg, fg = brand$zinc, primary = brand$sienna, secondary = brand$zinc,
  base_font = font_collection("Inter", "system-ui", "-apple-system", "Segoe UI", "Roboto", "sans-serif"),
  "card-border-color" = brand$grid, "border-radius" = "0.5rem"
)
css <- "
.navbar { border-bottom: 3px solid #CB4B2F; }
.navbar .navbar-brand { font-weight: 700; letter-spacing: .02em; }
.card { background: #FFFFFF; box-shadow: 0 1px 2px rgba(24,24,27,.06); }
.card-header { background: #FFFFFF; font-weight: 600; border-bottom: 1px solid #E4E4E7; }
.bslib-value-box .value-box-title { font-size: .85rem; color: #52525B; }
.bslib-value-box .value-box-value { font-size: 1.6rem; font-weight: 700; color: #18181B; }
.bslib-value-box { border-left: 4px solid #CB4B2F !important; }
.tag-observed, .tag-synthetic { display:inline-block; font-size:.75rem; font-weight:600; padding:.15rem .5rem; border-radius:999px; }
.tag-observed { background:#18181B; color:#FAFAFA; }
.tag-synthetic { background:#FBEAE5; color:#8E2F1B; }
.note { color:#52525B; font-size:.9rem; }
table.table { font-size: .9rem; }
"

`%||%` <- function(a, b) if (is.null(a)) b else a
fmt_inr <- function(x) paste0("₹", formatC(round(x), format = "d", big.mark = ","))
fmt_pct <- function(x, d = 2) sprintf(paste0("%.", d, "f%%"), x)
tag_observed <- function() span(class = "tag-observed", "Observed official data")
tag_synthetic <- function() span(class = "tag-synthetic", "Hypothetical calculation")

plot_layout <- function(p, xlab = NULL, ylab = NULL, ...) {
  layout(p, paper_bgcolor = brand$card, plot_bgcolor = brand$card,
         font = list(color = brand$zinc, family = "Inter, system-ui, sans-serif"),
         xaxis = list(title = xlab, gridcolor = brand$grid, zeroline = FALSE),
         yaxis = list(title = ylab, gridcolor = brand$grid, zeroline = FALSE),
         legend = list(orientation = "h", y = 1.12, x = 0), hovermode = "x unified",
         margin = list(t = 40), ...) |>
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("lasso2d", "select2d"))
}

# Insert explicit NA rows for missing months so lines break instead of
# implying values that were never published.
fill_months <- function(d) {
  if (!nrow(d)) return(d)
  out <- lapply(split(d, d$sector), function(x) {
    all <- seq(min(x$date), max(x$date), by = "month")
    m <- merge(data.frame(date = all), x, by = "date", all.x = TRUE)
    m$sector <- x$sector[1]
    m
  })
  do.call(rbind, out)
}

line_chart <- function(d, measure) {
  ylab <- if (measure == "inflation_yoy") "YoY inflation (%)" else "CPI index (2024=100)"
  d <- fill_months(d[!is.na(d[[measure]]), , drop = FALSE])
  p <- plot_ly()
  for (s in intersect(names(sector_col), unique(d$sector))) {
    x <- d[d$sector == s, ]
    p <- add_trace(p, data = x, x = ~date, y = x[[measure]], type = "scatter", mode = "lines+markers",
                   name = s, line = list(color = sector_col[[s]], width = 2, dash = if (s == "Combined") "dash" else "solid"),
                   marker = list(color = sector_col[[s]], size = 8, line = list(color = brand$card, width = 2)),
                   connectgaps = FALSE,
                   hovertemplate = paste0(s, ": %{y:.2f}<extra></extra>"))
  }
  plot_layout(p, "Reference month", ylab)
}

hces <- get_static_data("hces")
cpi_detail <- get_static_data("cpi_detail")
states <- c("All India", sort(setdiff(unique(cpi_detail$state), "All India")))
categories <- c("General", sort(setdiff(unique(cpi_detail$category), "General")))
short_cat <- function(x) sub("^(.{30}).+$", "\\1...", x)
bundled_scenarios <- get_scenario_data()
food_benchmark <- function(profile) 100 * hces$share[hces$sector == profile & hces$category == "Food"]
stress_presets <- list(Low = c(Food = 2, `Non-food` = 2), Moderate = c(Food = 5, `Non-food` = 4),
                       `Food shock` = c(Food = 15, `Non-food` = 4), `Non-food shock` = c(Food = 4, `Non-food` = 12))

# ---- UI ---------------------------------------------------------------------
ui <- page_navbar(
  title = "INFLASCOPE INDIA", theme = theme, fillable = FALSE, id = "nav", bg = brand$zinc, inverse = TRUE,
  header = tagList(tags$head(tags$style(HTML(css)))),
  nav_panel("Overview",
    div(class = "py-2", p(class = "note mb-1", "Rural and urban inflation in India: official MoSPI CPI (base 2024=100) and HCES 2023-24 benchmarks."), tag_observed()),
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Latest rural CPI inflation (YoY)", textOutput("rural_kpi")),
      value_box("Latest urban CPI inflation (YoY)", textOutput("urban_kpi")),
      value_box("Rural minus urban", textOutput("gap_kpi")),
      value_box("Latest reference month", textOutput("month_kpi"))),
    layout_columns(col_widths = c(8, 4),
      navset_card_tab(
        nav_panel("YoY inflation", plotlyOutput("trend", height = "360px"),
                  p(class = "note", "Year-on-year rates are published from January 2026, the first month with a full year of 2024-base indices.")),
        nav_panel("CPI index", plotlyOutput("trend_index", height = "360px"),
                  p(class = "note", "Monthly CPI general index, base 2024=100, January 2025 onwards."))),
      div(card(card_header("Key finding"), uiOutput("insight")),
          card(card_header("Data provenance"), tableOutput("provenance")))),
    card(card_header("Latest YoY inflation by CPI division: rural vs urban"), plotlyOutput("division_plot", height = "460px"))),

  nav_panel("Historical Explorer",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        selectInput("state", "State / UT", states),
        selectInput("category", "CPI division", categories),
        checkboxGroupInput("sector", "Sectors", c("Rural", "Urban", "Combined"), selected = c("Rural", "Urban")),
        radioButtons("measure", "Metric", c("Year-on-year inflation (%)" = "inflation_yoy", "CPI index level" = "index")),
        dateRangeInput("range", "Reference months", start = NULL, end = NULL),
        downloadButton("download_cpi", "Download filtered CSV")),
      navset_card_tab(
        nav_panel("Trend", plotlyOutput("historic_plot", height = "380px")),
        nav_panel("Compare states", selectInput("state_month", "Month", choices = NULL, width = "200px"),
                  plotlyOutput("state_plot", height = "760px"),
                  p(class = "note", "Uses the selected CPI division and the first selected sector. Chandigarh has no rural series."))),
      card(card_header("Selected observations"), DTOutput("historic_table"))))
  ,
  nav_panel("Rural & Urban Consumption",
    div(class = "py-2", tag_observed()),
    layout_columns(col_widths = c(3, 3, 3, 3),
      value_box("Rural MPCE", fmt_inr(4122), p("per person per month")),
      value_box("Urban MPCE", fmt_inr(6996), p("per person per month")),
      value_box("Rural food share", "47.04%"),
      value_box("Urban food share", "39.68%")),
    layout_columns(col_widths = c(6, 6),
      card(card_header("Share of spending: food vs non-food"), plotlyOutput("hces_share", height = "320px")),
      card(card_header("Monthly per-capita spending by category (INR)"), plotlyOutput("hces_amount", height = "320px"))),
    card(card_header("HCES 2023-24 benchmarks (excluding free-item imputation)"), tableOutput("hces_table"),
         p(class = "note", "MPCE is a per-person national average, not a typical household's monthly spending. HCES shares are consumption benchmarks, not official CPI weights."))),

  nav_panel("Personal Inflation Calculator",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        selectInput("profile", "Benchmark profile", c("Rural", "Urban")),
        numericInput("monthly", "Current monthly household spending (INR)", 25000, min = 0, step = 500),
        sliderInput("food_pct", "Food spending share (%)", 0, 100, 47),
        numericInput("food_rate", "Assumed annual food inflation (%)", 6, min = -99, max = 100, step = 0.5),
        numericInput("other_rate", "Assumed annual non-food inflation (%)", 4, min = -99, max = 100, step = 0.5),
        actionButton("reset", "Reset share to HCES benchmark", class = "btn-outline-primary"),
        actionButton("official_food", "Use latest official food inflation", class = "btn-outline-primary mt-2"),
        p(class = "note mt-2", textOutput("official_food_note", inline = TRUE))),
      div(class = "pb-2", tag_synthetic()),
      layout_columns(col_widths = c(4, 4, 4),
        value_box("Your basket inflation", textOutput("p_rate")),
        value_box("Extra cost per month (1 year)", textOutput("p_extra")),
        value_box("Projected monthly cost", textOutput("p_proj"))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Contribution to basket inflation (percentage points)"), plotlyOutput("contrib_plot", height = "300px")),
        card(card_header("Your basket vs HCES rural and urban baskets"), plotlyOutput("burden_plot", height = "300px"))),
      card(card_header("Category detail"), tableOutput("personal_table"),
           p(class = "note", "Fixed-basket estimate: sum of category weight x assumed rate. Not an official CPI measure.")))),

  nav_panel("Stress Test & Budget Resilience",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        selectInput("stress_profile", "Spending mix", c("Rural benchmark" = "Rural", "Urban benchmark" = "Urban", "My calculator mix" = "Personal")),
        numericInput("stress_spend", "Current basket cost (INR per month)", 25000, min = 0, step = 500),
        numericInput("available", "Available monthly budget (INR)", 28000, min = 0, step = 500),
        sliderInput("horizon", "Years ahead", 1, 5, 3),
        selectInput("scenario", "Highlighted scenario", c(names(stress_presets), "Custom"), selected = "Moderate"),
        numericInput("custom_food", "Custom food inflation (%)", 8, min = -99, max = 100),
        numericInput("custom_other", "Custom non-food inflation (%)", 5, min = -99, max = 100)),
      div(class = "pb-2", tag_synthetic()),
      layout_columns(col_widths = c(4, 4, 4),
        value_box("Projected monthly cost", textOutput("s_proj")),
        value_box("Additional monthly cost", textOutput("s_extra")),
        value_box("Budget surplus / shortfall", textOutput("s_gap"))),
      card(card_header("Projected monthly cost by scenario"), plotlyOutput("stress_plot", height = "360px"),
           p(class = "note", "Dotted line: available monthly budget. Costs compound annually with fixed quantities; no substitution is modelled.")),
      card(card_header("All scenarios at the selected horizon"), tableOutput("stress_table")))),

  nav_panel("Scenario Explorer",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        checkboxGroupInput("sc_profile", "Profile", c("Rural", "Urban"), selected = c("Rural", "Urban"), inline = TRUE),
        uiOutput("sc_filters"),
        fileInput("sc_file", "Use your own scenario CSV (same columns)", accept = ".csv"),
        textOutput("sc_source"),
        downloadButton("sc_download", "Download filtered scenarios")),
      div(class = "pb-2", tag_synthetic(), span(class = "note ms-2", "10,000 calculated scenarios. Not household survey records or official CPI data.")),
      layout_columns(col_widths = c(3, 3, 3, 3),
        value_box("Scenarios selected", textOutput("sc_n")),
        value_box("Mean basket inflation", textOutput("sc_rate")),
        value_box("Mean extra monthly cost", textOutput("sc_extra")),
        value_box("Scenarios with a shortfall", textOutput("sc_short"))),
      layout_columns(col_widths = c(6, 6),
        card(card_header("Share of scenarios with a budget shortfall (%)"), plotlyOutput("sc_heat", height = "360px"),
             p(class = "note", "Budget = 110% of current spending. Each cell pools the selected spending levels and horizons.")),
        card(card_header("Mean extra monthly cost by horizon"), plotlyOutput("sc_years", height = "360px"))),
      card(card_header("Extra monthly cost vs monthly spending"), plotlyOutput("sc_spend", height = "320px")),
      card(card_header("Summary by profile and horizon"), tableOutput("sc_summary")),
      card(card_header("Scenario rows"), DTOutput("sc_table")))),

  nav_panel("Forecasting Studio",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        selectInput("forecast_sector", "Sector", c("Rural", "Urban")),
        sliderInput("forecast_horizon", "Months ahead", 1, 12, 6),
        selectInput("forecast_model", "Model", c("ARIMA" = "arima", "Seasonal naive" = "snaive")),
        fileInput("forecast_file", "Import a long, continuous official CPI CSV", accept = ".csv"),
        downloadButton("forecast_download", "Download forecast CSV")),
      card(card_header("Series readiness"), tableOutput("forecast_status"), uiOutput("forecast_message")),
      card(card_header("CPI index forecast with 80% and 95% intervals"), plotlyOutput("forecast_plot", height = "360px")),
      layout_columns(col_widths = c(7, 5),
        card(card_header("Forecast values"), tableOutput("forecast_table")),
        card(card_header("Holdout accuracy (ARIMA vs seasonal naive)"), uiOutput("accuracy_message"), tableOutput("accuracy_table"))))),

  nav_panel("Research & Data",
    layout_columns(col_widths = c(5, 7),
      card(card_header("Data selection and import"),
        selectInput("data_mode", "CPI data source", c("Bundled official snapshot" = "static", "Validated cached data" = "cache", "Best validated available" = "auto")),
        fileInput("import_file", "Import a canonical official CPI CSV", accept = ".csv"),
        actionButton("import_btn", "Validate and save to cache", class = "btn-primary"),
        div(class = "mt-2", textOutput("import_status")),
        p(class = "note mt-2", "Automatic network updates are disabled until a reliable official endpoint is verified.")),
      card(card_header("Research notes"),
        tags$ul(
          tags$li("Inflation is a change in prices; HCES shares describe spending composition, not causal behavioural responses."),
          tags$li("CPI data: MoSPI download cpi_1153 (base 2024=100), January 2025 to September 2026, All India plus 36 states and UTs, 13 divisions."),
          tags$li("Only CPI base 2024=100 observations are combined. Missing months stay missing."),
          tags$li("Scenario rows are synthetic calculations from simulate_budget(), kept separate from observed data.")),
        tags$a(href = "https://esankhyiki.mospi.gov.in/macroindicators?product=cpi", target = "_blank", "Official CPI portal"), tags$br(),
        tags$a(href = "https://mospi.gov.in/sites/default/files/publication_reports/HCES%20FactSheet%202023-24.pdf", target = "_blank", "Official HCES 2023-24 fact sheet"))),
    card(card_header("Active CPI dataset"), DTOutput("all_data"), div(downloadButton("all_download", "Download active CPI data"))),
    card(card_header("Data dictionary"), tableOutput("dictionary"))),

  nav_panel("Export Report",
    card(card_header("Downloadable HTML summary"),
      p("The report reflects your current selections: CPI data source, personal calculator, stress test and scenario filters."),
      tags$ul(tags$li("Rural vs urban CPI comparison and provenance"), tags$li("HCES consumption benchmarks"),
              tags$li("Personal basket and stress test results"), tags$li("Scenario Explorer summary"), tags$li("Assumptions and limitations")),
      downloadButton("report", "Download analysis report (.html)", class = "btn-primary")))
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {
  cache_version <- reactiveVal(0)
  chosen_data <- reactive({
    cache_version()
    if (input$data_mode == "cache") tryCatch(get_cpi_data("cache"), error = function(e) {
      showNotification("No validated cache found; showing the bundled snapshot.", type = "warning")
      get_cpi_data("static")
    }) else get_cpi_data(input$data_mode)
  })
  general <- reactive({ d <- chosen_data(); d[d$category == "General", , drop = FALSE] })
  latest <- reactive(get_latest_cpi(chosen_data()))
  comparison <- reactive(compare_rural_urban(data = chosen_data()))

  observe({
    d <- general()
    updateDateRangeInput(session, "range", start = min(d$date), end = max(d$date), min = min(d$date), max = max(d$date))
  })
  observe({
    m <- sort(unique(cpi_detail$date[!is.na(cpi_detail$inflation_yoy)]), decreasing = TRUE)
    updateSelectInput(session, "state_month", choices = stats::setNames(format(m), format(m, "%B %Y")))
  })

  # Overview
  kpi <- function(s) { l <- latest(); v <- l$inflation_yoy[l$sector == s]; if (length(v) && !is.na(v)) fmt_pct(v) else "n/a" }
  output$rural_kpi <- renderText(kpi("Rural"))
  output$urban_kpi <- renderText(kpi("Urban"))
  output$gap_kpi <- renderText(sprintf("%+.2f pp", comparison()$difference))
  output$month_kpi <- renderText(format(comparison()$date, "%B %Y"))
  output$trend <- renderPlotly(line_chart(general()[general()$sector != "Combined", ], "inflation_yoy"))
  output$trend_index <- renderPlotly(line_chart(general()[general()$sector != "Combined", ], "index"))
  output$division_plot <- renderPlotly({
    d <- chosen_data(); d <- d[d$date == comparison()$date & d$sector %in% c("Rural", "Urban") & !is.na(d$inflation_yoy), ]
    validate(need(nrow(d) > 0, "No division-level rates in the active data."))
    ord <- d[d$sector == "Rural", ]; ord <- ord$category[order(ord$inflation_yoy)]
    p <- plot_ly()
    for (s in c("Urban", "Rural")) {
      x <- d[d$sector == s, ]
      p <- add_trace(p, y = factor(x$category, ord), x = x$inflation_yoy, type = "bar", orientation = "h", name = s,
                     marker = list(color = sector_col[[s]]), hovertemplate = paste0(s, ": %{x:.2f}%<extra></extra>"))
    }
    plot_layout(p, "YoY inflation (%)", NULL, barmode = "group", bargap = 0.3) |>
      layout(hovermode = "y unified", margin = list(l = 10), legend = list(traceorder = "reversed", orientation = "h", y = 1.08, x = 0))
  })
  output$insight <- renderUI(tagList(p(generate_insights(comparison())),
    p(class = "note", sprintf("Rural index %.2f vs urban %.2f in %s.",
      compare_rural_urban(comparison()$date, chosen_data(), measure = "index")$rural,
      compare_rural_urban(comparison()$date, chosen_data(), measure = "index")$urban, format(comparison()$date, "%B %Y")))))
  output$provenance <- renderTable({
    m <- get_data_metadata(chosen_data())
    data.frame(Item = c("Source", "Latest period", "CPI base", "Latest release", "Dataset version", "Observations"),
               Value = c(m$source, format(m$latest_period), paste(m$base_year, collapse = ", "), format(m$latest_release),
                         paste(m$dataset_version, collapse = ", "), nrow(chosen_data())))
  })

  # Historical explorer
  explorer_base <- reactive({
    req(input$state, input$category)
    if (input$state != "All India") return(get_cpi_detail(input$state, category = input$category))
    d <- chosen_data(); d <- d[d$category == input$category, , drop = FALSE]
    if (!nrow(d)) d <- get_cpi_detail("All India", category = input$category)
    d
  })
  filtered <- reactive({
    d <- explorer_base(); req(input$range, length(input$sector) > 0)
    d[d$date >= as.Date(input$range[1]) & d$date <= as.Date(input$range[2]) & d$sector %in% input$sector, , drop = FALSE]
  })
  output$state_plot <- renderPlotly({
    req(input$state_month, length(input$sector) > 0)
    s <- input$sector[1]
    d <- cpi_detail[cpi_detail$date == as.Date(input$state_month) & cpi_detail$category == input$category & cpi_detail$sector == s & !is.na(cpi_detail$inflation_yoy), ]
    validate(need(nrow(d) > 0, "No published rates for this selection."))
    d <- d[order(d$inflation_yoy), ]
    col <- ifelse(d$state == "All India", brand$zinc, sector_col[[s]])
    plot_ly(y = factor(d$state, d$state), x = d$inflation_yoy, type = "bar", orientation = "h", marker = list(color = col),
            hovertemplate = "%{y}: %{x:.2f}%<extra></extra>") |>
      plot_layout(paste0(s, " YoY inflation (%), ", short_cat(input$category)), NULL, showlegend = FALSE) |>
      layout(hovermode = "closest", margin = list(l = 10))
  })
  output$historic_plot <- renderPlotly({
    validate(need(length(input$sector) > 0, "Select at least one sector."), need(nrow(filtered()) > 0, "No observations in this date range."))
    line_chart(filtered(), input$measure)
  })
  output$historic_table <- renderDT(datatable(
    transform(filtered(), date = format(date, "%Y-%m"), state = input$state)[, c("date", "state", "sector", "category", "index", "inflation_yoy", "base_year")],
    rownames = FALSE, colnames = c("Month", "State / UT", "Sector", "Division", "CPI index", "YoY inflation (%)", "Base"),
    options = list(pageLength = 10, scrollX = TRUE, dom = "tip")))

  # Consumption
  output$hces_table <- renderTable(data.frame(
    Sector = c("Rural", "Urban"), `MPCE (INR / person / month)` = c(4122, 6996),
    `Food share (%)` = c(47.04, 39.68), `Non-food share (%)` = c(52.96, 60.32),
    `Food spend (INR)` = round(c(4122 * .4704, 6996 * .3968)), `Non-food spend (INR)` = round(c(4122 * .5296, 6996 * .6032)),
    check.names = FALSE), digits = 2)
  hces_bars <- function(value, ylab, fmt) {
    p <- plot_ly()
    for (cat in names(category_col)) {
      x <- hces[hces$category == cat, ]
      y <- value(x)
      p <- add_trace(p, x = x$sector, y = y, type = "bar", name = cat, marker = list(color = category_col[[cat]], line = list(color = brand$card, width = 2)),
                     text = fmt(y), textposition = "inside", insidetextfont = list(color = "#FFFFFF"),
                     hovertemplate = paste0(cat, ": ", fmt(y), "<extra></extra>"))
    }
    plot_layout(p, NULL, ylab, barmode = "stack", bargap = 0.45) |> layout(hovermode = "closest")
  }
  output$hces_share <- renderPlotly(hces_bars(function(x) 100 * x$share, "Percent of MPCE", function(y) fmt_pct(y, 1)))
  output$hces_amount <- renderPlotly(hces_bars(function(x) x$mpce_inr * x$share, "INR per person per month", fmt_inr))

  # Personal calculator
  observeEvent(input$profile, updateSliderInput(session, "food_pct", value = round(food_benchmark(input$profile))))
  observeEvent(input$reset, updateSliderInput(session, "food_pct", value = round(food_benchmark(input$profile))))
  official_food_rate <- reactive({
    d <- chosen_data(); d <- d[d$category == "Food and beverages" & d$sector == input$profile & !is.na(d$inflation_yoy), ]
    if (!nrow(d)) return(NULL)
    d[which.max(d$date), ]
  })
  output$official_food_note <- renderText({ r <- official_food_rate(); if (is.null(r)) "" else
    sprintf("Official %s food and beverages inflation, %s: %.2f%%.", tolower(input$profile), format(r$date, "%B %Y"), r$inflation_yoy) })
  observeEvent(input$official_food, { r <- official_food_rate(); if (!is.null(r)) updateNumericInput(session, "food_rate", value = r$inflation_yoy) })
  weights <- reactive(c(Food = input$food_pct / 100, `Non-food` = 1 - input$food_pct / 100))
  rates <- reactive({ req(is.numeric(input$food_rate), is.numeric(input$other_rate)); c(Food = input$food_rate, `Non-food` = input$other_rate) })
  personal <- reactive({ req(is.numeric(input$monthly), input$monthly >= 0); simulate_budget(input$monthly, weights(), rates(), 1) })
  burden <- reactive(inflation_burden(rates(), weights()))
  output$p_rate <- renderText(fmt_pct(burden()$personal$inflation_pct))
  output$p_extra <- renderText(fmt_inr(personal()$extra))
  output$p_proj <- renderText(fmt_inr(personal()$projected))
  output$personal_table <- renderTable({
    x <- personal()$category
    data.frame(Category = x$category, `Share (%)` = 100 * weights()[x$category], `Assumed rate (%)` = rates()[x$category],
               `Contribution (pp)` = weights()[x$category] * rates()[x$category],
               `Current (INR)` = x$current, `After 1 year (INR)` = x$projected, check.names = FALSE)
  }, digits = 2)
  output$contrib_plot <- renderPlotly({
    b <- burden()$personal
    plot_ly(x = names(b$contributions), y = as.numeric(b$contributions), type = "bar",
            marker = list(color = unname(category_col[names(b$contributions)])),
            text = sprintf("%.2f pp", b$contributions), textposition = "outside", cliponaxis = FALSE,
            hovertemplate = "%{x}: %{y:.2f} pp<extra></extra>") |>
      plot_layout(NULL, "Percentage points", bargap = 0.5, showlegend = FALSE) |> layout(hovermode = "closest")
  })
  output$burden_plot <- renderPlotly({
    b <- burden()
    v <- c(You = b$personal$inflation_pct, `Rural basket` = b$rural$inflation_pct, `Urban basket` = b$urban$inflation_pct)
    plot_ly(x = factor(names(v), names(v)), y = v, type = "bar", marker = list(color = c(brand$zinc, sector_col[["Rural"]], sector_col[["Urban"]])),
            text = fmt_pct(v), textposition = "outside", cliponaxis = FALSE, hovertemplate = "%{x}: %{y:.2f}%<extra></extra>") |>
      plot_layout(NULL, "Modelled annual basket inflation (%)", bargap = 0.5, showlegend = FALSE) |> layout(hovermode = "closest")
  })

  # Stress test
  stress_weights <- reactive({
    if (input$stress_profile == "Personal") weights()
    else { f <- food_benchmark(input$stress_profile) / 100; c(Food = f, `Non-food` = 1 - f) }
  })
  all_presets <- reactive(c(stress_presets, list(Custom = c(Food = input$custom_food, `Non-food` = input$custom_other))))
  stress_run <- function(r, years) simulate_budget(input$stress_spend, stress_weights(), r, years, input$available)
  stressed <- reactive({ req(is.numeric(input$stress_spend), is.numeric(input$available), is.numeric(input$custom_food), is.numeric(input$custom_other))
    stress_run(all_presets()[[input$scenario]], input$horizon) })
  output$s_proj <- renderText(fmt_inr(stressed()$projected))
  output$s_extra <- renderText(fmt_inr(stressed()$extra))
  output$s_gap <- renderText({ g <- stressed()$budget_gap; paste(if (g >= 0) "Surplus" else "Shortfall", fmt_inr(abs(g))) })
  output$stress_plot <- renderPlotly({
    stressed()
    p <- plot_ly()
    for (nm in names(all_presets())) {
      y <- vapply(0:5, function(t) stress_run(all_presets()[[nm]], t)$projected, numeric(1))
      hl <- nm == input$scenario
      p <- add_trace(p, x = 0:5, y = y, type = "scatter", mode = "lines+markers", name = nm,
                     line = list(color = scenario_col[[nm]], width = if (hl) 3.5 else 2),
                     marker = list(color = scenario_col[[nm]], size = if (hl) 10 else 8), opacity = if (hl) 1 else 0.75,
                     hovertemplate = paste0(nm, ": ₹%{y:,.0f}<extra></extra>"))
    }
    p <- add_trace(p, x = c(0, 5), y = rep(input$available, 2), type = "scatter", mode = "lines", name = "Available budget",
                   line = list(color = brand$muted, dash = "dot", width = 2), hovertemplate = "Budget: ₹%{y:,.0f}<extra></extra>")
    plot_layout(p, "Years ahead", "INR per month") |> layout(shapes = list(list(type = "line", x0 = input$horizon, x1 = input$horizon, y0 = 0, y1 = 1, yref = "paper", line = list(color = brand$grid, width = 1))))
  })
  output$stress_table <- renderTable({
    stressed()
    do.call(rbind, lapply(names(all_presets()), function(nm) {
      r <- all_presets()[[nm]]; x <- stress_run(r, input$horizon)
      data.frame(Scenario = nm, `Food (%)` = r[["Food"]], `Non-food (%)` = r[["Non-food"]], `Projected (INR)` = x$projected,
                 `Extra (INR)` = x$extra, `Change (%)` = x$change_pct, `Budget gap (INR)` = x$budget_gap,
                 Status = if (x$budget_gap >= 0) "Within budget" else "Shortfall", check.names = FALSE)
    }))
  }, digits = 2)

  # Scenario explorer
  scen_data <- reactive({
    if (is.null(input$sc_file)) return(bundled_scenarios)
    tryCatch(get_scenario_data(input$sc_file$datapath), error = function(e) {
      showNotification(paste("Scenario file rejected:", conditionMessage(e)), type = "error", duration = 8)
      bundled_scenarios
    })
  })
  output$sc_source <- renderText(if (is.null(input$sc_file)) "Using bundled 10,000 scenarios." else paste("Loaded", nrow(scen_data()), "rows."))
  output$sc_filters <- renderUI({
    d <- scen_data()
    rng <- function(id, label, v, step) sliderInput(id, label, min(v), max(v), c(min(v), max(v)), step = step)
    tagList(
      rng("sc_spend", "Monthly spending (INR)", d$monthly_spend, 1000),
      rng("sc_food", "Food inflation (%)", d$food_inflation, 1),
      rng("sc_nonfood", "Non-food inflation (%)", d$nonfood_inflation, 1),
      checkboxGroupInput("sc_years", "Years ahead", sort(unique(d$years)), selected = sort(unique(d$years)), inline = TRUE))
  })
  scen <- reactive({
    d <- scen_data(); req(input$sc_spend, input$sc_food, input$sc_nonfood)
    d[d$profile %in% input$sc_profile & d$years %in% as.numeric(input$sc_years) &
      d$monthly_spend >= input$sc_spend[1] & d$monthly_spend <= input$sc_spend[2] &
      d$food_inflation >= input$sc_food[1] & d$food_inflation <= input$sc_food[2] &
      d$nonfood_inflation >= input$sc_nonfood[1] & d$nonfood_inflation <= input$sc_nonfood[2], , drop = FALSE]
  })
  has_rows <- function() validate(need(nrow(scen()) > 0, "No scenarios match these filters."))
  output$sc_n <- renderText(formatC(nrow(scen()), format = "d", big.mark = ","))
  output$sc_rate <- renderText({ has_rows(); fmt_pct(mean(scen()$basket_inflation)) })
  output$sc_extra <- renderText({ has_rows(); fmt_inr(mean(scen()$extra_cost)) })
  output$sc_short <- renderText({ has_rows(); fmt_pct(100 * mean(scen()$budget_gap < 0), 1) })
  output$sc_heat <- renderPlotly({
    has_rows(); d <- scen()
    z <- tapply(d$budget_gap < 0, list(d$nonfood_inflation, d$food_inflation), function(v) 100 * mean(v))
    plot_ly(x = as.numeric(colnames(z)), y = as.numeric(rownames(z)), z = z, type = "heatmap", colorscale = seq_scale,
            zmin = 0, zmax = 100, xgap = 2, ygap = 2, colorbar = list(title = "% short", ticksuffix = "%"),
            hovertemplate = "Food %{x}% | Non-food %{y}%<br>Shortfall in %{z:.0f}% of scenarios<extra></extra>") |>
      plot_layout("Assumed food inflation (%)", "Assumed non-food inflation (%)") |> layout(hovermode = "closest")
  })
  output$sc_years <- renderPlotly({
    has_rows(); s <- summarise_scenarios(scen(), c("profile", "years"))
    p <- plot_ly()
    for (pr in intersect(c("Rural", "Urban"), unique(s$profile))) {
      x <- s[s$profile == pr, ]
      p <- add_trace(p, x = x$years, y = x$mean_extra_cost, type = "scatter", mode = "lines+markers", name = pr,
                     line = list(color = sector_col[[pr]], width = 2), marker = list(color = sector_col[[pr]], size = 8),
                     hovertemplate = paste0(pr, ": ₹%{y:,.0f}<extra></extra>"))
    }
    plot_layout(p, "Years ahead", "Mean extra cost (INR per month)") |> layout(xaxis = list(dtick = 1))
  })
  output$sc_spend <- renderPlotly({
    has_rows(); s <- summarise_scenarios(scen(), c("profile", "monthly_spend"))
    p <- plot_ly()
    for (pr in intersect(c("Rural", "Urban"), unique(s$profile))) {
      x <- s[s$profile == pr, ]
      p <- add_trace(p, x = x$monthly_spend, y = x$mean_extra_cost, type = "scatter", mode = "lines+markers", name = pr,
                     line = list(color = sector_col[[pr]], width = 2), marker = list(color = sector_col[[pr]], size = 8),
                     hovertemplate = paste0(pr, ": ₹%{y:,.0f}<extra></extra>"))
    }
    plot_layout(p, "Current monthly spending (INR)", "Mean extra cost (INR per month)")
  })
  output$sc_summary <- renderTable({
    has_rows(); s <- summarise_scenarios(scen(), c("profile", "years"))
    names(s) <- c("Profile", "Years", "Scenarios", "Mean basket inflation (%)", "Mean extra cost (INR)", "Median budget gap (INR)", "Shortfall (%)")
    s
  }, digits = 2)
  output$sc_table <- renderDT(datatable(scen()[, c("scenario_id", "profile", "monthly_spend", "food_inflation", "nonfood_inflation", "years",
                                                   "basket_inflation", "projected_cost", "extra_cost", "budget_gap")],
    rownames = FALSE,
    colnames = c("ID", "Profile", "Spend (INR)", "Food (%)", "Non-food (%)", "Years", "Basket inflation (%)", "Projected (INR)", "Extra (INR)", "Budget gap (INR)"),
    options = list(pageLength = 10, scrollX = TRUE)) |> formatRound(c("basket_inflation"), 2) |> formatRound(c("projected_cost", "extra_cost", "budget_gap"), 0))
  output$sc_download <- downloadHandler(filename = function() paste0("inflascope-scenarios-", Sys.Date(), ".csv"),
                                        content = function(file) write.csv(scen(), file, row.names = FALSE))

  # Forecasting
  forecast_data <- reactive({
    if (is.null(input$forecast_file)) return(chosen_data())
    f <- read.csv(input$forecast_file$datapath, stringsAsFactors = FALSE)
    f$date <- as.Date(f$date); f$index <- as.numeric(f$index); f$inflation_yoy <- as.numeric(f$inflation_yoy)
    validate_cpi_data(f); f
  })
  output$forecast_status <- renderTable({
    d <- tryCatch(forecast_data(), error = function(e) NULL)
    validate(need(!is.null(d), "The uploaded file failed CPI validation."))
    x <- d[d$sector == input$forecast_sector & d$category == "General", ]
    m <- as.integer(format(x$date, "%Y")) * 12L + as.integer(format(x$date, "%m"))
    continuous <- length(m) > 1 && all(diff(sort(m)) == 1L)
    data.frame(Check = c("Observations", "First month", "Last month", "Continuous monthly series", "Minimum needed"),
               Value = c(nrow(x), format(min(x$date), "%Y-%m"), format(max(x$date), "%Y-%m"), if (continuous) "Yes" else "No (months missing)", "18 consecutive months"))
  })
  forecast_result <- reactive(tryCatch(forecast_inflation(forecast_data(), input$forecast_sector, input$forecast_horizon, input$forecast_model), error = function(e) e))
  output$forecast_message <- renderUI({
    v <- forecast_result()
    if (inherits(v, "error")) div(class = "alert alert-warning mb-0", strong("Forecast not available. "), conditionMessage(v),
      p(class = "mb-0 mt-1", "Forecasting needs at least 18 consecutive months of the same CPI base. Import a longer official series to enable it."))
    else div(class = "alert alert-info mb-0", "Forecasts are statistical CPI index-level predictions, not observed prices or YoY inflation rates. The 2024-base series is short (it starts in January 2025), so treat intervals as indicative.")
  })
  output$forecast_plot <- renderPlotly({
    x <- forecast_result()
    validate(need(!inherits(x, "error"), "No forecast to plot. See Series readiness above."))
    h <- forecast_data(); h <- h[h$sector == input$forecast_sector & h$category == "General", ]; h <- h[order(h$date), ]
    plot_ly() |>
      add_ribbons(x = x$date, ymin = x$lower95, ymax = x$upper95, name = "95% interval", fillcolor = "rgba(203,75,47,0.15)", line = list(width = 0)) |>
      add_ribbons(x = x$date, ymin = x$lower80, ymax = x$upper80, name = "80% interval", fillcolor = "rgba(203,75,47,0.30)", line = list(width = 0)) |>
      add_trace(x = h$date, y = h$index, type = "scatter", mode = "lines", name = "Observed", line = list(color = brand$zinc, width = 2)) |>
      add_trace(x = x$date, y = x$cpi, type = "scatter", mode = "lines+markers", name = "Forecast", line = list(color = brand$sienna, width = 2, dash = "dash"),
                marker = list(color = brand$sienna, size = 8)) |>
      plot_layout("Month", "CPI index (2024=100)")
  })
  output$forecast_table <- renderTable({ x <- forecast_result(); if (inherits(x, "error")) return(NULL)
    data.frame(Month = format(x$date, "%b %Y"), Forecast = x$cpi, `80% low` = x$lower80, `80% high` = x$upper80,
               `95% low` = x$lower95, `95% high` = x$upper95, check.names = FALSE) }, digits = 2)
  accuracy <- reactive(tryCatch(evaluate_forecast(forecast_data(), input$forecast_sector), error = function(e) e))
  output$accuracy_message <- renderUI({ a <- accuracy(); if (inherits(a, "error")) p(class = "note", conditionMessage(a)) })
  output$accuracy_table <- renderTable({ a <- accuracy(); if (inherits(a, "error")) return(NULL)
    data.frame(Model = a$model, MAE = a$mae, RMSE = a$rmse, `Train months` = as.integer(a$train_months), `Test months` = as.integer(a$test_months), check.names = FALSE) }, digits = 3)
  output$forecast_download <- downloadHandler(filename = function() paste0("inflascope-forecast-", Sys.Date(), ".csv"),
    content = function(file) { x <- forecast_result(); if (inherits(x, "error")) stop(conditionMessage(x)); write.csv(x, file, row.names = FALSE) })

  # Research & data
  output$all_data <- renderDT(datatable(transform(chosen_data(), date = format(date, "%Y-%m-%d")), rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10)))
  import_message <- reactiveVal("No import attempted.")
  observeEvent(input$import_btn, {
    if (is.null(input$import_file)) { import_message("Choose a CSV file first."); return() }
    result <- tryCatch({ import_cpi_csv(input$import_file$datapath); cache_version(cache_version() + 1)
      "Validated data saved to the local cache. Choose 'Best validated available' to use it." }, error = function(e) paste("Import rejected:", conditionMessage(e)))
    import_message(result)
  })
  output$import_status <- renderText(import_message())
  output$dictionary <- renderTable(data.frame(
    Column = c("date", "sector", "category", "index", "inflation_yoy", "base_year", "release_date", "source_url", "status", "dataset_version"),
    Meaning = c("First day of reference month (YYYY-MM-01)", "Rural, Urban or Combined", "CPI group (General)", "CPI index level, positive",
                "Published YoY inflation (%), NA if unpublished", "Must be 2024=100", "Official release date", "Official HTTPS source page",
                "final or provisional", "Snapshot identifier")))
  download_csv <- function(datafun) downloadHandler(filename = function() paste0("inflascope-cpi-", Sys.Date(), ".csv"),
                                                    content = function(file) write.csv(datafun(), file, row.names = FALSE))
  output$download_cpi <- download_csv(filtered)
  output$all_download <- download_csv(chosen_data)

  # Report
  output$report <- downloadHandler(filename = function() paste0("inflascope-report-", Sys.Date(), ".html"), content = function(file) {
    esc <- htmltools::htmlEscape
    tbl <- function(df) {
      df[] <- lapply(df, function(v) if (is.numeric(v)) format(round(v, 2), big.mark = ",", nsmall = 0, scientific = FALSE) else esc(as.character(v)))
      paste0("<table><thead><tr>", paste0("<th>", esc(names(df)), "</th>", collapse = ""), "</tr></thead><tbody>",
             paste0(apply(df, 1, function(r) paste0("<tr>", paste0("<td>", r, "</td>", collapse = ""), "</tr>")), collapse = ""), "</tbody></table>")
    }
    cmp <- comparison(); st <- stressed(); pb <- burden(); sc <- scen()
    lt <- latest()[, c("date", "sector", "index", "inflation_yoy", "release_date")]; lt$date <- format(lt$date, "%Y-%m")
    stress_tab <- do.call(rbind, lapply(names(all_presets()), function(nm) { x <- stress_run(all_presets()[[nm]], input$horizon)
      data.frame(Scenario = nm, Projected = x$projected, Extra = x$extra, `Budget gap` = x$budget_gap, check.names = FALSE) }))
    html <- c("<!DOCTYPE html><html lang='en'><head><meta charset='utf-8'><title>INFLASCOPE India report</title><style>",
      "body{font-family:Inter,system-ui,sans-serif;background:#FAFAFA;color:#18181B;max-width:900px;margin:2rem auto;padding:0 1rem;line-height:1.5}",
      "h1{border-bottom:3px solid #CB4B2F;padding-bottom:.4rem}h2{color:#CB4B2F;margin-top:2rem}",
      "table{border-collapse:collapse;width:100%;background:#fff;margin:.5rem 0}th,td{border:1px solid #E4E4E7;padding:.35rem .6rem;text-align:left}th{background:#18181B;color:#FAFAFA}",
      ".tag{display:inline-block;font-size:.75rem;font-weight:600;padding:.1rem .5rem;border-radius:999px;background:#FBEAE5;color:#8E2F1B}.obs{background:#18181B;color:#FAFAFA}",
      "</style></head><body>",
      "<h1>INFLASCOPE INDIA: rural vs urban inflation summary</h1>",
      sprintf("<p>Generated %s. Data source: %s. CPI base %s.</p>", format(Sys.time(), "%Y-%m-%d %H:%M"), esc(attr(chosen_data(), "data_source") %||% "User-supplied"), esc(cmp$base_year)),
      "<h2>1. Observed CPI inflation</h2><span class='tag obs'>Observed official data</span>",
      sprintf("<p>%s Rural: %.2f%%, urban: %.2f%%, gap %+.2f percentage points.</p>", esc(generate_insights(cmp)), cmp$rural, cmp$urban, cmp$difference),
      tbl(lt),
      { dv <- chosen_data(); dv <- dv[dv$date == cmp$date & dv$sector %in% c("Rural", "Urban") & !is.na(dv$inflation_yoy), ]
        if (nrow(dv)) { w <- reshape(dv[, c("category", "sector", "inflation_yoy")], idvar = "category", timevar = "sector", direction = "wide")
          names(w) <- c("CPI division", "Rural YoY (%)", "Urban YoY (%)"); paste0("<p>Division-level inflation, ", format(cmp$date, "%B %Y"), ":</p>", tbl(w)) } else "" },
      "<h2>2. Consumption benchmarks (HCES 2023-24)</h2><span class='tag obs'>Observed official data</span>",
      tbl(data.frame(Sector = c("Rural", "Urban"), `MPCE (INR)` = c(4122, 6996), `Food share (%)` = c(47.04, 39.68), check.names = FALSE)),
      "<h2>3. Personal basket</h2><span class='tag'>Hypothetical calculation</span>",
      sprintf("<p>Food share %d%%, assumed food inflation %.1f%%, non-food %.1f%%. Modelled basket inflation %.2f%% (rural basket %.2f%%, urban basket %.2f%%).</p>",
              input$food_pct, input$food_rate, input$other_rate, pb$personal$inflation_pct, pb$rural$inflation_pct, pb$urban$inflation_pct),
      "<h2>4. Stress test</h2><span class='tag'>Hypothetical calculation</span>",
      sprintf("<p>Basket cost %s per month, available budget %s, horizon %d years. Highlighted scenario: %s.</p>", fmt_inr(input$stress_spend), fmt_inr(input$available), input$horizon, esc(input$scenario)),
      tbl(stress_tab),
      "<h2>5. Scenario Explorer selection</h2><span class='tag'>Hypothetical calculation</span>",
      sprintf("<p>%d synthetic scenarios selected; %.1f%% show a budget shortfall.</p>", nrow(sc), if (nrow(sc)) 100 * mean(sc$budget_gap < 0) else NA),
      if (nrow(sc)) tbl(summarise_scenarios(sc, c("profile", "years"))) else "",
      "<h2>6. Assumptions and limitations</h2><ul>",
      "<li>Simulations assume fixed quantities and annually compounded category price changes; no substitution or behavioural response.</li>",
      "<li>HCES shares are consumption benchmarks, not official CPI weights.</li>",
      "<li>The bundled CPI snapshot covers selected months only and does not support time-series forecasting.</li>",
      "<li>Comparisons are descriptive and do not establish causal effects of inflation on spending.</li></ul>",
      "<p>Sources: <a href='https://esankhyiki.mospi.gov.in/macroindicators?product=cpi'>MoSPI CPI</a>; <a href='https://mospi.gov.in/sites/default/files/publication_reports/HCES%20FactSheet%202023-24.pdf'>HCES 2023-24 fact sheet</a>.</p>",
      "</body></html>")
    writeLines(enc2utf8(html), file, useBytes = TRUE)
  })
}

shinyApp(ui, server)
