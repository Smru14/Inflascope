library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(DT)
library(inflascope)

h <- get_static_data("hces")
ui <- page_navbar(
  title = "INFLASCOPE INDIA", theme = bs_theme(version = 5, bootswatch = "flatly", primary="#173b60"),
  header = div(class="container-fluid py-2", p("Rural and urban inflation intelligence | All India | Official source snapshots and transparent simulations")),
  nav_panel("Overview",
    layout_columns(value_box("Latest rural YoY",textOutput("rural_kpi")),
                   value_box("Latest urban YoY",textOutput("urban_kpi")),
                   value_box("Rural minus urban",textOutput("gap_kpi")),col_widths=c(4,4,4)),
    card(card_header("Rural vs urban CPI inflation"),plotlyOutput("trend")),
    card(card_header("Observed statistics and provenance"),verbatimTextOutput("provenance"))),
  nav_panel("Historical Explorer",
    layout_sidebar(sidebar=sidebar(selectInput("sector","Sector",c("Both","Rural","Urban","Combined")),
                                   radioButtons("measure","Metric",c("Year-on-year inflation (%)"="inflation_yoy","CPI index level"="index")),
                                   dateRangeInput("range","Reference months",start="2026-01-01",end="2026-09-01"),
                                   downloadButton("download_cpi","Download filtered CSV")),
    card(plotlyOutput("historic_plot")),card(DTOutput("historic_table")))),
  nav_panel("Rural & Urban Consumption",
    card(card_header("Official 2023–24 HCES benchmarks (without free-item imputation)"),
         tableOutput("hces_table"),plotOutput("hces_plot"),
         p("MPCE is a per-person national average, not a typical household's monthly spending."))),
  nav_panel("Personal Inflation Calculator",
    layout_sidebar(sidebar=sidebar(selectInput("profile","Benchmark",c("Rural","Urban")),
       numericInput("monthly","Current monthly household spending (₹)",25000,min=0),
       sliderInput("food_pct","Food spending share (%)",0,100,47),
       numericInput("food_rate","Assumed annual food inflation (%)",6,min=-99,max=100),
       numericInput("other_rate","Assumed annual non-food inflation (%)",4,min=-99,max=100),
       actionButton("reset","Reset share to HCES benchmark")),
       card(card_header("Hypothetical fixed-basket results"),tableOutput("personal_table"),
            plotOutput("contrib_plot"),verbatimTextOutput("personal_summary")))),
  nav_panel("Stress Test & Budget Resilience",
    layout_sidebar(sidebar=sidebar(numericInput("stress_spend","Current basket cost (₹)",25000,min=0),
              numericInput("available","Available monthly budget (₹)",28000,min=0),
              sliderInput("horizon","Years ahead",1,5,1),
              selectInput("scenario","Scenario",c("Moderate"="moderate","Low"="low","Food shock"="food","Non-food shock"="other","Custom"="custom")),
              numericInput("custom_food","Custom food inflation (%)",8,min=-99,max=100),
              numericInput("custom_other","Custom non-food inflation (%)",5,min=-99,max=100)),
      card(card_header("Hypothetical price-growth scenario"),tableOutput("stress_table"),
           plotOutput("stress_plot"),verbatimTextOutput("stress_summary")))),
  nav_panel("Forecasting Studio",
    layout_sidebar(sidebar=sidebar(selectInput("forecast_sector","Sector",c("Rural","Urban")),
            sliderInput("forecast_horizon","Months ahead",1,12,6),
            selectInput("forecast_model","Model",c("ARIMA"="arima","Seasonal naive"="snaive")),
            fileInput("forecast_file","Import a sufficiently long official CPI CSV",accept=".csv"),
            downloadButton("forecast_download","Download forecast CSV")),
      card(card_header("CPI index forecast and confidence intervals"),uiOutput("forecast_message"),plotOutput("forecast_plot"),
           tableOutput("forecast_table"),tableOutput("accuracy_table")))),
  nav_panel("Research & Data",
      card(card_header("Data selection"),selectInput("data_mode","CPI data source",c("Bundled historical snapshot"="static","Validated cached data"="cache","Best validated available"="auto")),
           fileInput("import_file","Import validated, canonical official CPI CSV",accept=".csv"),
           actionButton("import_btn","Validate and save cache"),textOutput("import_status"),
           p("Automatic network updates are disabled until a reliable official endpoint is verified.")),
      card(card_header("CPI dataset and dictionary"),DTOutput("all_data"),downloadButton("all_download","Download active data")),
      card(card_header("Research notes"),tags$p("Inflation is a change in prices; HCES expenditure shares describe spending composition, not causal behavioural responses."),
      tags$p("Only compatible CPI base 2024=100 observations are combined. Missing observation months are left missing."),
      tags$a(href="https://esankhyiki.mospi.gov.in/macroindicators?product=cpi", "Official CPI portal"),
      tags$br(),tags$a(href="https://mospi.gov.in/sites/default/files/publication_reports/HCES%20FactSheet%202023-24.pdf", "Official HCES fact sheet"))),
  nav_panel("Export Report",card(card_header("Reproducible downloadable summary"),
      p("Includes CPI comparison, household simulation, and assumptions."),
      downloadButton("report","Download analysis report (.html if R Markdown is installed; otherwise .txt)")))
)
server <- function(input, output, session) {
  chosen_data <- reactive({
    if (input$data_mode == "cache") tryCatch(get_cpi_data("cache"),error=function(e) get_cpi_data("static"))
    else get_cpi_data(input$data_mode)
  })
  filtered <- reactive({
    d <- chosen_data(); req(input$range)
    d <- d[d$date>=as.Date(input$range[1]) & d$date<=as.Date(input$range[2]) & d$category=="General",]
    if(input$sector!="Both") d <- d[d$sector==input$sector,]
    d
  })
  latest <- reactive(get_latest_cpi(chosen_data()))
  output$rural_kpi <- renderText(sprintf("%.2f%%",latest()$inflation_yoy[latest()$sector=="Rural"]))
  output$urban_kpi <- renderText(sprintf("%.2f%%",latest()$inflation_yoy[latest()$sector=="Urban"]))
  output$gap_kpi <- renderText(sprintf("%+.2f pp",compare_rural_urban(data=chosen_data())$difference))
  trendplot <- function(d,measure) {
    ggplot(d,aes(x=date,y=.data[[measure]],color=sector,group=sector))+
      geom_line(linewidth=1)+geom_point(size=2)+labs(x="Reference month",y=if(measure=="inflation_yoy") "YoY inflation (%)" else "Index level",color="Sector")+
      theme_minimal(base_size=13)
  }
  output$trend <- renderPlotly(ggplotly(trendplot(chosen_data()[chosen_data()$sector!="Combined",],"inflation_yoy")))
  output$historic_plot <- renderPlotly({validate(need(nrow(filtered())>0,"No verified records for selection"));ggplotly(trendplot(filtered(),input$measure))})
  output$historic_table <- renderDT(filtered(),options=list(pageLength=10,scrollX=TRUE))
  output$provenance <- renderPrint({ print(get_data_metadata(chosen_data()));cat("Latest YoY values are published, not computed from truncated snapshot rows.\n") })
  output$hces_table <- renderTable({
    data.frame(Sector=c("Rural","Urban"),`MPCE (₹ / person / month)`=c(4122,6996),
      `Food share (%)`=c(47.04,39.68),`Non-food share (%)`=c(52.96,60.32),check.names=FALSE)
  })
  output$hces_plot <- renderPlot(ggplot(h,aes(x=sector,y=100*share,fill=category))+geom_col()+
      labs(x=NULL,y="Percent of MPCE",fill="Spending category")+theme_minimal())
  observeEvent(input$profile,{
    updateSliderInput(session,"food_pct",value=round(100*h$share[h$sector==input$profile & h$category=="Food"]))
  })
  observeEvent(input$reset,{
    updateSliderInput(session,"food_pct",value=round(100*h$share[h$sector==input$profile & h$category=="Food"]))
  })
  weights <- reactive(c("Food"=input$food_pct/100,"Non-food"=1-input$food_pct/100))
  rates <- reactive(c("Food"=input$food_rate,"Non-food"=input$other_rate))
  personal <- reactive(simulate_budget(input$monthly, weights(),rates()))
  output$personal_table <- renderTable(personal()$category,digits=2)
  output$contrib_plot <- renderPlot({
    
    x <- personal_inflation(weights(), rates())
    
    df <- data.frame(
      Category = names(x$contributions),
      Contribution = as.numeric(x$contributions)
    )
    
    ggplot(df, aes(
      x = Category,
      y = Contribution,
      fill = Category
    )) +
      geom_col(width = 0.55) +
      labs(
        title = "Inflation Contribution by Category",
        x = NULL,
        y = "Contribution (percentage points)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 13),
        plot.margin = margin(10, 10, 10, 10)
      )
    
  })
  output$personal_summary <- renderPrint({x<-personal();b<-inflation_burden(rates(),weights());
    cat(sprintf("Modeled annual basket inflation: %.2f%%\nAdded monthly cost: ₹%.2f\nRural benchmark basket estimate: %.2f%%\nUrban benchmark basket estimate: %.2f%%\n",b$personal$inflation_pct,x$extra,b$rural$inflation_pct,b$urban$inflation_pct))})
  scenario_rates <- reactive(switch(input$scenario,low=c(Food=2,`Non-food`=2),moderate=c(Food=5,`Non-food`=4),
    food=c(Food=15,`Non-food`=4),other=c(Food=4,`Non-food`=12),custom=c(Food=input$custom_food,`Non-food`=input$custom_other)))
  stressed <- reactive(simulate_budget(input$stress_spend,weights(),scenario_rates(),input$horizon,input$available))
  output$stress_table <- renderTable(stressed()$category,digits=2)
  output$stress_plot <- renderPlot({
    
    x <- stressed()
    
    df <- data.frame(
      Category = rep(x$category$category, 2),
      Cost = c(
        x$category$current,
        x$category$projected
      ),
      Period = rep(
        c("Current", "Projected"),
        each = nrow(x$category)
      )
    )
    
    ggplot(df, aes(
      x = Category,
      y = Cost,
      fill = Period
    )) +
      geom_col(
        position = "dodge",
        width = 0.65
      ) +
      labs(
        title = "Household Expenditure Comparison",
        x = NULL,
        y = "Monthly expenditure (INR)",
        fill = "Period"
      ) +
      scale_y_continuous(
        labels = scales::label_comma()
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 13),
        plot.margin = margin(10, 10, 10, 10),
        legend.position = "bottom"
      )
    
  })
  output$stress_summary <- renderPrint({x<-stressed();cat(sprintf("Projected monthly cost: ₹%.2f\nAdditional monthly cost: ₹%.2f\nBudget surplus (+) or shortfall (-): ₹%.2f\n%s\n",
       x$projected,x$extra,x$budget_gap,x$assumptions))})
  forecast_data <- reactive({ if(is.null(input$forecast_file)) return(chosen_data())
    f<-read.csv(input$forecast_file$datapath,stringsAsFactors=FALSE)
    f$date<-as.Date(f$date);f$index<-as.numeric(f$index);f$inflation_yoy<-as.numeric(f$inflation_yoy)
    validate_cpi_data(f); f
  })
  forecast_result <- reactive(tryCatch(forecast_inflation(forecast_data(),input$forecast_sector,input$forecast_horizon,input$forecast_model),error=function(e)e))
  output$forecast_message <- renderUI({v<-forecast_result();if(inherits(v,"error")) div(class="alert alert-warning",conditionMessage(v))
    else div(class="alert alert-info","Forecasts are statistical CPI index-level predictions, not observed prices or YoY inflation rates.")})
  output$forecast_plot <- renderPlot({x<-forecast_result();req(!inherits(x,"error"));ggplot(x,aes(x=date,y=cpi))+
    geom_ribbon(aes(ymin=lower95,ymax=upper95),fill="lightblue",alpha=.5)+geom_line(linewidth=1)+
    labs(x="Forecast month",y="Projected CPI index")+theme_minimal()})
  output$forecast_table <- renderTable({x<-forecast_result();if(inherits(x,"error"))return(NULL);x},digits=2)
  output$accuracy_table <- renderTable({tryCatch(evaluate_forecast(forecast_data(),input$forecast_sector),error=function(e) NULL)},digits=3)
  output$all_data <- renderDT(chosen_data(),options=list(scrollX=TRUE,pageLength=10))
  import_message <- reactiveVal("No import attempted")
  observeEvent(input$import_btn,{
    req(input$import_file)
    result<-tryCatch({import_cpi_csv(input$import_file$datapath);"Validated data saved in local cache. Select best available to view."},error=function(e) conditionMessage(e))
    import_message(result)
  })
  output$import_status <- renderText(import_message())
  download_csv <- function(datafun) downloadHandler(filename=function() paste0("inflascope-",Sys.Date(),".csv"),
     content=function(file) write.csv(datafun(),file,row.names=FALSE))
  output$download_cpi <- download_csv(filtered)
  output$all_download <- download_csv(chosen_data)
  output$forecast_download <- download_csv(function(){x<-forecast_result();if(inherits(x,"error")) stop(conditionMessage(x));x})
  output$report <- downloadHandler(filename=function()"inflascope-report.html",content=function(file){
    result <- stressed();cmp <- compare_rural_urban(data=chosen_data())
    fmt <- function(x) format(round(x,2),big.mark=",",scientific=FALSE)
    lines <- c("<!DOCTYPE html><html><head><meta charset='utf-8'><title>INFLASCOPE Report</title></head><body>",
      "<h1>INFLASCOPE INDIA — Research and household scenario summary</h1>",
      sprintf("<p>Reference period: %s; index base: %s; data source: %s</p>",cmp$date,cmp$base_year,attr(chosen_data(),"data_source")),
      sprintf("<p>Published rural inflation: %s%%; urban: %s%%; rural minus urban: %s percentage points.</p>",fmt(cmp$rural),fmt(cmp$urban),fmt(cmp$difference)),
      sprintf("<p>Hypothetical monthly basket cost: ₹%s → ₹%s after %s years; budget gap: ₹%s.</p>",
         fmt(result$original),fmt(result$projected),input$horizon,fmt(result$budget_gap)),
      "<p>Simulation assumes fixed quantities and annual compounded category price changes, not household behavioural adjustments.</p>",
      "<p>Source: <a href='https://esankhyiki.mospi.gov.in/macroindicators?product=cpi'>MoSPI CPI</a>; HCES 2023–24.</p></body></html>")
    writeLines(lines,file,useBytes=TRUE)
  })
}
shinyApp(ui,server)
