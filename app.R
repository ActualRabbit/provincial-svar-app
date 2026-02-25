###-----------------------------------------------------------------------------
## Provincial Structural Vector Autoregression - Shiny App
## Thesis: Differential effects of monetary policy across Canadian provinces
###-----------------------------------------------------------------------------

library(shiny)
library(bslib)
library(vars)
library(cansim)
library(tidyverse)
library(plotly)

# =============================================================================
# DATA LOADING & CACHING
# =============================================================================

# Cache directory
cache_dir <- "cache"
if (!dir.exists(cache_dir)) dir.create(cache_dir)

load_or_cache <- function(name, loader_fn, max_age_days = 7) {
 cache_file <- file.path(cache_dir, paste0(name, ".rds"))
 
 if (file.exists(cache_file)) {
   file_age <- difftime(Sys.time(), file.mtime(cache_file), units = "days")
   if (file_age < max_age_days) {
     message(paste("Loading", name, "from cache"))
     return(readRDS(cache_file))
   }
 }
 
 message(paste("Fetching", name, "from source"))
 data <- loader_fn()
 saveRDS(data, cache_file)
 return(data)
}

# Data loaders
load_commodity_prices <- function() {
 read_csv("https://www.bankofcanada.ca/valet/observations/group/BCPI_MONTHLY/csv?start_date=1972-01-01",
          skip = 20, col_types = "Dd", show_col_types = FALSE) %>%
   mutate(Commodity_Index = log(M.BCPI) - log(lag(M.BCPI, 12))) %>%
   select(Date = date, Commodity_Index)
}

load_interest_rates <- function() {
 get_cansim("10-10-0139") %>%
   filter(`Financial market statistics` == "Treasury Bills, 1-month") %>%
   mutate(Year_Month_col = case_when(
     month(Date) >= 10 ~ paste0(year(Date), "-", month(Date)),
     month(Date) <= 9 ~ paste0(year(Date), "-0", month(Date))
   )) %>%
   group_by(Year_Month_col) %>%
   summarize(val_monthly_average = mean(VALUE, na.rm = TRUE), .groups = "drop") %>%
   mutate(
     T_Bill_1M_Rate = log(val_monthly_average) - log(dplyr::lag(val_monthly_average, 12)),
     Date = as.Date(paste0(Year_Month_col, "-01"))
   ) %>%
   select(Date, T_Bill_1M_Rate)
}

load_inflation <- function() {
 get_cansim("18-10-0004") %>%
   filter(`Products and product groups` == "All-items excluding food and energy",
          GEO == "Canada") %>%
   mutate(Core_Inflation = log(VALUE) - log(lag(VALUE, 12))) %>%
   select(Date, Core_Inflation)
}

load_output <- function() {
 Output_Old <- get_cansim("36-10-0393") %>%
   filter(`Seasonal adjustment` == "Unadjusted",
          `Prices` == "1997 constant dollars",
          `Special industry aggregations` == "Business sector industries") %>%
   mutate(GDP = log(VALUE) - log(lag(VALUE, 12))) %>%
   select(Date, GDP)
 
 Output_New <- get_cansim("36-10-0434") %>%
   filter(`Seasonal adjustment` == "Trading-day adjusted",
          `Prices` == "2017 constant prices",
          `North American Industry Classification System (NAICS)` == "All industries [T001]") %>%
   mutate(GDP = log(val_norm) - log(lag(val_norm, 12))) %>%
   select(Date, GDP)
 
 bind_rows(
   filter(Output_Old, Date <= as.Date("1997-12-01")),
   filter(Output_New, Date >= as.Date("1998-01-01"))
 )
}

load_provincial_employment <- function() {
 get_cansim("14-10-0017") %>%
   filter(`Labour force characteristics` == "Employment",
          Gender == "Total - Gender",
          `Age group` == "15 years and over",
          GEO != "Canada") %>%
   group_by(GEO) %>%
   mutate(
     Date = as.Date(paste0(REF_DATE, "-01")),
     Employment = log(VALUE) - log(lag(VALUE, 12))
   ) %>%
   select(Date, GEO, Employment) %>%
   arrange(GEO, Date) %>%
   ungroup()
}

load_national_employment <- function() {
 get_cansim("14-10-0017") %>%
   filter(`Labour force characteristics` == "Employment",
          Gender == "Total - Gender",
          `Age group` == "15 years and over",
          GEO == "Canada") %>%
   mutate(
     Date = as.Date(paste0(REF_DATE, "-01")),
     National_Employment = log(VALUE) - log(lag(VALUE, 12))
   ) %>%
   select(Date, National_Employment)
}

# Build combined datasets
build_datasets <- function() {
 message("Building datasets...")
 
 commodity <- load_or_cache("commodity_prices", load_commodity_prices)
 interest <- load_or_cache("interest_rates", load_interest_rates)
 inflation <- load_or_cache("inflation", load_inflation)
 output <- load_or_cache("output", load_output)
 prov_emp <- load_or_cache("provincial_employment", load_provincial_employment)
 nat_emp <- load_or_cache("national_employment", load_national_employment)
 
 # National data
 national_data <- commodity %>%
   left_join(interest, by = "Date") %>%
   left_join(inflation, by = "Date") %>%
   left_join(output, by = "Date") %>%
   left_join(nat_emp, by = "Date") %>%
   filter(Date >= as.Date("1992-01-01")) %>%
   na.omit()
 
 # Provincial data (with national variables)
 provincial_data <- prov_emp %>%
   inner_join(select(national_data, -National_Employment), by = "Date") %>%
   left_join(select(nat_emp, Date, National_Employment), by = "Date") %>%
   na.omit()
 
 list(
   national = national_data,
   provincial = provincial_data,
   provinces = sort(unique(provincial_data$GEO))
 )
}

# =============================================================================
# SVAR ESTIMATION
# =============================================================================

estimate_provincial_svar <- function(provincial_data, province, p_lags = 3) {
 # Force evaluation of parameters
 p_lags <- as.integer(p_lags)
 
 # Filter to province
 prov_df <- provincial_data %>%
   filter(GEO == province) %>%
   arrange(Date)
 
 # Build time series matrix (ordering: most exogenous to least)
 # Commodity -> Interest Rate -> Inflation -> GDP -> Employment
 start_year <- year(min(prov_df$Date))
 start_month <- month(min(prov_df$Date))
 
 ts_matrix <- ts(
   data.frame(
     Commodity_Index = prov_df$Commodity_Index,
     T_Bill_1M = prov_df$T_Bill_1M_Rate,
     Core_Inflation = prov_df$Core_Inflation,
     GDP = prov_df$GDP,
     Employment = prov_df$Employment
   ),
   start = c(start_year, start_month),
   frequency = 12
 )
 
 # A matrix for recursive identification (Cholesky)
 A_Matrix <- diag(5)
 A_Matrix[lower.tri(A_Matrix)] <- NA
 
 # Estimate VAR (use do.call to force argument evaluation - vars uses NSE)
 var_model <- do.call(vars::VAR, list(y = ts_matrix, p = p_lags, type = "both"))
 
 # Estimate SVAR
 svar_model <- do.call(vars::SVAR, list(
   x = var_model, Amat = A_Matrix, Bmat = NULL,
   max.iter = 100000, hessian = TRUE, estmethod = "direct"
 ))
 
 return(svar_model)
}

compute_irf <- function(svar_model, impulse, response, n.ahead = 24, ci = 0.66) {
 irf_result <- do.call(vars::irf, list(
   x = svar_model, impulse = impulse, response = response,
   n.ahead = n.ahead, ci = ci, boot = TRUE, runs = 200
 ))
 
 # Extract IRF data
 tibble(
   horizon = 0:n.ahead,
   irf = as.numeric(irf_result$irf[[1]]),
   lower = as.numeric(irf_result$Lower[[1]]),
   upper = as.numeric(irf_result$Upper[[1]])
 )
}

# =============================================================================
# UI
# =============================================================================

ui <- page_sidebar(
 title = "Provincial SVAR: Canadian Monetary Policy Transmission",
 theme = bs_theme(bootswatch = "flatly"),
 
 sidebar = sidebar(
   width = 300,
   
   h4("Model Settings"),
   
   selectInput("province", "Province",
               choices = NULL,  # Populated on load
               selected = NULL),
   
   selectInput("impulse", "Impulse (Shock)",
               choices = c("Interest Rate" = "T_Bill_1M",
                          "Commodity Prices" = "Commodity_Index",
                          "GDP" = "GDP",
                          "Inflation" = "Core_Inflation"),
               selected = "T_Bill_1M"),
   
   selectInput("response", "Response Variable",
               choices = c("Employment" = "Employment",
                          "GDP" = "GDP",
                          "Inflation" = "Core_Inflation",
                          "Interest Rate" = "T_Bill_1M"),
               selected = "Employment"),
   
   sliderInput("lag_order", "Lag Order (months)",
               min = 1, max = 12, value = 3, step = 1),
   
   sliderInput("horizon", "Forecast Horizon (months)",
               min = 12, max = 36, value = 24, step = 1),
   
   hr(),
   
   actionButton("estimate", "Estimate SVAR", class = "btn-primary w-100"),
   
   hr(),
   
   h5("Compare Provinces"),
   checkboxGroupInput("compare_provinces", "Select provinces to compare:",
                      choices = NULL),
   actionButton("compare", "Compare All Selected", class = "btn-secondary w-100"),
   
   hr(),
   
   p(class = "text-muted small",
     "Data: Statistics Canada CANSIM, Bank of Canada BCPI. ",
     "Model: Structural VAR with Cholesky identification.")
 ),
 
 navset_card_tab(
   nav_panel(
     "Impulse Response",
     plotlyOutput("irf_plot", height = "500px"),
     hr(),
     verbatimTextOutput("model_summary")
   ),
   
   nav_panel(
     "Provincial Comparison",
     plotlyOutput("comparison_plot", height = "600px"),
     hr(),
     tableOutput("comparison_table")
   ),
   
   nav_panel(
     "Credit Channels",
     fluidRow(
       column(6, plotlyOutput("narrow_channel_plot", height = "400px")),
       column(6, plotlyOutput("broad_channel_plot", height = "400px"))
     ),
     hr(),
     p("The narrow credit channel (manufacturing intensity) predicts interest rate sensitivity.",
       "The broad credit channel (firm size) does not appear to predict sensitivity in Canada.")
   ),
   
   nav_panel(
     "Data Explorer",
     selectInput("data_province", "Province", choices = NULL),
     plotlyOutput("data_plot", height = "500px")
   ),
   
   nav_panel(
     "About",
     includeMarkdown("about.md") %||%
       div(
         h3("Provincial SVAR Analysis"),
         p("This application investigates the differential effects of monetary policy ",
           "across Canadian provinces using Structural Vector Autoregression (SVAR)."),
         h4("Key Findings from Original Research"),
         tags$ul(
           tags$li("Ontario shows the largest employment response to interest rate shocks (-0.13%)"),
           tags$li("Alberta and British Columbia show negligible response to interest rates"),
           tags$li("Manufacturing intensity (narrow credit channel) predicts sensitivity"),
           tags$li("Firm size ratio (broad credit channel) does NOT predict sensitivity"),
           tags$li("Maximum effects occur 12-15 months after the monetary shock")
         ),
         h4("Model Specification"),
         p("Variables ordered from most to least exogenous:"),
         tags$ol(
           tags$li("Commodity Price Index (Bank of Canada BCPI)"),
           tags$li("Interest Rate (1-month T-Bill)"),
           tags$li("Core Inflation (CPI excluding food & energy)"),
           tags$li("GDP (real output)"),
           tags$li("Employment (provincial)")
         )
       )
   )
 )
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {
 
 # Load data on startup
 data <- reactiveVal(NULL)
 svar_results <- reactiveVal(list())
 comparison_results <- reactiveVal(NULL)
 
 # Initialize data
 observe({
   showNotification("Loading data from Statistics Canada...", type = "message", duration = NULL, id = "loading")
   
   datasets <- build_datasets()
   data(datasets)
   
   # Update UI choices
   updateSelectInput(session, "province", choices = datasets$provinces, selected = "Ontario")
   updateSelectInput(session, "data_province", choices = datasets$provinces, selected = "Ontario")
   updateCheckboxGroupInput(session, "compare_provinces", choices = datasets$provinces,
                            selected = c("Ontario", "Alberta", "British Columbia", "Quebec"))
   
   removeNotification("loading")
   showNotification("Data loaded successfully!", type = "message", duration = 3)
 })
 
 # Estimate SVAR when button clicked
 observeEvent(input$estimate, {
   req(data(), input$province)
   
   showNotification("Estimating SVAR model...", type = "message", duration = NULL, id = "estimating")
   
   tryCatch({
     svar <- estimate_provincial_svar(data()$provincial, input$province, input$lag_order)
     
     irf_data <- compute_irf(svar, input$impulse, input$response, input$horizon)
     
     svar_results(list(
       model = svar,
       irf = irf_data,
       province = input$province,
       impulse = input$impulse,
       response = input$response
     ))
     
     removeNotification("estimating")
     showNotification("SVAR estimated successfully!", type = "message", duration = 3)
     
   }, error = function(e) {
     removeNotification("estimating")
     showNotification(paste("Error:", e$message), type = "error", duration = 10)
   })
 })
 
 # IRF Plot
 output$irf_plot <- renderPlotly({
   req(svar_results()$irf)
   
   irf_data <- svar_results()$irf
   province <- svar_results()$province
   impulse <- svar_results()$impulse
   response <- svar_results()$response
   
   impulse_label <- switch(impulse,
                           "T_Bill_1M" = "Interest Rate",
                           "Commodity_Index" = "Commodity Prices",
                           "GDP" = "GDP",
                           "Core_Inflation" = "Inflation")
   
   response_label <- switch(response,
                            "Employment" = "Employment",
                            "GDP" = "GDP",
                            "Core_Inflation" = "Inflation",
                            "T_Bill_1M" = "Interest Rate")
   
   p <- ggplot(irf_data, aes(x = horizon)) +
     geom_ribbon(aes(ymin = lower, ymax = upper), fill = "steelblue", alpha = 0.3) +
     geom_line(aes(y = irf), color = "steelblue", linewidth = 1.2) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
     labs(
       title = paste(province, "-", impulse_label, "Shock →", response_label, "Response"),
       x = "Months after shock",
       y = paste("Response of", response_label),
       caption = "Shaded area: 66% confidence interval"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
   
   ggplotly(p) %>% layout(hovermode = "x unified")
 })
 
 # Model summary
 output$model_summary <- renderPrint({
   req(svar_results()$model)
   
   model <- svar_results()$model
   irf <- svar_results()$irf
   
   cat("SVAR Model Summary\n")
   cat("==================\n\n")
   cat("Province:", svar_results()$province, "\n")
   cat("Impulse:", svar_results()$impulse, "\n")
   cat("Response:", svar_results()$response, "\n\n")
   
   # Find max response
   max_idx <- which.min(irf$irf)
   if (abs(max(irf$irf)) > abs(min(irf$irf))) {
     max_idx <- which.max(irf$irf)
   }
   
   cat("Maximum Response:", round(irf$irf[max_idx], 4), "\n")
   cat("Occurs at month:", irf$horizon[max_idx], "\n")
 })
 
 # Provincial comparison
 observeEvent(input$compare, {
   req(data(), input$compare_provinces)
   
   showNotification("Computing provincial comparison...", type = "message", duration = NULL, id = "comparing")
   
   results <- list()
   
   for (prov in input$compare_provinces) {
     tryCatch({
       svar <- estimate_provincial_svar(data()$provincial, prov, input$lag_order)
       irf_data <- compute_irf(svar, input$impulse, input$response, input$horizon)
       irf_data$province <- prov
       results[[prov]] <- irf_data
     }, error = function(e) {
       message(paste("Error for", prov, ":", e$message))
     })
   }
   
   comparison_results(bind_rows(results))
   
   removeNotification("comparing")
   showNotification("Comparison complete!", type = "message", duration = 3)
 })
 
 # Comparison plot
 output$comparison_plot <- renderPlotly({
   req(comparison_results())
   
   impulse_label <- switch(input$impulse,
                           "T_Bill_1M" = "Interest Rate",
                           "Commodity_Index" = "Commodity Prices",
                           "GDP" = "GDP",
                           "Core_Inflation" = "Inflation")
   
   response_label <- switch(input$response,
                            "Employment" = "Employment",
                            "GDP" = "GDP",
                            "Core_Inflation" = "Inflation",
                            "T_Bill_1M" = "Interest Rate")
   
   p <- ggplot(comparison_results(), aes(x = horizon, y = irf, color = province)) +
     geom_line(linewidth = 1) +
     geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
     labs(
       title = paste("Provincial Comparison:", impulse_label, "Shock →", response_label),
       x = "Months after shock",
       y = paste("Response of", response_label),
       color = "Province"
     ) +
     theme_minimal() +
     theme(plot.title = element_text(face = "bold"))
   
   ggplotly(p) %>% layout(hovermode = "x unified")
 })
 
 # Comparison table
 output$comparison_table <- renderTable({
   req(comparison_results())
   
   comparison_results() %>%
     group_by(province) %>%
     summarize(
       `Max Response` = round(irf[which.min(abs(irf - min(irf)))], 4),
       `Month of Max` = horizon[which.min(irf)],
       `Response at 12m` = round(irf[horizon == 12], 4),
       `Response at 24m` = round(irf[horizon == min(24, max(horizon))], 4),
       .groups = "drop"
     ) %>%
     arrange(`Max Response`)
 })
 
 # Credit channel plots (using paper's data)
 output$narrow_channel_plot <- renderPlotly({
   # Manufacturing intensity data from paper (Figure 1)
   narrow_data <- tibble(
     province = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario",
                  "Quebec", "New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador"),
     manufacturing_pct = c(10.66, 8.66, 6.37, 12.58, 20.22, 20.01, 14.03, 10.15, 9.57, 5.99),
     max_response = c(-0.03, -0.02, -0.10, -0.08, -0.13, -0.10, -0.07, -0.10, -0.02, -0.13)
   )
   
   p <- ggplot(narrow_data, aes(x = manufacturing_pct, y = max_response, label = province)) +
     geom_point(size = 3, color = "steelblue") +
     geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50") +
     geom_text(hjust = -0.1, vjust = 0.5, size = 3) +
     labs(
       title = "Narrow Credit Channel",
       subtitle = "Manufacturing intensity predicts interest rate sensitivity",
       x = "Average % of output from manufacturing (1991-2008)",
       y = "Max employment response to interest rate shock"
     ) +
     theme_minimal() +
     xlim(5, 25)
   
   ggplotly(p)
 })
 
 output$broad_channel_plot <- renderPlotly({
   # Firm size data from paper (Figure 3)
   broad_data <- tibble(
     province = c("British Columbia", "Alberta", "Saskatchewan", "Manitoba", "Ontario",
                  "Quebec", "New Brunswick", "Nova Scotia", "Prince Edward Island", "Newfoundland and Labrador"),
     small_firm_ratio = c(0.842, 0.727, 0.690, 0.555, 0.576, 0.751, 0.644, 0.588, 0.732, 0.627),
     max_response = c(-0.03, -0.02, -0.10, -0.08, -0.13, -0.10, -0.07, -0.10, -0.02, -0.13)
   )
   
   p <- ggplot(broad_data, aes(x = small_firm_ratio, y = max_response, label = province)) +
     geom_point(size = 3, color = "coral") +
     geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "gray50") +
     geom_text(hjust = -0.1, vjust = 0.5, size = 3) +
     labs(
       title = "Broad Credit Channel",
       subtitle = "Firm size does NOT predict interest rate sensitivity",
       x = "Ratio of small to large firms (2000-2011)",
       y = "Max employment response to interest rate shock"
     ) +
     theme_minimal() +
     xlim(0.5, 0.95)
   
   ggplotly(p)
 })
 
 # Data explorer
 output$data_plot <- renderPlotly({
   req(data(), input$data_province)
   
   prov_data <- data()$provincial %>%
     filter(GEO == input$data_province) %>%
     select(Date, Commodity_Index, T_Bill_1M_Rate, Core_Inflation, GDP, Employment) %>%
     pivot_longer(-Date, names_to = "Variable", values_to = "Value")
   
   p <- ggplot(prov_data, aes(x = Date, y = Value, color = Variable)) +
     geom_line() +
     facet_wrap(~Variable, scales = "free_y", ncol = 1) +
     labs(title = paste("Time Series Data:", input$data_province),
          x = "Date", y = "12-month log difference") +
     theme_minimal() +
     theme(legend.position = "none")
   
   ggplotly(p, height = 800)
 })
}

# =============================================================================
# RUN APP
# =============================================================================

shinyApp(ui = ui, server = server)
