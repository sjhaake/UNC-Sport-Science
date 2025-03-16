library(shiny)
library(DT)
library(zoo)
library(bslib)
library(thematic)
library(ggrepel)
library(tidyverse)




# Sample dataset structure (replace with actual dataset)
forcedecks_shiny <- read.csv("forcedecks_oly_cleaned.csv")

thematic_shiny()

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "darkly",
                   bg = "#222222",
                   fg = "#86C7ED",
                   success ="#86C7ED"),
  
  title = "ForceDecks Dashboard",
  
  sidebar = sidebar(
    title = "Data Table",
    selectInput("team", "Select Team:", choices = NULL, selected = "Men's Lacrosse", multiple = FALSE),
    selectInput("date", "Select Date:", choices = NULL, selected = NULL, multiple = FALSE),
    "Most Recent CMJ Test Results",
    selectInput("metric","Select Variable",choices = c("jump_height_imp_mom_in_inches_in", "rsi_modified_imp_mom_m_s", "peak_power_bm_w_kg", "eccentric_peak_power_bm_w_kg", "countermovement_depth_cm"))),
  
  
  layout_columns(card(card_header("Team Results (Last 10 Testing Dates)"),
                      plotOutput("team_chart")),
                 card(card_header("Power vs mRSI"),
                      plotOutput("power_mrsi_chart")),
                 card(card_header("Results Table"),
                      DTOutput("data_table")),
                 col_widths = c(6,6,12),
                 row_heights = c(6,6,6))
)


server <- function(input, output, session) {
  
  
  # Compute Z-scores
  forcedecks_shiny <- forcedecks_shiny %>% group_by(name) %>%
    mutate(across(c(jump_height_imp_mom_in_inches_in, rsi_modified_imp_mom_m_s, peak_power_bm_w_kg, eccentric_peak_power_bm_w_kg, countermovement_depth_cm),
                  list(z = ~ round((.-rollapply(., width = 10, FUN = mean, fill = NA, align = "right")) /
                                     rollapply(., width = 10, FUN = sd, fill = NA, align = "right"), 2)),
                  .names = "{col}_zscore"))
  
  # Update team and date choices dynamically
  observe({
    updateSelectInput(session, "team", choices = sort(unique(forcedecks_shiny$team)), selected = "Men's Lacrosse")
  })
  
  # Update date choices based on selected team
  observeEvent(input$team, {
    filtered_dates <- forcedecks_shiny %>% filter(team %in% input$team) %>% pull(date) %>% unique() %>% sort(decreasing = TRUE)
    updateSelectInput(session, "date", choices = filtered_dates)
  }) 

  
  # Filter dataset based on input selections
  filtered_data <- reactive({
    forcedecks_shiny %>%
      filter(team == input$team, date == input$date) %>%
      select(name, jump_height_imp_mom_in_inches_in, jump_height_imp_mom_in_inches_in_zscore, rsi_modified_imp_mom_m_s, rsi_modified_imp_mom_m_s_zscore,
             peak_power_bm_w_kg, peak_power_bm_w_kg_zscore, eccentric_peak_power_bm_w_kg, eccentric_peak_power_bm_w_kg_zscore,
             countermovement_depth_cm, countermovement_depth_cm_zscore) %>%
      rename(`Jump Height (in)` = jump_height_imp_mom_in_inches_in,
             `Jump Ht (ZScore)` = jump_height_imp_mom_in_inches_in_zscore,
             `mRSI` = rsi_modified_imp_mom_m_s,
             `mRSI (ZScore)` = rsi_modified_imp_mom_m_s_zscore,
             `Peak Power (W/kg)` = peak_power_bm_w_kg,
             `Peak Power (ZScore)` = peak_power_bm_w_kg_zscore,
             `Ecc Peak Power (W/kg)` = eccentric_peak_power_bm_w_kg,
             `Ecc Peak Power (ZScore)` = eccentric_peak_power_bm_w_kg_zscore,
             `CM Depth` = countermovement_depth_cm,
             `CM Depth (Zscore)` = countermovement_depth_cm_zscore) %>%
      as.data.frame() %>%
      arrange(`mRSI (ZScore)`)
  })
  
  # Render DataTable
  output$data_table <- renderDT({
    datatable(
      filtered_data(), 
      options = list(pageLength = 100, rownames = FALSE, columnDefs = list(list(targets = "_all", className = "dt-center")))
    ) %>%
      formatStyle(
        c("Jump Ht (ZScore)", "mRSI (ZScore)", "Peak Power (ZScore)", "Ecc Peak Power (ZScore)", "CM Depth (Zscore)"),
        backgroundColor = styleInterval(c(-1, 1), c("orange","#222222", "green")),color = styleInterval(c(-1,1),c("black","86C7ED","black"))
      )
  })
  
  # Filter last 10 dates for selected team and plot average metric
  output$team_chart <- renderPlot({
    req(input$team, input$metric)
    last_10_dates <- forcedecks_shiny %>%
      filter(team == input$team) %>%
      group_by(date) %>%
      summarize(n=n()) %>% arrange(desc(date)) %>% head(10) %>% pull(date)
    
    team_avg_data <- forcedecks_shiny %>%
      filter(team == input$team, date %in% last_10_dates) %>%
      group_by(date) %>%
      summarize(avg_value = mean(.data[[input$metric]], na.rm = TRUE))
    
    ggplot(team_avg_data, aes(x = date, y = avg_value)) +
      geom_col(fill = "lightblue3") +
      theme_minimal() +
      geom_text(aes(label=round(avg_value,2),size=5,fontface = "bold",vjust=-1.25),color="white")+
      scale_y_continuous(limits = c(0,max(team_avg_data$avg_value)+.1*team_avg_data$avg_value))+
      labs(x="Date",y=input$metric)+
      theme(text = element_text(color="white",size=15),
            legend.position = "none",
            axis.text.x = element_text(size=10,color="white"),
            axis.text.y = element_text(size=10,color="white"))
  })
  
  # Render POWER vs MRSI Plot
  output$power_mrsi_chart <- renderPlot({
    req(input$team, input$date)
    
    forcedecks_shiny %>%
      filter(team == input$team, date == input$date) %>%
      ggplot(aes(peak_power_bm_w_kg_zscore,rsi_modified_imp_mom_m_s_zscore))+
      scale_x_continuous(breaks=seq(-3,3),limits=c(-3,3))+
      scale_y_continuous(breaks=seq(-3,3),limits=c(-3,3))+
      geom_rect(xmin=-3.5,xmax=-1,ymin=-3.5,ymax=-1,fill="red",alpha=.01)+
      geom_rect(xmin=1,xmax=3.5,ymin=1,ymax=3.5,fill="green",alpha=.01)+
      geom_rect(xmin=-1,xmax=0,ymin=-3.5,ymax=-1,fill="yellow",alpha=.01)+
      geom_rect(xmin=-3.5,xmax=-1,ymin=-1,ymax=0,fill="yellow",alpha=.01)+
      geom_hline(yintercept=0,linetype="dashed",color="blue",alpha=.3,size=1)+
      geom_vline(xintercept=0,linetype="dashed",color="blue",alpha=.3,size=1)+
      geom_abline(slope=1,intercept=0,linetype="dashed",color="grey",size=1)+
      geom_hline(yintercept=1,linetype="dashed",color="green",alpha=.6,size=1)+
      geom_hline(yintercept=-1,linetype="dashed",color="red",alpha=.6,size=1)+
      geom_vline(xintercept=-1,linetype="dashed",color="red",alpha=.6,size=1)+
      geom_vline(xintercept=1,linetype="dashed",color="green",alpha=.6,size=1)+
      geom_point(size=3.5,color="#118DFF",alpha=.8)+
      ggrepel::geom_text_repel(aes(label=name),size=5,color="black")+
      labs(x="Peak Power (ZScore)",y="mRSI (ZScore)")+
      theme_bw()+
      theme(title=element_text(size=15,face="bold"),legend.position="none")

  })
}

shinyApp(ui, server)

