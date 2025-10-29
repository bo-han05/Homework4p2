
library(shiny)
library(dplyr)
library(lubridate)
library(plotly)

# load model and threshold
model <- readRDS("chosen_model_random_forest.rds")  # or glm file
threshold <- readRDS("chosen_threshold.rds")
# load patient history (computed earlier from train set)
hist_patient <- readRDS("hist_patient.rds")  # save earlier

ui <- fluidPage(
  titlePanel("ECUR Weekly Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateInput("week_start", "Week start", value = Sys.Date())
    ),
    mainPanel(
      plotlyOutput("week_heatmap"),
      DT::DTOutput("hour_details")
    )
  )
)

server <- function(input, output, session) {
  week_data <- reactive({
    # assume week file is provided or generated: week_appts.csv
    w <- read_csv("week_future_appts.csv", col_types = cols(appt_time = col_datetime()))
    # feature engineering
    w2 <- make_features(w, hist_patient)
    # predict
    if(class(model)[1] == "ranger") {
      probs <- predict(model, data = w2)$predictions[,2]
    } else {
      probs <- predict(model, newdata = w2, type = "response")
    }
    w2$p_no_show <- probs
    w2
  })
  
  output$week_heatmap <- renderPlotly({
    w2 <- week_data()
    ecur_tbl <- w2 %>%
      mutate(hour_block = floor_date(appt_time, "hour")) %>%
      group_by(hour_block) %>%
      summarize(N = n(), expected_shows = sum(1 - p_no_show), ECUR = expected_shows / N, .groups = "drop")
    # plotly heatmap or tile plot; for brevity show simplified bar or tile
    # convert hour_block to day & hour columns, then use plot_ly (x=day, y=hour, z=ECUR)
  })
}

shinyApp(ui, server)
