
library(shiny)
library(tidyverse)
library(lubridate)

train = read.csv("train_dataset.csv.gz")
test = read.csv("test_dataset.csv.gz")

#### Transform Variables
train = train %>%
  mutate(
    appt_time = ymd_hms(appt_time, tz="UTC"),
    appt_date = as.Date(appt_time),
    appt_hour = hour(appt_time),
    appt_day = wday(appt_time, label=T, abbr=T),
    diff_time = as.numeric(difftime(appt_date, as.Date(appt_made), units="days")))
test = test %>%
  mutate(
    appt_time = ymd_hms(appt_time, tz="UTC"),
    appt_date = as.Date(appt_time),
    appt_hour = hour(appt_time),
    appt_day = wday(appt_time, label=T, abbr=T),
    diff_time = as.numeric(difftime(appt_date, as.Date(appt_made), units="days")))

#### Prediction Model
model = glm(no_show ~ appt_day + appt_hour + diff_time,
            data=train, family=binomial())
summary(model)

#### Predict No Shows and Evaluate Metric
test$pred_prob = predict(model, newdata=test, type="response")
test$pred_no_show = if_else(test$pred_prob >= 0.5, 1, 0)
test$show_prob = 1 - test$pred_prob

#### Summarize Expected Show Percentage
result = test %>%
  group_by(appt_day, appt_hour) %>%
  summarise(exp_show_perc = mean(show_prob, na.rm = T))

#### Shiny App
ui = fluidPage(
  titlePanel("Expected Show-Up Rates by Hour"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "day_select",
        "Select Day(s) of the Week:",
        choices = levels(result$appt_day),
        selected = "Mon",
        multiple = T
      ),
      helpText("Displays expected percentage of patients who will show up per hour-long block.")
    ),
    mainPanel(
      plotOutput("linePlot", hover = "plot_hover")
    )
  )
)

server = function(input, output) {
  output$linePlot = renderPlot({
    weekly = result %>%
      filter(appt_day %in% input$day_select)
    ggplot(weekly, aes(x = appt_hour, y = exp_show_perc, color = appt_day, group = appt_day)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      scale_x_continuous(breaks = 12:23, labels = paste0(12:23, ":00")) +
      labs(
        x = "Hour of Day",
        y = "Expected Show-Up Percentage",
        title = "Expected Patient Show-Up by Hour and Day"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.title = element_blank())
  })
}

shinyApp(ui, server)
