library(plotly)
library(tidyr)
library(dplyr)
library(scales)

# plotly_panel.R (Your existing code for v1)
panel_st_plotly_native_v1 <- function(data) {
  plotly_raw_data <- data$Application %>%
    arrange(End) %>%
    select(Start, End, Position, Height, Value, JobId) %>%
    mutate(
      xP0 = Start, yP0 = Position,
      xP1 = End,   yP1 = Position,
      xP2 = End,   yP2 = Position + Height - 0.2,
      xP3 = Start, yP3 = Position + Height - 0.2,
      xP4 = Start, yP4 = Position,
      xP5 = NA,    yP5 = NA
    ) %>%
    select(-Start, -End, -Position, -Height)

  plotly_data_xy <- plotly_raw_data %>%
    pivot_longer(
      cols = starts_with("x") | starts_with("y"),
      names_to = c(".value", "group"),
      names_pattern = "(.)P(\\d)"
    )

  plotly_data <- plotly_data_xy %>%
    left_join(data$Colors %>% select(Value, Color), by = "Value")

  # --- ADD THIS DEBUGGING LINE ---
  print("Head of plotly_data:")
  print(head(plotly_data))
  print("Summary of JobId column:")
  print(summary(plotly_data$JobId))
  # --- END DEBUGGING LINES ---

  p <- plot_ly(
    data = plotly_data,
    x = ~x,
    y = ~y,
    mode = "lines",
    fill = "toself",
    text = ~paste("Value:", Value, "<br>JobId:", JobId),
    customdata = ~JobId,
    hoveron = "points+fills",
    hoverinfo = "text",
    type = "scatter",
    split = ~Value,
    color = I(plotly_data$Color)
  )

  p <- p %>% layout(
    xaxis = list(
      rangeslider = list(
        visible = TRUE
      ),
      range = c(data$config$limits$start, data$config$limits$end)
    ),
    width = 1200,
    height = 600,
    hovermode = "closest",
    dragmode = "zoom"
  )

  return(p)
}

panel_st_plotly_native_v2 <- function(data){
  plotly_raw_data <- data$Application %>%
    arrange(End) %>%
    select(Start, End, Position, Height, Value, JobId) %>%
    mutate(xP0 = Start, yP0 = Position,                 # 1st point ofrectangle
           xP1 = End,   yP1 = Position,                 # 2nd point
           xP2 = End,   yP2 = Position + Height - 0.2,  # 3rd point
           xP3 = Start, yP3 = Position + Height - 0.2,  # 4th point
           xP4 = Start, yP4 = Position,                 # 5st point(going back to origin, i.e. P0)
           xP5 = NA,    yP5 = NA                        # NA to signalthe end of this polygon
    ) %>%
    select(-Start, -End, -Position, -Height)
  
  plotly_raw_data %>%
    pivot_longer(
      cols = starts_with("x") | starts_with("y"),
      names_to = c(".value", "group"),
      names_pattern = "(.)P(\\d)") -> plotly_data_xy
  
  plotly_data_xy %>%
    left_join(data$Colors %>% select(Value, Color), by = "Value") ->
    plotly_data
  
  ggplotly(ggplot(data$Application,
                  aes(x=End,
                      y=ResourceId,
                      fill=Value)) +
             #scale_y_reverse() +
             theme_bw()) %>%
    add_trace(data = plotly_data,
              x = ~x,
              y = ~y,
              mode="lines",
              fill="toself",
              text=~paste("Value:", Value, "<br>JobId:", JobId),
              hoveron="points",
              hoverinfo="text",
              type="scatter",
              split=~Value,
              color = I(plotly_data$Color))
}
