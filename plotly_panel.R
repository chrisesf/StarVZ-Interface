library(plotly)
library(tidyr)
library(dplyr)

panel_st_plotly_native_v1 <- function(data){
  # build rectangle points
  plotly_raw_data <- data$Application %>%
    arrange(End) %>%
    select(Start, End, Position, Height, Value, JobId) %>%
    mutate(xP0 = Start, yP0 = Position,                 # 1st point of rectangle
           xP1 = End,   yP1 = Position,                 # 2nd point
           xP2 = End,   yP2 = Position + Height - 0.2,  # 3rd point
           xP3 = Start, yP3 = Position + Height - 0.2,  # 4th point
           xP4 = Start, yP4 = Position,                 # 5st point (going back to origin, i.e. P0)
           xP5 = NA,    yP5 = NA                        # NA to signal the end of this polygon
    ) %>%
    select(-Start, -End, -Position, -Height)
  
  # list of x and y pairs
  plotly_raw_data %>%
    pivot_longer(
      cols = starts_with("x") | starts_with("y"),
      names_to = c(".value", "group"),
      names_pattern = "(.)P(\\d)") -> plotly_data_xy
  
  # merge with color info
  plotly_data_xy %>%
    left_join(data$Colors %>% select(Value, Color), by = "Value") ->
    plotly_data
  
  # plot
  plot_ly(data = plotly_data,
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


panel_st_plotly_native_v2 <- function(data){
  # build rectangle points
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
  
  # list of x and y pairs
  plotly_raw_data %>%
    pivot_longer(
      cols = starts_with("x") | starts_with("y"),
      names_to = c(".value", "group"),
      names_pattern = "(.)P(\\d)") -> plotly_data_xy
  
  # merge with color info
  plotly_data_xy %>%
    left_join(data$Colors %>% select(Value, Color), by = "Value") ->
    plotly_data
  
  ggplotly(ggplot(data$Application,
                  aes(x=End,
                      y=ResourceId,
                      fill=Value)) +
             #scale_y_reverse() +
             theme_bw()) %>% # I dont know how to do a empty plotequivalent to that without ggplotly
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
