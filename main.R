library(tidyverse)
library(plotly)
library(reshape2)

# Create 3D plot of regression model with two predictors
df <- read_csv("./data/cigarette.csv")

x1 <- df$weight
x2 <- df$nicotine
z1 <- df$`carbon-monoxide`

x_title <- c('weight')
y_title <- c('nicotine')
z_title <- c('carbon monoxide')

# Set up model
model <- lm(z1 ~ x1 + x2)

#Setup Axis - x axis must be taken as the min and max
axis_x <- seq(min(x1), max(x1), by = 0.001)
axis_y <- seq(min(x2), max(x2), by = 0.01)

# Sample points may not have unique values, make prediction using this model to generate data for surface
# acast will generate matrix that will be used as surface
lm_surface <- expand.grid(x1 = unique(axis_x), x2 = unique(axis_y))
lm_surface$z1 <- predict(model, newdata = lm_surface)
lm_surface <- acast(lm_surface, x2 ~ x1, value.var='z1')

# Camera controls zoom functionality
pl <- plot_ly(df, x = ~x1, y = ~x2, z = ~z1, size= I(20)) %>% 
  add_markers()

pl <- add_trace(p = pl,
                z = lm_surface,
                x = axis_x,
                y = axis_y,
                type = "surface")

pl <- layout(p = pl, 
             scene = list(xaxis = list(title = x_title),
                          yaxis = list(title = y_title),
                          zaxis = list(title = z_title),
                          camera = list(eye = list(x = -2, y = 2, z = 2))),
             title = list(text = "3D Scatterplot Showing Regression Plane"),
             showlegend = FALSE)

htmlwidgets::saveWidget(pl, "plot3d.html")
htmltools::tags$iframe(
  src="plot3d.html",
  width="100%", 
  height="500",
  scrolling="yes", 
  seamless="seamless",
  align="left",
  frameBorder="0"
)