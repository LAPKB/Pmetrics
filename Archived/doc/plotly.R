## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=F, message=F-------------------------------------------------
library(Pmetrics)
library(tidyverse)
library(plotly)

## ---- echo = T, eval = T------------------------------------------------------
NPex$op$plot()

## ---- echo = T, eval = T------------------------------------------------------
NPex$op$plot(line = list(lm = T, loess = F, ref = T))

## ---- echo = T, eval = T------------------------------------------------------
NPex$op$plot(line = list(loess = list(color = "orange", dash = "dashdot", width = 2)))

## ---- echo = T, eval = T------------------------------------------------------
NPex$op$plot()

## ---- echo = T, eval = T------------------------------------------------------
NPex$op$plot(marker = list(color = "slategray", size = 12, 
                           opacity = 0.5, line = list(color = "navy")))

## ---- echo = T, eval = T------------------------------------------------------
NPex$data$plot(legend = T)

## ---- echo = T, eval = T------------------------------------------------------
NPex$data$plot(pred = list(NPex$post, color = "navy"), 
               line = F, 
               legend = list(
                 x = 0.75,
                 y = 0.85,
                 borderwidth = 1,
                 bgcolor = "antiquewhite"
               ),
               xlim = c(119, 146))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot(xlab = "Time (h)", ylab = "Rifapentine (mg/L)")

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot(xlab = list(text = "Time (h)", font = list(color = "slategrey", family = "Times New Roman", size = 18)))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot(xaxis = list(linecolor = "firebrick", ticks = "inside"))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot(linecolor = "dodgerblue", ticks = "outside")

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot(title = list(text = "Rifapentine", yanchor = "top"), ylab = "Concentration (mg/L)", xlab = "Time (h)")

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot() %>%
  add_shapes(ab_line(h = 5, line = list(color = "green", width = 3)))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot() %>%
  add_shapes(shape = list(type = "circle", x0 = 122, x1 = 132,
                                y0 = 15, y1 = 22))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot() %>%
  add_annotations(text = ~id) #the tilde tells plotly to use the contents of the id column

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot() %>%
  add_text(text = ~id, textposition = "top right", 
           textfont = list(color = "green", size = 12))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot() %>%
  add_text(x = 125, y = 2, text = "<b>Limit of quantification</b>", textfont = list(size = 10)) %>%
  add_shapes(shape = ab_line(h = 1.8))

## ----echo = T, eval = T-------------------------------------------------------
NPex$data$plot() %>%
  add_annotations(x = 125, y = 2, text = "<i>Limit of quantification</i>", font = list(size = 10), showarrow = F, bgcolor = "antiquewhite", bordercolor = "black") %>%
  add_shapes(shape = ab_line(h = 1.8))

