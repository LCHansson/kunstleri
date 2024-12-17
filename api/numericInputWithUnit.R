library(shiny)
library(htmltools)

numericInputWithUnit <- function(inputId, label, value, min = NA, max = NA, step = NA, 
                                 width = NULL, unit = "") {
  div(
    numericInput(
      inputId = inputId,
      label = label,
      value = value,
      min = min,
      max = max,
      step = step,
      width = width
    ),
    div(
      class = "input-unit",
      unit
    )
  )
}