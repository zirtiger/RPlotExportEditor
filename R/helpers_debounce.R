# plot-editor/R/helpers_debounce.R
# Debouncing and throttling utilities for input handling

# Create a debounced version of a function
create_debouncer <- function(fun, delay = 500) {
  timer <- NULL
  
  function(...) {
    if (!is.null(timer)) {
      invalidateLater(timer)
    }
    
    timer <<- delay
    later(fun, delay = delay, ...)
  }
}

# Create a throttled version of a function
create_throttler <- function(fun, delay = 200) {
  last_run <- 0
  
  function(...) {
    now <- Sys.time()
    if (as.numeric(now - last_run) * 1000 >= delay) {
      last_run <<- now
      fun(...)
    }
  }
}

# Debounced reactive expression
debounced_reactive <- function(expr, delay = 500) {
  reactive({
    invalidateLater(delay)
    expr
  })
}

# Debounced observeEvent wrapper
debounced_observeEvent <- function(eventExpr, handlerExpr, delay = 500, ...) {
  debounced_handler <- create_debouncer(handlerExpr, delay)
  observeEvent(eventExpr, debounced_handler, ...)
}

# Throttled observeEvent wrapper
throttled_observeEvent <- function(eventExpr, handlerExpr, delay = 200, ...) {
  throttled_handler <- create_throttler(handlerExpr, delay)
  observeEvent(eventExpr, throttled_handler, ...)
}