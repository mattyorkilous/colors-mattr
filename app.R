pacman::p_load(
  tidyr, dplyr, ggplot2, shiny, bslib, purrr, forcats, 
  stringr, shinyjs, shinyFeedback, rclipboard
)

rm(list = ls())

# ------------------------------------------------------------------------------

main <- function() {
  wheel_plot_base <- create_base_wheel_plot()
  
  ui <- get_ui()
  
  server <- get_server(wheel_plot_base)
  
  shinyApp(ui, server)
}

create_base_wheel_plot <- function() {
  wheel_plot_base <- expand_grid(
    r = seq(0, 1, length = 128),
    theta = seq(0, 2 * pi, length = 128)
  ) |> 
    mutate(
      x = r * cos(theta),
      y = r * sin(theta),
      hex = hsv(
        h = theta / (2 * pi),
        s = r,
        v = 1
      )
    ) |> 
    ggplot(
      mapping = aes(x, y, colour = hex)
    ) + 
    geom_point(size = 2) + 
    scale_color_identity() + 
    coord_fixed() + 
    theme_void()
  
  wheel_plot_base
}

get_ui <- function() {
  ui <- page_sidebar(
    useShinyjs(),
    useShinyFeedback(),
    rclipboardSetup(),
    title = "Colors Mattr",
    sidebar = sidebar(
      textInput(
        inputId = "hex_text",
        label = "Hex code:",
        value = "#622F6B"
      ),
      numericInput(
        inputId = "n",
        label = "Number of colors:",
        value = 2
      ),
      actionButton(
        inputId = "calculate",
        label = "Calculate!"
      )
    ),
    card(
      plotOutput(
        "wheel_plot",
        height = 512,
        width = 512,
        click = "wheel_plot_click"
      ),
      plotOutput(
        "v_plot",
        width = 512
      ),
      sliderInput(
        inputId = "v_slider",
        label = NULL,
        min = 0,
        max = 1,
        value = 0.420,
        step = 0.001,
        width = 512
      )
    ),
    card(
      plotOutput(
        "color_set_plot"
      ),
      uiOutput("copy_button")
    )
  )
  
  ui
}

get_server <- function(wheel_plot_base) {
  server <- function(input, output) {
    h <- reactiveVal()
    
    s <- reactiveVal()
    
    v <- reactiveVal()
    
    hsv_coordinates <- eventReactive(
      input$calculate, 
      get_hsv(input$hex_text), 
      ignoreNULL = FALSE
    )
    
    n_selected <- eventReactive(
      input$calculate, 
      input$n, 
      ignoreNULL = FALSE
    )
    
    observeEvent(
      hsv_coordinates(), 
      handle_hsv_coordinates(hsv_coordinates, h, s, v)
    )
    
    observeEvent(
      input$wheel_plot_click, 
      handle_wheel_plot_click(input, h, s, v)
    )
    
    observeEvent(
      input$v_slider, 
      handle_v_slider(input, h, s, v)
    )
    
    color_set <- reactive(
      get_color_set(h(), s(), n_selected())
    )
    
    copy_text <- reactive(
      get_copy_text(color_set(), v())
    )
    
    wheel_plot <- reactive(
      get_wheel_plot(wheel_plot_base, h(), s(), color_set())
    )
    
    v_plot_base <- reactive(
      get_v_plot_base(h(), s())
    )
    
    v_plot <- reactive(
      get_v_plot(v_plot_base(), v())
    )
    
    color_set_plot <- reactive(
      get_color_set_plot(color_set(), v())
    )
    
    output$copy_button <- renderUI(
      rclipButton(
        inputId = "copy_button",
        label = "Copy color set",
        clipText = copy_text()
      )
    )
    
    output$wheel_plot <- renderPlot(
      wheel_plot()
    )

    output$v_plot <- renderPlot(
      v_plot()
    )

    output$color_set_plot <- renderPlot(
      color_set_plot()
    )
  }
  
  server
}

get_hsv <- function(hex) {
  hsv_raw <- rgb2hsv(col2rgb(hex))
  
  hsv <- set_names(
    as.list(hsv_raw),
    dimnames(hsv_raw)[[1]]
  )
  
  hsv
}

handle_hsv_coordinates <- function(hsv_coordinates, h, s, v) {
  h(hsv_coordinates()[["h"]])
  
  s(hsv_coordinates()[["s"]])
  
  v(hsv_coordinates()[["v"]])
  
  updateSliderInput(
    inputId = "v_slider",
    value = v()
  )
}

handle_wheel_plot_click <- function(input, h, s, v) {
  x <- input$wheel_plot_click$x
  
  y <- input$wheel_plot_click$y
  
  r <- sqrt(x ^ 2 + y ^ 2)
  
  req(r <= 1)
  
  h((atan2(y, x) %% (2 * pi)) / (2 * pi))
  
  s(r)
  
  updateTextInput(
    inputId = "hex_text",
    value = hsv(h(), s(), v())
  )
}

handle_v_slider <- function(input, h, s, v) {
  v(input$v_slider)
  
  updateTextInput(
    inputId = "hex_text",
    value = hsv(h(), s(), v())
  )
}

get_color_set <- function(h, s, n_selected) {
  theta <- 2 * pi * h
  
  color_set <- tibble(
    h =
      (
        seq(
          theta, 
          theta + 2 * pi * (1 - 1 / n_selected), 
          length.out = n_selected
        ) %% (2 * pi)
      ) / (2 * pi),
    s = s
  ) |> 
    mutate(
      bar_height = 1,
      x = s * cos(h * 2 * pi),
      y = s * sin(h * 2 * pi)
    )
  
  color_set
}

get_copy_text <- function(color_set, v) {
  inner_text <- color_set |> 
    select(h, s) |> 
    mutate(v = v) |> 
    rowwise() |> 
    mutate(hex = hsv(h, s, v)) |> 
    ungroup() |> 
    pull(hex) |> 
    str_c(collapse = '", "')
  
  copy_text <- str_c('c("', inner_text, '")')
  
  copy_text
}

get_wheel_plot <- function(wheel_plot_base, h, s, color_set) {
  color_set <- color_set |> 
    mutate(
      size = if_else(row_number() == 1, 8, 4),
      colour = if_else(row_number() == 1, "#000000", "#9E9E9E")
    )
  
  wheel_plot <- wheel_plot_base
  
  for (i in seq_along(color_set$x)) {
    wheel_plot <- wheel_plot + 
      geom_point(
        x = color_set$x[[i]],
        y = color_set$y[[i]],
        colour = color_set$colour[[i]],
        shape = 1,
        fill = NA, 
        size = color_set$size[[i]]
      )
  }
  
  wheel_plot
}

get_v_plot_base <- function(h, s) {
  v_plot_base <- tibble(
    x = seq(0, 1, length = 256),
    y = 0
  ) |> 
    rowwise() |> 
    mutate(
      hex = hsv(h, s, v = x)
    ) |> 
    ungroup() |> 
    ggplot(
      aes(x, y, colour = hex)
    ) + 
    geom_point(size = 2) + 
    scale_color_identity() + 
    theme_void()
  
  v_plot_base
}

get_v_plot <- function(v_plot_base, v) {
  v_plot <- v_plot_base + 
    geom_point(
      x = v,
      y = 0,
      colour = "#000000",
      shape = 1,
      fill = NA,
      size = 8
    )
  
  v_plot
}

get_color_set_plot <- function(color_set, v) {
  color_set_plot <- color_set |>
    rowwise() |> 
    mutate(
      hex = hsv(h, s, v)
    ) |> 
    ungroup() |> 
    mutate(hex = fct_inorder(hex)) |> 
    ggplot(aes(hex, bar_height, fill = hex)) +
    geom_bar(stat = "identity") +
    scale_fill_identity() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(size = 20),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank()
    )
  
  color_set_plot
}

# ------------------------------------------------------------------------------

main()

