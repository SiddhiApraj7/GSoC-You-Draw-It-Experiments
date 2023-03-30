# Load libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(r2d3)
library(tidyverse)
library(purrr)
library(gridSVG)
library(lubridate)
library(readxl)
library(renv)
library(shinyFeedback)
library(shinyalert)


csvdata <- NULL
linearModel <- NULL
nonLinearModel <- NULL

ydtUI <- function(id) {
  simulationMode <-
    c("Generate Data" = "generateData", "File Data" = "fileData")
  navbarPage(
    "Can 'You Draw It'?",

    tabPanel(title = "Example: Eye Fitting Straight Lines in the Modern Era",
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "css/d3.css")
             ),
             fluidRow(
               column(
                 width = 12,
                 shinyFeedback::useShinyFeedback(),
                 helpText(h4(
                   "Use your mouse to fill in the trend in the yellow box region"
                 )),
                 d3Output(NS(id, "shinydrawr"), height = "600px"),
                 br(),
                 textOutput(NS(id, "tip"))
               )),
             fluidRow(
               column(
                 width = 6,
                 selectInput(
                   NS(id, "modelType"),
                   "Model:",
                   c(
                     "Linear Regression" = "lm",
                     "Non Linear Least Square" = "nlls"
                   )
                 ),
                 # textInput("predictionInput", "Prediction Input X:"),
                 # actionButton("predict", "Predict Y!"),
                 radioButtons(
                   NS(id, 'simMode'),
                   "Use System Generated Data or File Data  ?",
                   simulationMode
                 ),
                 actionButton(NS(id, "reset"), "Reset"),
                 actionButton(NS(id, "usefile"), "Use File"),
                 fileInput(
                   NS(id, "upload"),
                   NULL,
                   buttonLabel = "Upload...",
                   multiple = FALSE,
                   accept = ".csv"
                 ),
               )
               ,
               column(
                 width = 6,

                 textInput(NS(id,"predictionInput"), "Prediction Input X:"),
                 actionButton(NS(id,"predict"), "Predict Y!"),
                 textOutput(NS(id,"predictOut")),
                 verbatimTextOutput(NS(id,"modelcode"))
               )
               # ,
               # column(
               #   width = 6,
               #   offset = 2,
               #   numericInput(
               #     NS(id, "n"),
               #     "Rows",
               #     value = 5,
               #     min = 1,
               #     step = 1
               #   ),
               #   tableOutput(NS(id, "head")),
               #   actionButton(NS(id, "showDataSummury"), "Show Data summary")
               # )

             )
             # ,
             # fluidRow(
             #   column(
             #     width = 6,
             #
             #     textInput("predictionInput", "Prediction Input X:"),
             #     actionButton("predict", "Predict Y!")
             # )
             # )
    )
  )
}





# Turn a list of data into a json file
data_to_json <- function(data) {
  jsonlite::toJSON(
    data,
    dataframe = "rows",
    auto_unbox = FALSE,
    rownames = TRUE
  )
  #  print(data);
}

# Redefine drawr function
drawr <- function(data,
                  linear            = "true",
                  draw_start        = NULL,
                  points_end        = NULL,
                  x_by              = 0.25,
                  free_draw         = T,
                  points            = "partial",
                  aspect_ratio      = 1.5,
                  title             = "",
                  x_range           = NULL,
                  y_range           = NULL,
                  x_lab             = "",
                  y_lab             = "",
                  drawn_line_color  = "steelblue",
                  data_tab1_color   = "steelblue",
                  x_axis_buffer     = 0.01,
                  y_axis_buffer     = 0.05,
                  show_finished     = T,
                  shiny_message_loc = NULL) {
  # print (data);
  # data2   = data_to_json(data)
  # print (data2);

  line_data  <- data$line_data
  point_data <- data$point_data

  x_min <- min(line_data$x)
  x_max <- max(line_data$x)
  y_min <- min(line_data$y)
  y_max <- max(line_data$y)

  # x_buffer <- (x_max - x_min) * x_axis_buffer
  # x_range <- c(x_min - x_buffer, x_max + x_buffer)
  # y_buffer <- (y_max - y_min) * y_axis_buffer
  # y_range <- c(y_min - y_buffer, y_max + y_buffer)

  if (is.null(x_range)) {
    x_buffer <- (x_max - x_min) * x_axis_buffer
    x_range <- c(x_min - x_buffer, x_max + x_buffer)
  }
  if (is.null(y_range)) {
    y_buffer <- (y_max - y_min) * y_axis_buffer
    y_range <- c(y_min - y_buffer, y_max + y_buffer)
    if (linear != "true") {
      if (y_range[1] <= 0) {
        y_range[1] <- min(y_min, y_axis_buffer)
      }
    }
  } else {
    print("y range")
    print(y_range)
    print(y_min)
    print(y_max)
    if (y_range[1] > y_min | y_range[2] < y_max) {
      stop("Supplied y range doesn't cover data fully.")
    }
  }

  if ((draw_start <= x_min) | (draw_start >= x_max)) {
    stop("Draw start is out of data range.")
  }

  r2d3::r2d3(
    data   = data_to_json(data),
    script = "www/you-draw-it.js",
    d3_version = "5",
    dependencies = c("d3-jetpack"),
    options = list(
      draw_start        = draw_start,
      points_end        = points_end,
      linear            = as.character(linear),
      free_draw         = free_draw,
      points            = points,
      aspect_ratio      = aspect_ratio,
      pin_start         = T,
      x_range           = x_range,
      x_by              = x_by,
      y_range           = y_range,
      line_style        = NULL,
      data_tab1_color   = data_tab1_color,
      drawn_line_color  = drawn_line_color,
      show_finished     = show_finished,
      shiny_message_loc = shiny_message_loc,
      title             = title,
      tipText = "orig"
    )
  )

}

# Linear Data from File
linearDataFileGen <-
  function(y_xbar,
           slope,
           sigma,
           points_choice = "full",
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25) {
    print("csvfiledata")
    print(csvdata)

    x_min_file =  min(csvdata$X)
    x_max_file =  max(csvdata$X)

    print("X MIN and MAX")
    print(x_min_file)
    print(x_max_file)

    x_min = x_min_file
    x_max = x_max_file


    points_end_scale <-
      ifelse(points_choice == "full", 1, points_end_scale)

    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)

    #yintercept = y_xbar - slope * mean(xVals)

    yVals <- csvdata$Y

    # Get point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yVals) %>%
      arrange(x)


    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    slopehat <- coef(lm.fit)[2] %>% as.numeric()
    linearModel <<- lm.fit

    # Simulate best fit line data
    line_data <- tibble(
      data = "line_data",
      x = seq(x_min, x_max, x_by),
      y = yintercepthat + slopehat * x
    )

    data <- list(point_data = point_data, line_data = line_data)

    return(data)
  }


# Linear Data Simulation
linearDataGen <-
  function(y_xbar,
           slope,
           sigma,
           points_choice = "full",
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25) {
    points_end_scale <-
      ifelse(points_choice == "full", 1, points_end_scale)

    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)

    yintercept = y_xbar - slope * mean(xVals)

    errorVals <- rnorm(N, 0, sigma)


    # Simulate point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yintercept + slope * x + errorVals) %>%
      arrange(x)

    # Obtain least squares regression coefficients
    lm.fit <- lm(y ~ x, data = point_data)
    yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    slopehat <- coef(lm.fit)[2] %>% as.numeric()

    linearModel <<- lm.fit

    # Simulate best fit line data
    line_data <- tibble(
      data = "line_data",
      x = seq(x_min, x_max, x_by),
      y = yintercepthat + slopehat * x
    )

    data <- list(point_data = point_data, line_data = line_data)

    return(data)
  }


# Non Linear Data Simulation
nonLinearDataGen <-
  function(y_xbar,
           slope,
           sigma,
           points_choice = "full",
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25) {
    points_end_scale <-
      ifelse(points_choice == "full", 1, points_end_scale)

    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)

    yintercept = y_xbar - slope * mean(xVals)

    errorVals <- rnorm(N, 0, sigma)


    # Simulate point data
    #point_data <- tibble(data = "point_data",
    #                     x = xVals,
    #                     y = yintercept + slope * x + errorVals) %>%
    # arrange(x)


    # Simulate point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yintercept +  slope * slope * x + errorVals) %>%
      arrange(x)


    print("point+data:")
    print(select(point_data, select = c('y')))

    # Obtain least squares regression coefficients
    #lm.fit <- lm(y ~ x, data = point_data)

    ysvals = unlist(select(point_data, select = c('y')))
    xsvals = unlist(select(point_data, select = c('x')))

    print(xsvals)
    print("Priting y values")
    print(ysvals)



    #nls <- nls(ysvals ~ b1*xsvals^2+b2, data=point_data, start = list(b1 = 1, b2 = 3))

    nls <-
      nls(ysvals ~ b1 * xsvals ^ 2 + b2,
          data = point_data,
          start = list(b1 = 1, b2 = 3))



    print("Min max xsvals")

    print (min(xsvals))
    print (max(xsvals))

    new.data <-
      data.frame(xvalues = seq(min(xsvals), max(xsvals), len = 40))

    #new.data <- data.frame(xvalues = seq(min(xvalues),max(xvalues),len = 100))

    #yintercepthat <- coef(lm.fit)[1] %>% as.numeric()
    #slopehat <- coef(lm.fit)[2] %>% as.numeric()

    errorVals <- rnorm(N, 0, sigma)

    # Simulate best fit line data
    line_data <- tibble(
      data = "line_data",
      x = seq(min(xsvals), max(xsvals), len = 40),
      #y = yintercepthat + slopehat * x
      y = predict (nls, newdata = new.data)
    )

    nonLinearModel <<- nls

    print("After Prediction - non linear")
    ysvals = unlist(select(line_data, select = c('y')))
    xsvals = unlist(select(line_data, select = c('x')))

    print(xsvals)
    print("Priting y values")
    print(ysvals)

    #data <- list(point_data = point_data, line_data = line_data)

    data <- list(point_data = point_data, line_data = line_data)

    return(data)
  }



# Non Linear Data from File
nonLinearDataFileGen <-
  function(y_xbar,
           slope,
           sigma,
           points_choice = "full",
           points_end_scale = 1,
           N = 30,
           x_min = 0,
           x_max = 20,
           x_by  = 0.25) {
    print("csvfiledata")
    print(csvdata)

    x_min_file =  min(csvdata$X)
    x_max_file =  max(csvdata$X)

    print("X MIN and MAX")
    print(x_min_file)
    print(x_max_file)

    x_min = x_min_file
    x_max = x_max_file


    points_end_scale <-
      ifelse(points_choice == "full", 1, points_end_scale)

    # Set up x values
    xVals <- seq(x_min, x_max, length.out = N)
    xVals <- ifelse(xVals < x_min, x_min, xVals)
    xVals <- ifelse(xVals > x_max, x_max, xVals)

    #yintercept = y_xbar - slope * mean(xVals)

    yVals <- csvdata$Y

    # Get point data
    point_data <- tibble(data = "point_data",
                         x = xVals,
                         y = yVals) %>%
      arrange(x)

    #------------------

    print("point+data:")
    print(select(point_data, select = c('y')))

    ysvals = unlist(select(point_data, select = c('y')))
    xsvals = unlist(select(point_data, select = c('x')))

    nls <-
      nls(ysvals ~ b1 * xsvals ^ 2 + b2,
          data = point_data,
          start = list(b1 = 1, b2 = 3))

    nonLinearModel <<- nls

    new.data <-
      data.frame(xvalues = seq(min(xsvals), max(xsvals), len = 40))

    # Simulate best fit line data
    line_data <- tibble(
      data = "line_data",
      x = seq(min(xsvals), max(xsvals), len = 40),
      y = predict (nls, newdata = new.data)
    )

    print("After Prediction - non linear")
    ysvals = unlist(select(line_data, select = c('y')))
    xsvals = unlist(select(line_data, select = c('x')))

    print(xsvals)
    print("Priting y values")
    print(ysvals)

    #-------------------


    data <- list(point_data = point_data, line_data = line_data)

    return(data)
  }





load_file <- function(name, path) {
  showNotification("click Use File")
  ext <- tools::file_ext(name)
  switch(
    ext,
    csv = vroom::vroom(path, delim = ","),
    tsv = vroom::vroom(path, delim = "\t"),
    validate("Invalid file; Please upload a .csv or .tsv file")
  )
}

# Define server logic required to draw a histogram
ydtServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    modelTypeOut <- reactive({
      # if (input$modelType=='nls')
      #   linearModel <<- NULL
      #nonLinearModel<<- NULL
      print("model type selecte:")
      print(input$modelType)

      input$modelType
    })


    useFile <- reactive({
      switch(input$simMode,
             generateData = FALSE,
             fileData = TRUE,
             FALSE)
    })

    filedata <- eventReactive (input$upload, {
      req(input$upload)
      load_file(input$upload$name, input$upload$datapath)
    })

    # output$head <- renderTable({
    #   head(filedata(), input$n)
    # })

    #linear_data <- eventReactive(input$reset, {
    linear_data <- reactive ({
      input$reset
      print("event Reactive linear_data")

      tibble(
        dataset = c("S"),
        y_xbar = c(3.9),
        slope  = c(0.8),
        sigma  = c(2.8),
        x_min   = c(0),
        x_max   = c(20),
        N       = 40,
        x_by    = 0.25
      ) %>%
        mutate(data = purrr::pmap(
          list(
            y_xbar = y_xbar,
            slope  = slope,
            sigma  = sigma,
            x_min  = x_min,
            x_max  = x_max,
            x_by   = x_by,
            N      = N
          ),
          linearDataGen
        )) %>%
        unnest(data) %>%
        unnest(data)
    })

    #linear_data <- eventReactive(input$reset, {
    non_linear_data <- reactive ({
      input$reset
      print("event Reactive linear_data")

      tibble(
        dataset = c("S"),
        y_xbar = c(3.9),
        slope  = c(0.8),
        sigma  = c(2.8),
        x_min   = c(0),
        x_max   = c(20),
        N       = 40,
        x_by    = 0.25
      ) %>%
        mutate(data = purrr::pmap(
          list(
            y_xbar = y_xbar,
            slope  = slope,
            sigma  = sigma,
            x_min  = x_min,
            x_max  = x_max,
            x_by   = x_by,
            N      = N
          ),
          nonLinearDataGen
        )) %>%
        unnest(data) %>%
        unnest(data)
    })


    linear_data_file <- eventReactive(input$usefile, {
      print("event Reactive linear_data_file")

      csvdata <<- filedata()

      print("CSV data")
      print(csvdata)

      tibble(
        dataset = c("S"),
        y_xbar = c(3.9),
        slope  = c(0.9),
        sigma  = c(2.8),
        x_min   = c(0),
        x_max   = c(20),
        N       = 40,
        x_by    = 0.25
      ) %>%
        mutate(data = purrr::pmap(
          list(
            y_xbar = y_xbar,
            slope  = slope,
            sigma  = sigma,
            x_min  = x_min,
            x_max  = x_max,
            x_by   = x_by,
            N      = N
          ),
          linearDataFileGen
        )) %>%
        unnest(data) %>%
        unnest(data)
    })


    non_linear_data_file <- eventReactive(input$usefile, {
      print("event Reactive linear_data_file")

      csvdata <<- filedata()

      print("CSV data")
      print(csvdata)

      tibble(
        dataset = c("S"),
        y_xbar = c(3.9),
        slope  = c(0.9),
        sigma  = c(2.8),
        x_min   = c(0),
        x_max   = c(20),
        N       = 40,
        x_by    = 0.25
      ) %>%
        mutate(data = purrr::pmap(
          list(
            y_xbar = y_xbar,
            slope  = slope,
            sigma  = sigma,
            x_min  = x_min,
            x_max  = x_max,
            x_by   = x_by,
            N      = N
          ),
          nonLinearDataFileGen
        )) %>%
        unnest(data) %>%
        unnest(data)
    })


    linear_y_range <- reactive({
      linear_data <- linear_data()
      range(linear_data$y) * c(1.5, 1.5)
    })

    linear_x_range <- reactive({
      linear_data <- linear_data()

      modelT = modelTypeOut()
      useFileData = useFile()

      if ((modelT == 'nlls')  && useFileData)
      {
        linear_data <- non_linear_data_file()
      }else if (useFileData)
      {
        linear_data <- linear_data_file()
      }else if (modelT == 'nlls') {
        linear_data <- non_linear_data()
      }

      c(min(linear_data$x), max(linear_data$x))
    })

    message_loc <- session$ns("drawr_message")

    #addTooltip(session, id = "tip", title = "This is an input.",
    #         placement = "left", trigger = "hover")

    output$tip <- renderText({
      paste(input$tipText)
      #addTooltip(session, id = "tip", title = "This is an input.",
      #      placement = "left", trigger = "click")
      #paste("sdfsdfsdfsfd")
    })

    # textInput(NS(id,"predictionInput"), "Prediction Input X:"),
    # actionButton(NS(id,"predict"), "Predict Y!"),
    #textOutput(NS(id,"predictOut")),
    #verbatimTextOutput(NS(id,"modelcode"))


    # observeEvent(input$predict, {
    #   output$predictOut <- renderText({
    #     if (input$predictionInput !="")
    #
    #       temp.data <-
    #         data.frame(
    #           x=as.numeric(c(input$predictionInput)))
    #
    #       modelT = modelTypeOut()
    #
    #       if ((modelT == 'nlls'))
    #       {
    #         out = predict(nonLinearModel, temp.data)
    #         paste("Non Linear Prediction", out)
    #       }else {
    #         out = predict(linearModel, temp.data)
    #         #paste("Prediction", out)
    #       }
    #   })
    # }, once="TRUE")


    observeEvent(input$predict, {
        if (input$predictionInput !="")

        modelT = modelTypeOut()

        if ((modelT == 'nlls'))
        {
          temp.data <-
            data.frame(
              xsvals=as.numeric(c(input$predictionInput)))


          out = predict(nonLinearModel, newdata=temp.data)
          #paste("Non Linear Prediction", out)
        }else {
          temp.data <-
            data.frame(
              x=as.numeric(c(input$predictionInput)))

          out = predict(linearModel, temp.data)
          #paste("Prediction", out)
        }

        shinyalert("Prediction", paste("Prediction", out))
        output$predictOut <- renderText({out})
      })



    output$modelcode <- renderPrint({
      #summary(1:10)
    })



    output$shinydrawr <- r2d3::renderD3({
      line_data <- linear_data() %>%
        filter(data == "line_data")

      point_data <- linear_data() %>%
        filter(data == "point_data")

      modelT = modelTypeOut()

      useFileData = useFile()

      if ((modelT == 'nlls')  && useFileData)
      {
        line_data <- non_linear_data_file()  %>%
          filter(data == "line_data")

        point_data <- non_linear_data_file() %>%
          filter(data == "point_data")

      } else if (useFileData)
      {
        line_data <- linear_data_file()  %>%
          filter(data == "line_data")

        point_data <- linear_data_file() %>%
          filter(data == "point_data")
      }else if (modelT == 'nlls') {
        line_data <- non_linear_data()  %>%
          filter(data == "line_data")

        point_data <- non_linear_data() %>%
          filter(data == "point_data")
      }


      data <- list(line_data = line_data, point_data = point_data)

      drawr(
        data              = data,
        aspect_ratio      = 1,
        linear            = "true",
        free_draw         = TRUE,
        points            = "full",
        x_by
        = 0.25,
        draw_start        = 1,
        points_end        = 20,
        show_finished     = T,
        shiny_message_loc = message_loc,
        x_range           = linear_x_range(),
        y_range           = linear_y_range()
      )

    })


  })
}


ydtApp <- function(...) {
  ui <- fluidPage (ydtUI("hist1"))

  server <- function(input, output, session) {
    ydtServer("hist1")
  }

  shinyApp(ui, server, ...)
}

ydtApp()
