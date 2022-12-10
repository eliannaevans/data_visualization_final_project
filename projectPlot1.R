library(shiny)
library(tidyverse)
library(reshape2)

data_path <- "speed_dating_data.csv"
spd_dat <- read.csv(data_path)
spddat <- subset(spd_dat, select=c(match, dec_o, dec,
                                   field_cd, goal, go_out, date,
                                   gender, samerace,
                                   iid))

spddat %>% mutate(field_cd = recode(field_cd,
                                    '1' = 'EPPS',
                                    '2' = 'NSM',
                                    '3' = 'BBS',
                                    '4' = 'NSM',
                                    '5' = 'ECS',
                                    '6' = 'ATEC/A&H',
                                    '7' = 'Other',
                                    '8' = 'JSOM',
                                    '9' = 'Other',
                                    '10' = 'NSM',
                                    '11' = 'Other',
                                    '12' = 'Other',
                                    '13' = 'EPPS',
                                    '14' = 'ATEC/A&H',
                                    '15' = 'ATEC/A&H',
                                    '16' = 'ATEC/A&H',
                                    '17' = 'ECS',
                                    '18' = 'Other')) -> spddat
spddat %>% mutate(goal = recode(goal,
                                '1' = "Fun Night Out",
                                '2' = "Meet New People",
                                '3' = "Get A Date",
                                '4' = "Relationship",
                                '5' = "To Say I Did It",
                                '6' = "Other")) -> spddat
spddat %>% mutate(go_out = recode(go_out,
                                  '1' = "3+/Week",
                                  '2' = "2/Week",
                                  '3' = "1/Week",
                                  '4' = "2/Month",
                                  '5' = "1/Month",
                                  '6' = "3+/Year",
                                  '7' = "Almost Never")) -> spddat
spddat %>% mutate(date = recode(date,
                                '1' = "3+/Week",
                                '2' = "2/Week",
                                '3' = "1/Week",
                                '4' = "2/Month",
                                '5' = "1/Month",
                                '6' = "3+/Year",
                                '7' = "Almost Never")) -> spddat
spddat <- na.omit(spddat)

data <- spddat
data$numinf <- 1
data$numm <- 1

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput("x", h4("X Variable"),
                  choices=c("Field of Study" = "field_cd",
                            "Goal of Speed Dating" = "goal",
                            "How Often Go Out" = "go_out",
                            "How Often Go On Dates" = "date")),
      selectInput("y", h4("Y Variable"),
                  choices=c("Chance To Match" = "match",
                            "Chance of Parter Pursuing Match" = "dec_o",
                            "Chance of Pursuing Match" = "dec")),
      radioButtons("binary", h4("Categorize By"),
                   choices=c("Gender" = "gender",
                             "Same Race" = "samerace",
                             "None")),
      checkboxGroupInput("type", h4("Plot Type"), "N/A")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput("plot"),
      helpText(h4("Explantion of X, Y, and Categorical Variables")),
      textOutput("x"),
      textOutput("y"),
      textOutput("binary")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  x <- reactive({
    switch(input$x,
           "field_cd" = spddat$field_cd,
           "goal" = spddat$goal,
           "go_out" = spddat$go_out,
           "date" = spddat$date)
  })
  y <- reactive({
    switch(input$y,
           "match" = spddat$match,
           "dec_o" = spddat$dec_o,
           "dec" = spddat$dec)
  })
  binary <- reactive({
    switch(input$binary,
           "gender" = spddat$gender,
           "samerace" = spddat$samerace,
           "None" = NULL)
  })
  alpha_point <- reactive({
    if ("Point" %in% input$type) 1
    else 0
  })
  alpha_line <- reactive({
    if ("Average Line" %in% input$type) 1
    else 0
  })
  alpha_smooth <- reactive({
    if ("Smooth" %in% input$type) 0.5
    else 0
  })
  size_smooth <- reactive({
    if ("Smooth" %in% input$type) 1
    else 0
  })
  alpha_violin <- reactive({
    if ("Violin" %in% input$type) 0.75
    else 0
  })
  size_violin <- reactive({
    if ("Violin" %in% input$type) 0.25
    else 0
  })
  
  observe({
    updateCheckboxGroupInput(session, "type",
                             selected = c("Point"),
                             choices=switch(input$x,
                                            "field_cd" = c("Point", "Violin"),
                                            "goal" = c("Point", "Violin"),
                                            "go_out" = c("Point", "Average Line", "Smooth"),
                                            "date" = c("Point", "Average Line", "Smooth")))
  })
  
  observe({
    if (!is.null(binary())) {
      for (category in unique(x())) {
        data$numinf[x() == category & binary() == 1] = sum(x() == category &
                                                           binary() == 1)
        data$numm[x() == category & binary() == 1] = sum(x() == category &
                                                         y() == 1 & 
                                                         binary() == 1)
        data$numinf[x() == category & binary() == 0] = sum(x() == category &
                                                           binary() == 0)
        data$numm[x() == category & binary() == 0] = sum(x() == category &
                                                         y() == 1 & 
                                                         binary() == 0)
      }
      data$chance = data$numm * 100 / data$numinf # chance of y variable as a percentage
      for (category in unique(x())) {
        for (id in unique(data$iid)) {
          data$idnuminf[x() == category & binary() == 1 & data$iid == id] = sum(x() == category &
                                                                                binary() == 1 &
                                                                                data$iid == id)
          data$idnumm[x() == category & binary() == 1 & data$iid == id] = sum(x() == category &
                                                                              y() == 1 & 
                                                                              binary() == 1 &
                                                                              data$iid == id)
          data$idnuminf[x() == category & binary() == 0 & data$iid == id] = sum(x() == category &
                                                                                binary() == 0 &
                                                                                data$iid == id)
          data$idnumm[x() == category & binary() == 0 & data$iid == id] = sum(x() == category &
                                                                              y() == 1 & 
                                                                              binary() == 0 &
                                                                              data$iid == id)
        }
      }
      data$idchance = data$idnumm * 100 / data$idnuminf # chance of y variable as a percentage
      data %>% mutate(gender = recode(gender,
                                        '0' = "Women",
                                        '1' = "Men")) -> data
      data %>% mutate(samerace = recode(samerace,
                                          '0' = "Different Race/Ethnicity",
                                          '1' = "Same Race/Ethnicity")) -> data
      
      output$plot <- renderPlot({
        ggplot(data, aes_string(x=input$x, color=input$binary, group=input$binary, y="idchance")) +
          geom_point(aes(y=chance), alpha=alpha_line(), shape=17, size=2) + # points on average line
          geom_violin(aes_string(group=paste0("interaction(", paste0(c(input$binary, input$x), collapse =  ", "), ")")),
                      width=alpha_violin(), size=alpha_violin(), draw_quantiles=c(0.5)) +
          geom_smooth(alpha=alpha_smooth(), size=size_smooth(), method = "loess", span=8, formula=y ~ x) +
          geom_line(aes(y=chance), alpha=alpha_line()) +
          geom_point(alpha=alpha_point()) +
          scale_x_discrete(limits=switch(input$x,
                                         "field_cd" = c("ATEC/A&H", "BBS", "ECS", "EPPS", "JSOM", "NSM", "Other"),
                                         "goal" = c("Fun Night Out", "Meet New People", "Get A Date", "Relationship", "To Say I Did It", "Other"),
                                         "go_out" = c("Almost Never", "3+/Year", "1/Month", "2/Month", "1/Week", "2/Week", "3+/Week"),
                                         "date" = c("Almost Never", "3+/Year", "1/Month", "2/Month", "1/Week", "2/Week", "3+/Week"))) +
          scale_colour_discrete(type=c("#7b3294", "#008837")) +
          ylab(switch(input$y,
                 "match" = "Chance To Match (%)",
                 "dec_o" = "Chance of Partner Pursuing Match (%)",
                 "dec" = "Chance of Pursuing Match (%)")) +
          xlab(switch(input$x,
                      "field_cd" = "Field of Study",
                      "goal" = "Goal of Speed Dating",
                      "go_out" = "How Often Go Out",
                      "date" = "How Often Go On Dates")) +
          labs(colour = switch(input$binary,
                               "gender" = "Gender",
                               "samerace" = "Same Race/Ethnicity")) +
          theme_light() +
          theme(text = element_text(family="Avenir"))
      })
    }
    else {
      for (category in unique(x())) {
        data$numinf[x() == category] = sum(x() == category)
        data$numm[x() == category] = sum(x() == category &
                                         y() == 1)
      }
      data$chance = data$numm * 100 / data$numinf # chance of y variable as a percentage
      for (category in unique(x())) {
        for (id in unique(data$iid)) {
          data$idnuminf[x() == category & data$iid == id] = sum(x() == category &
                                                                data$iid == id)
          data$idnumm[x() == category & data$iid == id] = sum(x() == category &
                                                              y() == 1 & 
                                                              data$iid == id)
        }
      }
      data$idchance = data$idnumm * 100 / data$idnuminf # chance of y variable as a percentage
      
      output$plot <- renderPlot({
        ggplot(data, aes_string(x=input$x, y="idchance")) +
          geom_line(aes(y=chance, group=1), alpha=alpha_line(), color = "#7b3294") +
          geom_point(aes(y=chance), alpha=alpha_line(), color = "#7b3294", shape=17, size=2) + # points on average line
          geom_violin(aes_string(group=input$x), width=alpha_violin(),
                      size=alpha_violin(), color="#7b3294", draw_quantiles=c(0.5)) +
          geom_smooth(aes(group=1), alpha=alpha_smooth(), size=size_smooth(), method = "loess", span=8, formula=y ~ x, color="#7b3294") +
          geom_point(alpha=alpha_point(), color="#e66101") +
          scale_x_discrete(limits=switch(input$x,
                                         "field_cd" = c("ATEC/A&H", "BBS", "ECS", "EPPS", "JSOM", "NSM", "Other"),
                                         "goal" = c("Fun Night Out", "Meet New People", "Get A Date", "Relationship", "To Say I Did It", "Other"),
                                         "go_out" = c("Almost Never", "3+/Year", "1/Month", "2/Month", "1/Week", "2/Week", "3+/Week"),
                                         "date" = c("Almost Never", "3+/Year", "1/Month", "2/Month", "1/Week", "2/Week", "3+/Week"))) +
          ylab(switch(input$y,
                      "match" = "Chance To Match (%)",
                      "dec_o" = "Chance of Partner Pursuing Match (%)",
                      "dec" = "Chance of Pursuing Match (%)")) +
          xlab(switch(input$x,
                      "field_cd" = "Field of Study",
                      "goal" = "Goal of Speed Dating",
                      "go_out" = "How Often Go Out",
                      "date" = "How Often Go On Dates")) +
          theme_light() +
          theme(text = element_text(family="Avenir"),
                legend.title=element_blank(), legend.position="none")
      })
    }
  })
  
  output$x <- renderText(switch(input$x,
                                "field_cd" = "Field of Study: The 
                                              participant's reported major 
                                              seperated into groups based on
                                              UTD's schools of study.",
                                "goal" = "Goal of Speed Dating: The 
                                          participant's primary goal for 
                                          participating in speed dating as 
                                          reported prior to the event.",
                                "go_out" = "How Often Go Out: The
                                            participant's reported frequency of
                                            going out, not just for dates, as
                                            reported prior to the event.",
                                "date" = "How Often Go On Dates: The
                                          participant's reported frequency of
                                          going out specifically for dates, as
                                          reported prior to the event."))
  output$y <- renderText(switch(input$y,
                                "match" = "Chance to Match: The percentage of
                                           matches for participant compared to
                                           the total number of partners the
                                           participant had during speed dating.",
                                "dec_o" = "Chance of Partner Pursuing Match: The
                                           percentage of participant's partners
                                           that indicated they would match with
                                           the particpant out of the total
                                           number of partners.",
                                "dec" = "Chance of Pursuing Match: The
                                         percentage of partners the participant
                                         indicated they would match with out of 
                                         the total number of partners."))
  output$binary <- renderText(switch(input$binary,
                                     "gender" = "Gender: The reported gender of
                                                 the participant.",
                                     "samerace" = "Same Race/Ethnicity: Whether
                                                   the participant and their
                                                   partner reported being of the
                                                   same race or ethnicity."))
  
  
  
}

shinyApp(ui = ui, server = server)