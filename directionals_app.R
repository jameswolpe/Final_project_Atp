library(shiny)
library(tidyverse)
#install.packages("shinythemes")
library(shinythemes)

df <- read_csv("merge3.csv")

#coordinates of the baseline + side line 
#save the coordinates as a data frame as "base_side_line"
base_side_line <- data.frame(
  y = c(0, 0, 23.77, 23.77, 0),
  x = c(0, 10.97, 10.97, 0, 0)
)
#coordinates of the service line and center line 
#save the coordinates as a data frame as "serves_center_line"
serves_center_line <- data.frame(
  y = c(5.585, 5.585, 5.585, 18.385, 18.385, 18.385),
  x = c(1.37, 9.6, 5.485, 5.485, 1.37, 9.6)
)
direction <- c(1,2,3)
y <- c(22, 22, 22)
x <- c(2.604, 5.485, 8.365)
court_df <- data.frame(direction, x, y)

# create my own directional lines on the court
court <- ggplot() +
  geom_path(data = base_side_line, aes(x = x, y = y)) +
  geom_path(data = serves_center_line, aes(x = x, y = y)) +
  geom_path(aes(y = c(23.77, 0), x = c(1.37, 1.37))) + # include lower singles lines
  geom_path(aes(y = c(23.77, 0), x = c(9.6, 9.6))) + # include upper singles lines
  geom_path(aes(y = c(11.985, 11.985), x = c(0, 10.97))) + # include dotted net line
  geom_path(aes(y = c(23.77, 0), x = c(3.839, 3.839)), lty = 2, color = "red") +  # my new line for the bottom directional
  geom_path(aes(y = c(23.77, 0), x = c(7.131, 7.131)), lty = 2, color = "red") +
  geom_point(aes(y = 1.77, x= 8.365), size = 3) + # left side 3 
  geom_point(aes(y = 1.77, x = 5.485), size = 3) + # left side 2
  geom_point(aes(y = 1.77, x = 2.604), size = 3) + # left side 1
  
  xlim(c(-1, 11.97)) +
  ylim(c(-10, 27.77)) + 
  scale_x_reverse()+
  theme_void()


# change choices order manually 
# Theme = shiny theme 
# choice names
ui <- fluidPage(shinytheme("paper"),
                sidebarLayout(
                  sidebarPanel(selectizeInput(inputId = "playerselect",
                                              label = "Choose a Player",
                                              selected = "Daniil Medvedev",
                                              choices = levels(factor(df$player))),
                               radioButtons(inputId = "strokeselect",
                                            label = "Select the incoming Stroke",
                                            choices = unique(df$stroke)),
                               radioButtons(inputId = "strokeafterselect",
                                            label = "Select the outgoing Stroke",
                                            choices = unique(df$stroke_after)),
                               radioButtons(inputId = "direction",
                                            label = "Select the incoming direction",
                                            choices = c(1,2,3))),
                  # render print 
                  mainPanel(plotOutput("colplot"),
                            tableOutput("wintab"),
                            plotOutput("courtplot", width = "60%"))
                  
                  
                )
)


server <- function(input, output, session) {
  
  df_oneplayer <- reactive({
    df %>% filter(player == input$playerselect,
                  stroke == input$strokeselect,
                  stroke_after == input$strokeafterselect,
                  direction == input$direction)%>% 
      group_by(direction_after) %>%
      summarise(count = n()) %>%
      mutate(perc = count / sum(count))
    # do a sum of all three
  })
  
  df_oneplayer_sum <- reactive({
    df %>% filter(player == input$playerselect,
                  stroke == input$strokeselect,
                  stroke_after == input$strokeafterselect,
                  direction == input$direction) %>%
      group_by(direction_after) %>%
      summarise(count = n()) %>%
      mutate(perc = count / sum(count),
             n = sum(count)) 
  })
  
  court_df_reactive <- reactive({
    court_df %>% filter(direction == input$direction)
  })
  
  output$wintab <- renderTable({
    table(df_oneplayer_sum()$n)
  })
  
  output$colplot <- renderPlot({
    ggplot(data = df_oneplayer(), aes(x = direction_after, y = perc *100)) +
      geom_col() +
      labs(x = "Shot After", y = "Percentage")
  })
  #this should have controls over witdth and height of the graph
  # df oneplayer sum * the thickness
  output$courtplot <- renderPlot({
    court + 
      geom_point(data = court_df_reactive(), aes(x = x, y = y)) +
      geom_segment(data = court_df_reactive(), aes(x=x, y=y, yend=1.77, xend= 2.604)) +
      geom_segment(data = court_df_reactive(), aes(x=x, y=y, yend=1.77, xend= 5.485)) +
      geom_segment(data = court_df_reactive(), aes(x=x, y=y, yend=1.77, xend= 8.365)) +
      scale_y_reverse()
  })
}
shinyApp(ui, server)
