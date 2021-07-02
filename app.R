library(shiny)
library(tidyverse)
library(ggplot2)

###################
NYFedData <- read.csv("debt_balance.csv") 
i <- 1
while (i < length(NYFedData[,1])) 
{
  NYFedData[i,1]<-sub(":", " \\1", NYFedData[i,1]) #substitute ":" with space
  i = i+1
}

###################
ui <- fluidPage(
  
  titlePanel("Interactive Graph and Table App"),
  
  
  
  
  
  sidebarLayout(
    sidebarPanel(
      
      
      p("The graph is a visualization of the historical total debt data (between the first quarter of 2003 and the first quarter of 2021) of the private sector in the United States segmented by the debt type."),
      
      h3("Graph:"),
      p("Please use the ",  strong("drop down menu")," (below) to select which debt type to plot on the Y axis. Quarters are plotted on the X axis."),
      h3("Table:"),
      p("The same data are shown in the",strong("table"),"  (below the graph). The data can be filtered using the radio buttons (below the drop down menu) and searched using the search menu."),
      p(span("The controls of the table and the graph are decoupled.", style = "color:green")),
      br(),
      em("Data Source:"),
      em("Total Debt Balance and its Composition in the Quarterly Report on Household Debt and Credit by the Federal Reserve Bank of New York. Released in May 2021.", "Data can be accessed ", a("HERE", href = "https://www.newyorkfed.org/microeconomics/hhdc")),
     
      br(),
      br(),

      selectInput("plot_var", 
                  label = "Drop down menu: Select a variable to plot on the Y axis:",
                  choices = c("Mortgage",
                              "HE.Revolving",
                              "Auto.Loan", 
                              "Credit.Card", 
                              "Student.Loan", 
                              "Other", 
                              "Total" ),
                  selected = "Mortgage"),
      
      br(),
      br(),
      
      conditionalPanel(
       'input.dataset === "NYFedData"',
        checkboxGroupInput("show_vars", "Radio buttons: Select the columns to show in the table:",
                           names(NYFedData), selected = names(NYFedData)) )),
    
###################
    mainPanel(
      plotOutput("Plot1"),
      br(), 
      br(),
      tabsetPanel(
        id = 'dataset',
        tabPanel("NYFedData", DT::dataTableOutput("mytable1")) 
      )
      
    ) 
    ) 
  
  
)



###################
server <- function(input, output) {

  output$Plot1 <- renderPlot({
    
    plot <- ggplot(NYFedData, aes(Quarter, NYFedData[,input$plot_var]))
    plot <- plot + geom_bar(stat = "identity", fill = "darkolivegreen4")
    plot <- plot + ggtitle("Total Debt Balance by its Composition Type. Trillions of $")
    
    plot <- plot + xlab("Quarters") +   theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
    plot <- plot + ylab(input$plot_var)
    print(plot)
  })


  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(NYFedData[, input$show_vars, drop = FALSE], options = list( pageLength = 73) )
  })


  
}
shinyApp(ui, server)
