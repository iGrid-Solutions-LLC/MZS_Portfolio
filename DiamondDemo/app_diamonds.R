#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(RSQLite)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  navbarPage(
    theme = shinytheme("sandstone"),
    "DemoApp",
    tabPanel(
      "Diamond Prices in 2022",
      
      sidebarPanel(
        # style="position:fixed;width:30%", #Uncomment to make sidebar sticky
        h1("Diamond Prices in 2022"),
        h2(strong("Name: ")),
        h3("Zac Smith"),
        h2(strong("Email: ")),
        h3("michaelzsmith09@gmail.com"),
        h2(strong("Phone Number: ")),
        h3("256-708-0238"),
        # HTML("<br><br><br>"),

        h5("This is a demo to showcase the effectiveness of RShiny in conjunction with AWS cloud 
        to provide dynamic and interactive dashboards to present data to customers. The data
        in this demo comes from Kaggle.com."),
        
        h5("The dataset came as a CSV file. For the sake of this demonstration the data was pocessed and 
        cleaned before being put into a normalized SQLite databse. SQLite was chosen because of is 
        light weight nature and ease of use with the R programming language."),
        
        h5("This application demonstrates how data can be drawn and analyzed from a database and then 
        deployed in a quick and efficent manner to a customer with relative ease.")),
      
      
      mainPanel(
        
        h2("SQL Database Tables"),
        p(style="font-size: 18px;","The data that informs this analysis came in the form of a csv file with 11 variables and 53,943 observations.
          The first step was to move the data from a csv format to a normalized SQLite database. In the context of
          databases normalization refers to the process of structurng a relational database in accordance with a a series of normal forms in order 
          to reduce data redundancy and improve data integrity."),
        p(style="font-size: 18px;","First the data was cleaned a bit, replacing some of the column names so that they made more sense for the analysis.
          Then the data separated into 4 tables, one table for each of the categorical variables (cut, clarity and color) and one table for the diamond
          sales data. Each table was given its own unique identifier which will be utilized later to reform the data."),
        p(style="font-size: 18px;","Below is a table that shows the resulting 4 tables, as well as the queries used to pull them from the database."),
        HTML("<br><br><br>"),
        
        DTOutput("sqltables"),
        HTML("<br><br><br>"),
        
        fluidRow(
          column(width = 4,h3(strong("Table: diamond_cut")),DTOutput('cuttbl')),
          column(width = 4,h3(strong("Table: diamond_color")),DTOutput('coltbl')),
          column(width = 4,h3(strong("Table: diamond_clarity")),DTOutput('clatbl'))
        ),
 
        fluidRow(
          column(width = 4, h4(em("Query")),verbatimTextOutput('q1')),
          column(width = 4, h4(em("Query")),verbatimTextOutput('q2')),
          column(width = 4, h4(em("Query")),verbatimTextOutput('q3'))
        ),
        HTML("<br><br><br>"),

        h3(strong("Table: diamond_sales")),
        DTOutput("dsalestbl"),

        h4(em("Query")),
        verbatimTextOutput('q4'),

        HTML("<br><br><br>"),
        
        h2("Diamond Sales"),
        p(style="font-size: 18px;","The analysis requires the complete dataset in order to draw useful insights.Since all of the data was divided out into
          separate tables, the next step is to bring all the data back together by joining the data tables by their unique identifiers."),
        
        DTOutput('tbl'),
        
        h4(em("Query")),
        verbatimTextOutput('q5'),
        HTML("<br><br><br>"),
        
        
        h2("Summary Statistics"),
        p(style="font-size: 18px;","With the data consolidated it is simple to gather some initial information about the dataset. Distributions and 
          frequencies are a common method of exploratory analysis, and the visuals generated are quick and easy to understand."),
       
         tabsetPanel(
          id="tabset1",
          tabPanel("Carat",
                   plotOutput("CaratHist")
          ),
          tabPanel("Length",
                   plotOutput("LengthHist")
          ),
          tabPanel("Width",
                   plotOutput("WidthHist")
          ),
          tabPanel("Depth",
                   plotOutput("DepthHist")
          ),
          tabPanel("Color",
                   plotOutput("ColorBar")),
          tabPanel("Cut",
                   plotOutput("CutBar")),
          tabPanel("Clarity",
                   plotOutput("ClarityBar")),
        ),
        HTML("<br><br><br>"),
        
        h2("Correlation Analysis"),
        p(style="font-size: 18px;","Analysis at this point an take many different paths. One of the simplest things to do with a dataset such
          as this is to look at the correlation between variables and determine if any of the variables have a positive or negative relationship
          to one another. This information can then lead to futher and more in depth analysis depending on the need."),
        tabsetPanel(
          id="tabset2",
          tabPanel("Correlation Matrix",
                   plotOutput("hm")),
          tabPanel("Scatter Plot",
                   selectInput(inputId="Varx",
                               label = "Select X-axis Variable:",
                               choices=c("carat","color","clarity","cut","price","length.mm","width.mm","depth.mm")),
                   selectInput(inputId="Vary",
                               label = "Select Y-axis Variable:",
                               choices=c("carat","color","clarity","cut","price","length.mm","width.mm","depth.mm")),
                   plotOutput("scatter") )
        ),
        HTML("<br><br><br>")
        
        
        #   h2("Scatter Plot"),
        #   h3("Scatter Plot Controls"),
        #   selectInput(inputId="Varx",
        #               label = "Select X-axis Variable:",
        #               choices=c("carat","color","clarity","cut","price","length.mm","width.mm","depth.mm")),
        #   selectInput(inputId="Vary",
        #               label = "Select Y-axis Variable:",
        #               choices=c("carat","color","clarity","cut","price","length.mm","width.mm","depth.mm")),
        #   plotOutput("scatter"),
        # 
        # h1("plot1"),
        # plotOutput("hm")
        
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  conn<-dbConnect(RSQLite::SQLite(),"diamond.db")
  #tables in database
  tables<-tibble(dbListTables(conn))
  tables<-tables%>%
    rename(Tables='dbListTables(conn)')
  
  #cut table
  cutdf<-dbGetQuery(conn,"Select * from diamond_cut")
  
  #color table
  coldf<-dbGetQuery(conn,"Select * from diamond_color")
  
  #clarity table
  cladf<-dbGetQuery(conn,"Select * from diamond_clarity")
  
  #diamond sales table
  dsales<-dbGetQuery(conn,"select * from diamond_sales")
  
  #joined diamond sales table
  diamonds<-dbGetQuery(conn,
                               "Select diamond_sales.id,
           diamond_sales.carat,
           diamond_cut.cut,
           diamond_color.color,
           diamond_clarity.clarity,
           diamond_sales.depth,
           diamond_sales.'length.mm',
           diamond_sales.'width.mm',
           diamond_sales.'depth.mm',
           diamond_sales.price
           from diamond_sales
           inner join diamond_color on diamond_sales.colorid=diamond_color.id
           inner join diamond_cut on diamond_cut.id=diamond_sales.cutid
           inner join diamond_clarity on diamond_sales.clarityid=diamond_clarity.id")
  
  
  dbDisconnect(conn)
  
  output$sqltables<-renderDataTable({
    datatable(tables)
    
  })
  

  output$cuttbl<-renderDataTable({
    datatable(cutdf)

  })

  output$coltbl<-renderDataTable({
    datatable(coldf)

  })

  output$clatbl<-renderDataTable({
    datatable(cladf)

  })
  
    output$dsalestbl<-renderDataTable({
      datatable(dsales)



    })

  output$q1<-renderText({
    "SELECT *
FROM diamond_cut"
  })

  output$q2<-renderText({
    "SELECT *
FROM diamond_color"
  })

  output$q3<-renderText({
    "SELECT *
FROM diamond_clarity"
  })

  output$q4<-renderText({
    "SELECT *
FROM diamond_sales"
  })
  
  
  # diamonds<-read.csv("Diamonds Prices2022.csv")
  
  output$tbl = renderDT(
    diamonds,filter="top", options = list(lengthChange = FALSE)
  )
  
  output$q5<-renderText({
"Select diamond_sales.id,
diamond_sales.carat,
diamond_cut.cut,
diamond_color.color,
diamond_clarity.clarity,
diamond_sales.depth,
diamond_sales.'length.mm',
diamond_sales.'width.mm',
diamond_sales.'depth.mm',
diamond_sales.price
from diamond_sales
inner join diamond_color on diamond_sales.colorid=diamond_color.id
inner join diamond_cut on diamond_cut.id=diamond_sales.cutid
inner join diamond_clarity on diamond_sales.clarityid=diamond_clarity.id"
  })
  
  output$CaratHist<-renderPlot({
    diamonds%>%
      ggplot(aes(x=carat))+
      geom_histogram()+
      ggtitle("Carat of Diamonds in 2022")
  })
  
  output$LengthHist<-renderPlot({
    diamonds%>%
      ggplot(aes(x=length.mm))+
      geom_histogram()+
      ggtitle("Length of Diamonds in 2022")
  })
  
  output$WidthHist<-renderPlot({
    diamonds%>%
      ggplot(aes(x=width.mm))+
      geom_histogram()+
      ggtitle("Width of Diamonds in 2022")
  })
  
  output$DepthHist<-renderPlot({
    diamonds%>%
      ggplot(aes(x=depth.mm))+
      geom_histogram()+
      ggtitle("Depth of Diamonds in 2022")
  })
  
  output$ColorBar<-renderPlot({
    diamonds%>%
      ggplot(aes(x=color,fill=color))+
      geom_bar()+
      ggtitle("Frequency of Color in Diamond Sales")
  })
  
  output$CutBar<-renderPlot({
    diamonds%>%
      ggplot(aes(x=cut,fill=cut))+
      geom_bar()+
      ggtitle("Frequency of Cut in Diamond Sales")
  })
  
  output$ClarityBar<-renderPlot({
    diamonds%>%
      ggplot(aes(x=clarity,fill=clarity))+
      geom_bar()+
      ggtitle("Frequency of Clarity in Diamond Sales")
  })
  
  output$scatter<-renderPlot({
    xvar<-input$Varx
    yvar<-input$Vary
    print(xvar)
    
    diamonds%>%
      ggplot(aes_string(x=xvar,y=yvar))+
      geom_point()+
      geom_smooth(method=lm)
  })
  
  output$hm <-renderPlot({
    diamond_features<-diamonds%>%
      select(-1)
    
    #turning data into matrix to do some analysis
    matrix_d<-data.matrix(diamond_features)
    
    corr<-round(cor(matrix_d),
                digits = 2 # rounded to 2 decimals
    )
    
    ggcorrplot(corr,
               lab = TRUE,
               type = "upper")
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
