# <---------- Libraries --------------->

# reads the csv file
library('readxl')
# 'group by' & 'summarize' functions
library('dplyr')
# plotting graphs(pie chart, scatter plot, ...)
library("ggplot2")
# the gui is made by shiny
library("shiny")
# themes for the gui
library("shinythemes")
# apriori algorithm to generate association rules
library(arules)
# used to render graphs into the gui (?????)
library("ggplotgui")
# scaling graphical representation
library(scales)
# generating tables
library(data.table)



# <----------- GUI (UI) ------------->

ui <- fluidPage(
  theme = "darkly",
  titlePanel("Dashboard"),
  sidebarLayout(
    sidebarPanel(
      
      # Side panel for taking user input
      #
      # 1- file input to enter the csv file
      # 2- Numeric input for number of clusters in kmeans algorithm
      # 3- Slider for the Minimum support in apriori algorithm (change into a more acurate input method ???)
      # 4- Slider for the Minimum confidence in apriori algorithm (change into a more acurate input method ???)
      
      fileInput("file", "Choose CSV File" , accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv")
        ),
      numericInput('num_clusters', 'Number Of Clusters', 3, min = 2, max = 4),
      sliderInput("min_support", "Min. Support",min = 0.001, max = 1,value = 0.03, step = 0.001),
      sliderInput("min_confidence", "Min. Confidence",min = 0.001, max = 1,value = 0.03, step = 0.001),
      numericInput('min_length', 'Minimum rule length', 2, min = 1, max = 50)
    ),
    mainPanel(
      
      # Main panel for displaying plots and tables
      #
      # 1- table to display kmeans clustering    2- table to display association rules
      # 3- pie chart to compare cash and credit usage
      # 4- scatter plot to plot spending with age
      # 5- bar graph to compare the total spending of multiple cities 
      # 6- box plot to display the distribution of total spending
      textOutput("ctitle"),
      dataTableOutput("clusteringtable"),
      textOutput("atitle"),
      dataTableOutput("arulestable"),
      plotOutput("piechart"),
      plotOutput("schatterplot"),
      plotOutput("bargraph"),
      plotOutput("boxplot")
    )
  )
  
)




# <------------- GUI(server) / Data visualization --------------->

server <- function(input, output){
  
  # <---------- File reading --------------->
  

  GRC <- reactive({
    
    req(input$file)
    
    df <- read.csv(input$file$datapath)
    
    # <----------- Data cleaning / Verification ------------>
    
    # Removing duplicates is unnecessary 
    # Removing outlines is unnecessary
    
    # Convert to numeric (NEEDED for Data Visualization)
    df$total <- as.numeric(df$total)
    df$count <- as.numeric(df$count)
    df$rnd <- as.numeric(df$rnd)
    df$age <- as.numeric(df$age)
    
    #removing rows N|A values (if present)
    df <- na.omit(df)
    
    return(df)
    
  })
  
  
  #  <-----k-means clustering between total and age (table)------>
  
  # sets the title
  output$ctitle <- renderText({
    'Kmeans Clustering :'
  })
  
  # as the outputs will change with the change of user inputs, the whole thing has to be 'Reactive'
  # as the render... functions are reactive, interactivity will not be an issue
  
  output$clusteringtable <- renderDataTable({
    # using the 'pipe' operator (%>%), group the data set by rnd(id of each customer), age, customer(name) -
    # - Summaries collapses data of a whole column into a single variable
    df_new <- GRC() %>%
      group_by(rnd, age, customer) %>%
      summarise(total = sum(total), .groups = 'drop')
    #for reproducibility (ie. random numbers will be the same every time)
    set.seed(123)
    # Perform k-means clustering
    kmeans_result <- kmeans(df_new[, c("age", "total")], centers = input$num_clusters)
    # Add the clustering info to the data frame
    df_new$cluster <- kmeans_result$cluster
    # render the data frame into as a data table
    as.data.table(df_new)
  })
  
  

  # <--------- Generating association rules using apriori algorithm (table) ------->
  
  # Same interactivity as kmeans
  
  # sets the title
  output$atitle <- renderText({
    'Association rules ~ using Apriori algorithm :'
  })
  
  output$arulestable <- renderDataTable({
    
    # Convert the data frame column into a list
    items <- strsplit(as.character(GRC()$items), ",")
    # Convert the list into transactions
    trans <- as(items, "transactions")
    # Perform the Apriori algorithm
    rules <- apriori(trans, parameter = list( supp = input$min_support, conf = input$min_confidence, minlen=input$min_length))
    # Convert the rules into a data frame (to turn into a data table)
    rules_df <- as(rules, "data.frame")
    # Convert the data frame to a data table
    as.data.table(rules_df)
  })
  
  
  # <------------ Compare cash and credit totals (Using Pie Charts) ------------->
  
  
  # renders the plot to the ui
  output$piechart <- renderPlot({
    
    cash_total <- sum(GRC()$total[GRC()$paymentType == "Cash"])
    credit_total <- sum(GRC()$total[GRC()$paymentType == "Credit"])
    
    Cash_Credit_Comparison <- data.frame(
      PaymentType = c("Cash", "Credit"),
      Total = c(cash_total, credit_total)
    )
    
    # function used to get the percent with 4 decimal points to tell the difference
    Cash_Credit_Comparison$Percent <- round(Cash_Credit_Comparison$Total / sum(Cash_Credit_Comparison$Total) * 100, 4)
    
    ggplot(Cash_Credit_Comparison, aes(x = "", y = Total, fill = PaymentType)) + # pie charts don't have x axis that's why it's empty
      geom_bar(width = 1, stat = "identity") + #creates the bar plot identity to use the actual values in total
      geom_text(aes(label = paste0(Percent, "%")), 
                position = position_stack(vjust = 0.5)) +  # this adds text for the percentages in the middle
      coord_polar("y", start = 0) +  #converts the bar plot into a pie chart
      theme_void() +   # removes all non-data
      labs(title = "Comparison of the total spending of cash and credit", 
           x = "Payment Type", y = "Total Spending") +
      scale_y_continuous(labels = percent_format())   # y-axis changes to percentages
  })
  
  
  
  
  # <---------- Comparison of age and sum of total spending (Using Scatter Plot) ---------->

  # renders the plot to the ui
  output$schatterplot <- renderPlot({
    
    Age_Total_Spending_Comparison <- GRC() %>%
      group_by(age) %>%
      summarise(Total_Age_Spending = sum(total))  #groups the data by unique ages summarize calculates total spending for each age group
    
    ggplot(Age_Total_Spending_Comparison, aes(x = age, y = Total_Age_Spending)) +   
      geom_point(color = "green") + #creates the points for the scatter plot
      labs(title = "Comparison of age and total spending", x = "Age", y = "Total Spent by Each Age")
  })
  
  
  # <----------- Showing each city's total spending (using bar graph) ------------>

  # renders the plot to the ui
  output$bargraph <- renderPlot({
    
    unique_cities <- unique(GRC()$city) # checking what cities are mentioned in the GRC
    # "Hurghada" "Aswan"  "Dakahlia"   "Sohag"  "Giza" "Gharbia"  "Cairo"  "Alexandria" "Port Said"  "Fayoum"
    
    #Getting the total spending of each city
    Hurghada_total <- sum(GRC()$total[GRC()$city == "Hurghada"])
    Aswan_total <- sum(GRC()$total[GRC()$city == "Aswan"])
    Dakahlia_total <- sum(GRC()$total[GRC()$city == "Dakahlia"])
    Sohag_total <- sum(GRC()$total[GRC()$city == "Sohag"])
    Giza_total <- sum(GRC()$total[GRC()$city == "Giza"])
    Gharbia_total <- sum(GRC()$total[GRC()$city == "Gharbia"])
    Cairo_total <- sum(GRC()$total[GRC()$city == "Cairo"])
    Alexandria_total <- sum(GRC()$total[GRC()$city == "Alexandria"])
    Port_Said_total <- sum(GRC()$total[GRC()$city == "Port Said"])
    Fayoum_total <- sum(GRC()$total[GRC()$city == "Fayoum"])
    
    City_Spending <- data.frame(
      City = c("Hurghada" ,"Aswan", "Dakahlia", "Sohag", "Giza", "Gharbia", 
               "Cairo", "Alexandria", "Port Said", "Fayoum"),
      Total = c(Hurghada_total, Aswan_total , Dakahlia_total, Sohag_total, Giza_total, Gharbia_total, 
                Cairo_total, Alexandria_total, Port_Said_total, Fayoum_total)
    )
    
    ggplot(City_Spending, aes(x=reorder(City, -Total), y=Total)) + #reorder with -total makes it go from largest to smallest
      geom_bar(stat="identity", fill ="steelblue") +  #creates the bar plot that can represent the actual values
      theme_bw() +
      labs(title = "City Spending", 
           x="City", y="Total Spending")
  })
  
  
  # <------------- Display the distribution of total spending (Using Histogram) ---------->
#  output$boxplot <- renderPlot({
#    ggplot(GRC(), aes(x=total)) +
#      geom_histogram(binwidth=1, fill="black", color="black") + #bin width makes it take values like 1,2,3,4,5... bin width 10 wouldbe 0-10,10-20....
#      labs(title="Display of distribution of Total Spending", x="Total Spending", y="Frequency")
#  })
  
  # <------------- Display the distribution of total spending (Using Box Plot) ---------->
  
  output$boxplot <- renderPlot({
    ggplot(GRC(), aes(x = "", y = total)) +
      geom_boxplot(fill = "white", color = "blue") + 
      labs(title = "Display of distribution of Total Spending", x = "", y = "Total Spending")
  })
  
  
  
}


# <------------ Putting the ui and server together in a shiny web app -------------->
shinyApp(ui=ui, server = server)
