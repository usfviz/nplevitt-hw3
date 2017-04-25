packageList = c('ggplot2', 'shiny', 'plyr', 'reshape', 'ggvis', 'pairsD3')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(ggplot2)
library(shiny)
library(plyr)
library(reshape)
library(ggvis)
library(pairsD3)

numvars <- c("Page_Total_Likes","Total_Reach","Total_Impressions","Engaged_Users","Total_Consumers",   
             "Total_Consumptions", "Comments", "Likes", "Shares", "Total_Interactions")

shinyUI(navbarPage("Facebook Data Visualizations",
                   tabPanel("Bubble Plot",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput('bub_x', "Select an X-Variable", 
                                            choices = numvars, selected = 'Likes'),
                                selectInput('bub_y', "Select an X-Variable", 
                                            choices = numvars, selected = 'Shares'),
                                selectInput('bub_scale', "Select an Variable to Scale By ", 
                                            choices = numvars, selected = 'Total_Interactions'),
                                selectInput('type', 'Select A Post Type', 
                                            choices = c('Link', 'Photo', 'Status', 'Video'), multiple = T),
                                h5("Hover over a point to see the country name; click for more information.",align='center')
                              ),
                              #Render the results
                              mainPanel(ggvisOutput("bubble"))
                              )
                            ),
                   tabPanel("Small Multiples",
                            sidebarLayout(
                              sidebarPanel(uiOutput('mult_vars')),
                              mainPanel(plotOutput("small_mult", height=600, width=800))
                            )
                   ),
                   tabPanel("Scatterplot Matrix",
                            pairsD3Output("scatter", width = 800, height = 800),
                            h5('Click and drag to select points to highlight'),
                            h5('Hover over a point to see if it was paid or free - this will appear in the upper lefthand corner')
                            ),
                   tabPanel("Parallel Coordinates",
                            sidebarLayout(
                              sidebarPanel(uiOutput('par_vars'),
                                           checkboxInput('par_col',
                                                         'Color Code By Type?',
                                                         value = F)),
                              mainPanel(plotOutput('parallel'))
                            )
                            
                   )))