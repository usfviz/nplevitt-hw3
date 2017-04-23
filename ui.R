packageList = c('ggplot2', 'shiny', 'plyr', 'reshape', 'resahpe2', 'ggvis')
for (i in 1:length(packageList)) {
  if(! is.element(packageList[i],installed.packages()[,1])) {
    install.packages(packageList[i])
  }
}

library(ggplot2)
library(shiny)
library(plyr)
library(reshape)
library(reshape2)
library(ggvis)

shinyUI(navbarPage("Facebook Data",
                   tabPanel("Bubble Plot",
                            sidebarLayout(
                              sidebarPanel(
                                selectInput('type', 'Select A Post Type', 
                                            choices = c('Link', 'Photo', 'Status', 'Video'), multiple = T),
                                h5("Hover over a point to see the country name; click for more information.",align='center')
                              ),
                              #Render the results
                              mainPanel(ggvisOutput("bubble"))
                              )
                            ),
                   tabPanel("Scatterplot Matrix",
                            pairsD3Output("scatter", width = 1100, height = 700),
                            h5('Click and drag to select points to highlight'),
                            h5('Hover over a point to see if it was paid or free')
                            ),
                   tabPanel("Parallel Coordinates",
                            sidebarLayout(
                              sidebarPanel(uiOutput('par_vars')),
                              mainPanel(plotOutput('parallel'))
                            )
                            
                   )))