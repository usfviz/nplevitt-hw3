packageList = c('ggplot2', 'shiny', 'plyr', 'reshape', 'resahpe2', 'ggvis', 'pairsD3', 'MASS')
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
library(pairsD3)
library(MASS)

df <- read.csv('dataset_Facebook.csv', sep=';')
df$Paid <- ifelse(df$Paid == 1, 'Paid', 'Not Paid')
clean_df <- df[complete.cases(df),]
pairs_df <- clean_df[, names(clean_df) %in% c("Lifetime.Post.Total.Reach", 
                                  "Lifetime.Post.Total.Impressions", 
                                  "Lifetime.Post.Consumptions",
                                  "Total.Interactions")]
names(pairs_df) <- c('Total Reach', 'Total Impressions', 'Total Consumptuin', 'Total Interactions')
catvars <- c('Type', 'Category', 'Post.Month', 'Post.Weekday', 'Post.Hour', 'Paid')
par_df <- df[,!names(df) %in% catvars]

server <- function(input, output, session) {
  autoInvalidate <- reactiveTimer(500)

  bubble <- reactive({
    if(!is.null(input$type)) {
      plot_df <- subset(df, Type %in% input$type)
    } else {
      plot_df <- df
    }
    plot_df %>% 
      subset(!is.na(like)) %>% 
      subset(!is.na(share)) %>%
      subset(!is.na(Type)) %>% 
      ggvis(prop("x", as.name('like')),
            prop("y",as.name('share')),
            fill = ~Type,
            fillOpacity := 0.5, fillOpacity.hover := 1,
            stroke := NA, stroke.hover = ~Type, strokeWidth := 4, strokeOpacity := 0.7) %>% 
      scale_numeric("size", range = c(50, 500), nice = FALSE) %>%
      layer_points(prop("size",as.name('Total.Interactions'))) %>%
      ggvis::hide_legend('size') %>% 
      add_axis("x", title = 'Number of Likes', title_offset = 50) %>%
      add_axis("y", title = 'Number of Shares', title_offset = 50) %>%
      scale_numeric("x", domain = c(0,5500), nice = FALSE) %>%
      scale_numeric("y", domain = c(0,850), nice = FALSE) %>%
      add_axis("x", orient = "top", ticks = 0, title_offset = 50,
               title = "Likes versus Shares, Scaled by Total Interactions",
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0))) %>% 
      add_tooltip(function(df){
        paste0("Total Interactions: <b>", df$Total.Interactions, "</b><br>",
               "Type: <b>", df$Type, "</b><br>",
               "Likes: <b>", df$like, "</b><br>",
               "Shares: <b>", df$share, "</b>")
      }, "click") %>% 
      add_tooltip(function(df){
        paste0("Total Interactions: <t><b>", df$Total.Interactions, "</b>")
      }, "hover") %>% 
      set_options(width = 800, height = 600, renderer = "svg")
  })
    
  bubble %>% 
    bind_shiny('bubble', 'bubble_stuff')
  
  output$scatter <- renderPairsD3({
    pairs_df %>% 
    pairsD3(group = clean_df$Type, tooltip = clean_df$Paid, leftmar=60, topmar=0)
  })
  
  output$par_vars <- renderUI({
    selectInput('parvars', 'Select Variables to Visualize', multiple=T,
                choices <- names(par_df), selected = names(par_df)[1:3])
  })
  output$parallel <- renderPlot({
    validate(
      need(length(input$parvars) > 0, "Please select variables to visualize")
    )
    par_df[,names(par_df) %in% input$parvars] %>% 
      parcoord(col=rainbow(nrow(par_df)), var.label=TRUE)
  })
  
}




