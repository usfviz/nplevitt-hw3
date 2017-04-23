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

# Read in the data
df <- read.csv('dataset_Facebook.csv', sep=';')

# Clean up the names
names(df) <- c('Page_Total_Likes', 'Type', 'Category', 'Post_Month', 'Post_Weekday', 'Post_Hour', 'Paid',
               'Total_Reach', 'Total_Impressions', 'Engaged_Users', 'Total_Consumers', 'Total_Consumptions',
               'Post_Impressions_Likers', 'Post_Reach_Likers', 'Lifetime_People_Engagement', 'Comments', 'Likes', 'Shares', 'Total_Interactions')

# Coerce binary 'paid' variable to categorical strings
df$Paid <- ifelse(df$Paid == 1, 'Paid', 'Not Paid')

# Coerce Month number to abbreviation
df$Post_Month <- month.abb[df$Post_Month]

# Remove rows with NA values. Remove boring variables.
clean_df <- df[complete.cases(df),]
clean_df <- clean_df[, !names(clean_df) %in% c('Post_Impressions_Likers', 'Post_Reach_Likers', 'Lifetime_People_Engagement')]

# Initialize df for paired scatterplot visualization
pairs_df <- clean_df[, names(clean_df) %in% c('Total_Reach', 'Total_Impressions',
                                              'Total_Consumptions','Total_Interactions')]

# Initialize df for parallel plot visualization (remove categorical variables)
catvars <- c('Type', 'Category', 'Post_Month', 'Post_Weekday', 'Post_Hour', 'Paid')
numvar_indx <- !names(clean_df) %in% catvars
numvars <- names(clean_df)[numvar_indx]
par_df <- clean_df[, numvar_indx]

# Initialize df of only small mu features
df_num <- par_df
df_num$Post_Month <- clean_df$Post_Month
mdf <- melt(df_num, id.vars = 'Post_Month')
cdf <- cast(mdf, Post_Month ~ variable, mean)
mdf2 <- melt(cdf, id.vars = 'Post_Month')

# Initialize Shiny Server
server <- function(input, output, session) {
  # Set a timer to .5 seconds
  autoInvalidate <- reactiveTimer(500)
  
  # Build the bubble plot in ggvis. 
  bubble <- reactive({
    # Subset by user selected types
    if(!is.null(input$type)) {
      plot_df <- subset(df, Type %in% input$type)
    } else {
      plot_df <- df
    }
    validate(
      need(!is.null(input$bub_x), 'select a var')
    )
    
    xmin <- floor(min(clean_df[,names(clean_df) == input$bub_x]))
    xmax <- ceiling(max(clean_df[,names(clean_df) == input$bub_x]))
    ymin <- floor(min(clean_df[,names(clean_df) == input$bub_y]))
    ymax <- ceiling(max(clean_df[,names(clean_df) == input$bub_y]))
    
    # Build the ggvis plot.
    plot_df %>% 
      subset(!is.na(Likes)) %>% 
      subset(!is.na(Shares)) %>%
      subset(!is.na(Type)) %>% 
      ggvis(prop("x", as.name(input$bub_x)),
            prop("y",as.name(input$bub_y)),
            fill = ~Type,
            fillOpacity := 0.5, fillOpacity.hover := 1,
            stroke := NA, stroke.hover = ~Type, strokeWidth := 4, strokeOpacity := 0.7) %>% 
      scale_numeric("size", range = c(50, 500), nice = FALSE) %>%
      layer_points(prop("size",as.name(input$bub_scale))) %>%
      ggvis::hide_legend('size') %>% 
      add_axis("x", title = paste('Number of', input$bub_x), title_offset = 50) %>%
      add_axis("y", title = paste('Number of', input$bub_y), title_offset = 100) %>%
      scale_numeric("x", domain = c(xmin,xmax), nice = FALSE) %>%
      scale_numeric("y", domain = c(ymin,ymax), nice = FALSE) %>%
      add_axis("x", orient = "top", ticks = 0, title_offset = 50,
               title = paste(input$bub_x, " versus ", 
                             input$bub_y, ", Scaled by ", input$bub_scale, sep=' '),
               properties = axis_props(
                 axis = list(stroke = "white"),
                 labels = list(fontSize = 0))) %>% 
      add_tooltip(function(df){
        paste0("Total Interactions: <b>", df$Total_Interactions, "</b><br>",
               "Type: <b>", df$Type, "</b><br>",
               "Likes: <b>", df$Likes, "</b><br>",
               "Shares: <b>", df$Shares, "</b>")
      }, "click") %>% 
      add_tooltip(function(df){
        paste0("Total Interactions: <t><b>", df$Total_Interactions, "</b>")
      }, "hover") %>% 
      set_options(width = 800, height = 600, renderer = "svg")
  })
  # Attach the ggvis plot  
  bubble %>% 
    bind_shiny('bubble', 'bubble_stuff')
  
  # Build the scatterplot using the pairsD3 package. TODO: Fix margins.
  output$scatter <- renderPairsD3({
    pairs_df %>% 
    pairsD3(group = clean_df$Type, tooltip = clean_df$Paid, leftmar=30, topmar=0, width=800)
    
  })
  
  # Build the input for selecting variables for the parallel plot.
  output$par_vars <- renderUI({
    selectInput('parvars', 'Select Variables to Visualize', multiple=T,
                choices <- names(par_df), selected = names(par_df)[1:3])
  })
  
  # Build the parallel plot using the parcoord function w/n the MASS package.
  output$parallel <- renderPlot({
    validate(
      need(length(input$parvars) > 1, "Please select at least two variables to visualize")
    )
    par_df[,names(par_df) %in% input$parvars] %>% 
      parcoord(col=rainbow(nrow(par_df)), var.label=TRUE)
  })
  
  output$mult_vars <- renderUI({
    selectInput('multvars', 'Select Variables to Visualize', multiple=T,
                choices <- names(cdf)[2:ncol(cdf)], selected = names(cdf)[2:4])
  })
  
  output$small_mult <- renderPlot({
    validate(
      need(length(input$multvars > 0), "Please select at leat one variable to visualize.")
    )
    mult_df <- subset(mdf2, variable %in% append(input$multvars, 'Post_Month'))
    ggplot(data=mult_df, aes(x=Post_Month, y=value, group=1)) + 
      geom_line() + 
      facet_grid(variable ~ ., scales = 'free')
  })
}




