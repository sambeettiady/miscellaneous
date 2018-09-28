library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(jpeg)

x = read_csv('ipl_data.csv',col_names = T)

players = unique(c(unique(x$batsman),unique(x$bowler)))

ui = fluidPage(selectInput(inputId = 'player',label = 'Choose Player',choices = players,size = 10),
               imageOutput(outputId = 'Player_Photo',hover = 'Player Photo'))

server = function(input,output){
    output$Player_Photo = renderImage({
        image = readJPEG(source = paste('player_photos/',input$player,'.jpg'))
        image
    })
}

shinyApp(ui = ui,server = server)