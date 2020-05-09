library(shiny)
source('./server/utilities.R')
source("./data/data.R")
library(reshape2)
library(plotly)
library(tidyr)
source("./data/machine_learning.R")

source('./server/utilities.R')

# define server logic

library(stringr)



server <- function(input, output) {
  values <-
    reactiveValues(dataset = NULL,
                   molten_dataset  = NULL)
  
  grocery_df <- shiny::reactiveValues()
  
  recipe_df <- shiny::reactiveValues()
  
  grocery_data <- shiny::reactiveValues()
  
  bubble_df<-shiny::reactiveValues()
  
  density_df<-shiny::reactiveValues()
  

  recipe_df$df <- data.frame("Recipes" = character(),
                             stringsAsFactors = F)
  
  grocery_data$df <-
    data.frame(
      "Ingredients" = character(),
      "Weight" = integer(),
      stringsAsFactors = F
    )
  
  
  
  observeEvent(input$recipe, {
    output$quantity <- renderUI({
      numericInput(
        'number',
        'Enter the number of servings',
        value = 1,
        min = 0,
        max = 100,
        step = 1
      )
    })
    
    output$add <- renderUI({
      actionButton(
        inputId = "Add",
        label = "Add",
        icon = icon("cart-plus")
      )
    })
    values$deletedRows <- NULL
    values$deletedRowIndices = list()
  })
  
  observeEvent(input$Add, {
    ingredients <-
      as.vector(get_ingredients(input$recipe))
    
    recipe_df$df[nrow(recipe_df$df) + 1,] <- input$recipe
    
    #grocery_df$df <- as.data.frame(grocery_df$df, stringsAsFactors = F)
    grocery_data$df <-
      as.data.frame(grocery_data$df,
                    "Weight" = integer(),
                    stringsAsFactors = F)
    Weights <- as.vector(get_Weight(input$recipe))
    
    for (i in 1:length(ingredients)) {
      if (!(any(grocery_data$df$Ingredients == ingredients[i]))) {
        grocery_data$df[nrow(grocery_data$df) + 1,] <-
          c(ingredients[i], round(as.double(Weights[i]), 2))
      }
      
      else
      {
        k <-
          grocery_data$df[grocery_data$df$Ingredients == ingredients[i],]
        weight <-
          as.double(grocery_data$df$Weight[grocery_data$df$Ingredients == ingredients[i]]) + as.double(Weights[i])
        grocery_data$df$Weight[grocery_data$df$Ingredients == ingredients[i]] <-
          round(weight, 2)
      }
      
      
      
    }
    
    output$groceryListUI <- renderUI({
      box(
        title = "Grocery List",
        solidHeader = T,
        width = 12,
        collapsible = T,
        #h5(input$recipe),
        DT::DTOutput("grocery_data")

      )
    })
    
   # output$Developer <-  renderUI({
   #   widgetUserBox(
   #   title = "Soumyajeet Patra",
   #   subtitle = "MS DAE 2019",
   #   type = NULL,
   #   width = 12,
   #   src = "./www/Soumyajeet.png",
   #   background = TRUE,
   #   backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
   #   closable = TRUE,
   #   "R Shiny App Developer",
   # )
   # })
    
    output$RecipeListUI <- renderUI({
      box(
        title = "Recipes...",
        solidHeader = T,
        width = 12,
        collapsible = T,
        div(DT::DTOutput("recipe_df"), style = "font-size: 70%;")
      )
    })
    
    output$instructionUI <- renderUI({
      selectInput("InstructionRecipe",
                  label = "Instructions",
                  choices = recipe_df$df$Recipes)
    })
    output$instruction_URL <- renderUI({
      selectInput("InstructionURL",
                  label = "Instructions",
                  choices = recipe_df$df$Recipes)
    })
    observeEvent(input$InstructionURL,{
      link <- recipe_data$url[recipe_data$title == input$InstructionURL]
      
      output$url <- renderUI({
        return(actionButton("url",
                            label = "Link to Web Page",
                            icon = icon("twitter"),
                            size = "lg",
                            style = "bordered",
                            onclick = sprintf("window.open('%s')", link)))})
    })
    
    df <-
      data.frame('Nutrition.Name' = character(), 'Value' = integer())
    values$number <- input$number
    for (i in recipe_df$df$Recipes) {
      de <- nutri_table(recipe_data, i, values$number)
      df <- rbind(df, de)
    }
    output$table2 <- DT::renderDataTable({
      shiny::validate(need(df, ''))
      df <-
        df %>% group_by(Nutrition.Name) %>% summarize(Value = sum(Value))
      u <- c('KJ(cal)', 'gram', 'gram', 'gram', 'mg', 'gram')
      values$col1 <- data.frame(df, units = u)
      values$col1
    })
    
    
    output$centralPlot <- renderPlotly({
      aggregation_of_ingredients <-
        grocery_data$df %>% group_by(Ingredients) %>% summarise(totalWeight = round(sum(Weight),2))
      trace2 <- list(
        hole = 0.8,
        type = "pie",
        labels = aggregation_of_ingredients$Ingredients,
        values = aggregation_of_ingredients$totalWeight,
        showlegend = F
      )
      layout <-
        list(
          title = "Compositions of Ingredients by Weight",
          family= "Lobster",
          color = 'rgb(128,177,221)',
          xaxis = list(domain = c(0.33, 0.67)),
          yaxis = list(domain = c(0.33, 0.67))
        )
      p <- plot_ly()
      p <-
        add_trace(
          p,
          hole = trace2$hole,
          type = trace2$type,
          labels = trace2$labels,
          values = trace2$values,
          showlegend = trace2$showlegend
        )
      p <-
        layout(
          p,
          title = layout$title,
          xaxis = layout$xaxis,
          yaxis = layout$yaxis,
          paper_bgcolor = 'rgba(0,0,0,0)',
          plot_bgcolor = 'rgba(0,0,0,0)'
        )
      p
    })
    
    output$barplot <- renderUI({
      #   box(grocery_data$df %>% group_by(Ingredients) %>% summarise(totalWeight = sum(Weight)) %>% plot_ly(
      #     x = ~ Ingredients,
      #     y = ~ totalWeight,
      #     type = "bar"
      #   ) %>% layout(title = "Distribution of Groceries", xaxis = list(title = "Groceries"), yaxis = list(title = "Total Weight in Grams")))
    })
    #box(recipe_df$df$Recipes[i],cola)
    bubble_df <-
      data.frame(
        'title' = character(),
        'weights' = integer(),
        'quantity' = integer(),
        'units' = character(),
        'ingredients' = character()
      )
    for (i in recipe_df$df$Recipes) {
      row_data <- bubble_data %>% filter(title == c(i))
      bubble_df <- rbind(bubble_df, row_data)
    }

    output$bubble_chart <- plotly::renderPlotly({
      #colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
      colors <- c('#FF5733', '#7AFF33', '#33FFF0', '#333CFF', '#FF33E9')
      
      fig <-
        plot_ly(
          bubble_df,
          x = ~ quantity+1 ,
          y = ~ weights,
          type = 'scatter',
          mode = 'markers',
          color = ~ title,
          sizes = c(10, 100),
          colors = colors,
          size=~weights
          ,
          marker = list(
            sizemode = 'diameter',
            opacity = 0.5
           # size = ~ sqrt(weights) * 2
          ) ,
          hoverinfo = 'text',
          text = ~ paste(
            'Recipe :',
            title,
            '<br>Ingredient:',
            ingredients,
            '<br> Measure:',
            weights,
            units
          )
        )
      title <-
        list(family = "Lobster",
             color = 'rgb(128,177,221)',
             size = 20)
      fig <-
        fig %>% layout(
          title = 'Recipe Composition',
          showlegend = F,
          font = title,
          margin = list(
            l = 50,
            r = 50,
            b = 100,
            t = 100,
            pad = 4
          ),
          xaxis = list(
            title = 'Quantity',
            type = 'log',
            showgrid = F,
            zeroline = F,
            visible = F
          ),
          yaxis = list(
            title = 'Weights',
            type = 'log',
            showgrid = F,
            zeroline = F,
            visible = F
          ),
          paper_bgcolor = 'rgb(0,0,0,0)',
          plot_bgcolor = 'rgb(0,0,0,0)'
        )
      fig
      
    })
    
    output$pie_chart_choices<-renderUI({
      selectInput("InputRecipe",
                  label = "Nutritions",
                  choices = recipe_df$df$Recipes)
    })
    
   
    observeEvent(input$InputRecipe,{
      pie_data<-pie_chart_table(recipe_data)
      pie_data_output<-pie_data %>%
        filter(title==input$InputRecipe) %>%
        dplyr::select(title,nutrition.name,quantity) %>%
        group_by(nutrition.name) %>%
        summarise(quantity=sum(quantity))
      pie_data_output_1 <-pie_data %>%
        filter(title==input$InputRecipe) %>%
        dplyr::select(title,variable,value) %>%
        group_by(variable) %>%
        summarise(value=sum(value))
      output$pie_chart<-plotly::renderPlotly({
        fig <- plot_ly(pie_data_output, 
                       labels = ~nutrition.name, 
                       values = ~quantity, 
                       textinfo = "none",
                       type = 'pie',
                       domain = list(x = c(0.6, 1), y = c(0.4, 1)))
        fig<-fig %>% 
          add_trace(data=pie_data_output_1,
                    labels = ~variable, 
                    values = ~value,
                    type = 'pie',
                    textinfo = "none",
                    domain = list(x = c(0, 0.4), y = c(0.4, 1)))
        title<- list(color='rgb(128,177,221)',size=20)
        fig <- fig %>%
          layout(plot_bgcolor='rgba(254, 247, 234)',
                 font=title,
                 showlegend=F,
                 #margin = list(l=80, r=50, b=100, t=120, pad=4),
                 paper_bgcolor = 'rgba(0,0,0,0)',
                 plot_bgcolor ='rgba(0,0,0,0)'
                 ) %>%
          add_annotations(y=1.07, x=0.5, text=~paste(""),
          showarrow=F,font=list(size=15))%>%add_annotations(x=0.1,y=0.3, text='Nutrition Per 100 grams', showarrow=F,font=list(size=14))%>%add_annotations (x=0.88,y=0.3,text='Nutrition Per Ingredients', showarrow=F,font=list(size=14))
        fig
      }) 
      
      output$MachineLearningUI <- renderUI({
        mutated_rules <-
          rules_df %>% mutate(to = str_split(LHS, ","))%>% dplyr::select(lift, confidence, to, RHS, count) 
        
        matched_rules <- data.frame(RHS = as.character(), confidence = as.integer(), lift = as.integer())
        for(i in 1:length(mutated_rules$to)){
          if(all(mutated_rules$to[[i]] %in% grocery_data$df$Ingredients)){
            matched_rules <- rbind(matched_rules, mutated_rules[i,c("RHS","confidence","lift")])
          
          }
        }
        matched_rules<-matched_rules%>%dplyr::select(-c(lift))%>%rename('Recommended Ingredients'=RHS)
        box(
          title =" ",
          width = 12,
          renderTable({
            matched_rules[!duplicated(matched_rules$`Recommended Ingredients`),]
            matched_rules%>%dplyr::select(`Recommended Ingredients`)
          })
        )
      })
      
    })
  })
  
  observeEvent(input$InstructionRecipe, {
    instructions <- get_instructions(recipe_df$df)
    output$instructionSteps <- renderUI({
      i <- 1
      steps <- ""
      
      return(
        box(
          title = input$InstructionRecipe,
          width = 12,
          solidHeader = TRUE,
          #gradientColor = "teal",
          status = "primary",
          renderUI(for (instructions in instructions[input$InstructionRecipe]) {
            return(lapply(1:length(instructions), function(i) {
              p(paste0("Step ", i, ": ", instructions[i]))
            }))
          })
        )
      )
      
      
    })
    
    # link <- recipe_data$url[recipe_data$title == input$InstructionURL]
    # 
    # output$url <- renderUI({
    #   return(actionButton("url",
    #              label = "Link to Web Page",
    #              icon = icon("twitter"),
    #              size = "lg",
    #              style = "bordered",
    #              onclick = sprintf("window.open('%s')", link)))})
    #output$url <- renderText({recipe_data$url[recipe_data$title == input$InstructionRecipe]})
  })
  
  output$recipe_df <- DT::renderDataTable({
    recipe_df$df
  }, rownames = FALSE, options = list(pageLength = 7))
  
  output$grocery_data <-
    DT::renderDataTable(deleteButtonColumn(grocery_data$df, 'delete_button'))
  
  observeEvent(input$deletePressed, {
    rowNum <- parseDeleteEvent(input$deletePressed)
    grocery_data$df <-
      data.frame(grocery_data$df, stringsAsFactors = F)
    dataRow <- grocery_data$df[rowNum, ]
    values$deletedRows <- rbind(dataRow, values$deletedRows)
    values$deletedRowIndices <-
      append(values$deletedRowIndices, rowNum, after = 0)
    grocery_data$df <- grocery_data$df[-(rowNum), ]
  })
  
  df <-
    data.frame('Nutrition.Name' = character(), 'Value' = integer())
  
  observeEvent(input$Add, {
    values$number <- input$number
    for (i in recipe_df$df$Recipes) {
      de <- nutri_table(recipe_data, i, values$number)
      df <- rbind(df, de)
    }
    
    output$calories <- renderValueBox ({
      df <-
        df %>% group_by(Nutrition.Name) %>% summarize(Value = sum(Value))
      u <- c('cal', 'mgram', 'mgram', 'mgram', 'mgram', 'mgram')
      values$col1 <- data.frame(df, units = u)
      #validate(need(values$col1, ''))
      dat <-
        values$col1 %>% filter(Nutrition.Name == 'Energy') %>% dplyr::select(Value)
      if (nrow(dat) > 0) {
        valueBox(
          paste(dat$Value, 'cal'),
          'Energy',
          icon = icon('fire'),
          color = 'red',
          width = NULL
        )
      }
      
      else{
        valueBox(
          'Add Recipe',
          'Energy',
          icon = icon('fire'),
          color = 'red',
          width = NULL
        )
      }
      
    })
    output$Protein <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Protein') %>% dplyr::select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'mg'),
          'Protein',
          icon = icon('child'),
          color = 'maroon',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Protein',
          icon = icon('child'),
          color = 'maroon',
          width = NULL
        )
      }
    })
    output$Fat <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Fat') %>% dplyr::select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'mg'),
          'Fat',
          icon = icon('bacon'),
          color = 'yellow',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Fat',
          icon = icon('bacon'),
          color = 'yellow',
          width = NULL
        )
      }
    })
    output$Sodium <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Sodium') %>% dplyr::select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'mg'),
          'Sodium',
          icon = icon('mortar-pestle'),
          color = 'blue',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Sodium',
          icon = icon('baby'),
          color = 'blue',
          width = NULL
        )
      }
    })
    output$Saturated_Fat <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Saturated fat') %>% dplyr::select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'mg'),
          'Saturated Fat',
          icon = icon('beer'),
          color = 'orange',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Saturated Fat',
          icon = icon('pizza-slice'),
          color = 'orange',
          width = NULL
        )
      }
    })
    output$Sugar <- renderValueBox({
      #validate(need(values$col1, ''))
      df1 <-
        values$col1 %>% filter(Nutrition.Name == 'Sugar') %>% dplyr::select(Value)
      if (nrow(df1) > 0) {
        valueBox(
          paste(df1$Value, 'mg'),
          'Sugar',
          icon = icon('stroopwafel'),
          color = 'lime',
          width = NULL
        )
      }
      else{
        valueBox(
          'Add Recipe',
          'Sugar',
          icon = icon('cookie'),
          color = 'lime',
          width = NULL
        )
      }
    })
  })
  }
