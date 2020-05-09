recipe_csv <- read.csv("data/final_data.csv")
bubble_data <- read.csv('data/Bubble.csv')
recipe_data <- recipe_csv[c(3, 5, 6, 7, 9, 10, 13, 14, 16, 18, 20)]


colnames(recipe_data) <-
  c(
    'url',
    'title',
    'id',
    'instructions',
    'ingredient_list',
    'fsa_lights_per_100gm',
    'nutr_per_ingr',
    'nutr_per_100gm',
    'quantity',
    'units',
    'weight_per_ingr'
  )

clean_data <- function(col_name){
  pattern <- c("\\[\\{'text': ", "\\{'text'", ": ", "\\}", "\\]", "'", 
               "\\[", "\\]", '\\}', "\\[\\{", '\\{', '\\]')
  
  for(i in 1:length(pattern)){
    col_name <- str_remove_all(col_name, pattern[i])
  }
  return (noquote(col_name))
}

clean_data_nutrients <- function(col_name){
  pattern <- c('\\[\\{', '\\}', '\\{', '\\]')
  
  for(i in 1:length(pattern)){
    col_name <- str_remove_all(col_name, pattern[i])
  }
  return (noquote(col_name))
}

recipe_data$ingredient_list <- clean_data(recipe_data$ingredient_list)
recipe_data$quantity <- clean_data(recipe_data$quantity)
recipe_data$weight_per_ingr <- clean_data(recipe_data$weight_per_ingr)
recipe_data$nutr_per_ingr <- clean_data_nutrients(recipe_data$nutr_per_ingr)
recipe_data$nutr_per_100gm <- clean_data_nutrients(recipe_data$nutr_per_100gm)

nutritiontable<-data.frame('fat'=integer(),'nrg'=integer(),'pro'=integer(),'sat'=integer(),'sod'=integer(),'sug'=integer(),'fat'=integer())

gmtable<-data.frame('energy'=integer(),'fat'=integer(),'proteins'=integer(),'salt'=integer(),'saturates'=integer(),'sugars'=integer())

for (i in (1:length(recipe_data$nutr_per_ingr)))
{
  new <- strsplit(recipe_data$nutr_per_ingr[i], ',')
  new1 <- strsplit(recipe_data$nutr_per_100g[i], ',')
  new <- new[[1]]
  new1 <- new1[[1]]
  new <- noquote(new)
  new1 <- noquote(new1)
  df <- data.frame(new)
  df1 <- data.frame(new1)
  df <- df %>% separate(new, c('content', 'value'), sep = ':')
  df1 <- df1 %>% separate(new1, c('content', 'value'), sep = ':')
  df[, 1] <- (gsub("'", '', df[, 1]))
  df1[, 1] <- (gsub("'", '', df1[, 1]))
  df[, 2] <- as.numeric(df[, 2])
  df1[, 2] <- as.numeric(df1[, 2])
  df <- df %>% group_by(content) %>% summarize(value = sum(value))
  df1 <- df1 %>% group_by(content) %>% summarize(value = sum(value))
  df <- pivot_wider(df, names_from = content, values_from = value)
  df1 <- pivot_wider(df1, names_from = content, values_from = value)
  df <- df %>% rename('fat.1' = 'fat')
  nutritiontable[nrow(nutritiontable) + 1, 1:7] <- df
  gmtable[nrow(gmtable)+1, 1:6] <- df1
}

options(scipen=999)
nutritiontable<-round(nutritiontable,2)
rownames(nutritiontable)<-NULL
recipe_data<-cbind(recipe_data,nutritiontable)
recipe_data<-recipe_data%>%rename('Fat'='fat','Energy'='nrg','Protein'='pro','Saturated fat'='sat','Sodium'='sod','Sugar'='sug')
gmtable<-round(gmtable,2)
rownames(gmtable)<-NULL
recipe_data<-cbind(recipe_data,gmtable)
recipe_data<- recipe_data %>% rename('Energy_100'=energy,'Fat_100'=fat,'Proteins_100'=proteins,'Salt_100'=salt,'Saturated fat_100'=saturates,'Sugar_100'=sugars)
recipe_data




