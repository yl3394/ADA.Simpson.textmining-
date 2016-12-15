library(data.table)
library(plotly)
# read original data
simpsonstotal.variable <- fread("/Users/sun93/Documents/ADA/project/simpsons_episodes_final.csv", header = TRUE)
colnames(simpsonstotal.variable)
simpsonstotal.variable$year <- as.factor(simpsonstotal.variable$year)

simpsonstotal.variable$season <- as.factor(simpsonstotal.variable$season)
levels(simpsonstotal.variable$season) <- c("season1","season2","season3","season4","season5","season6","season7","season8","season9","season10",
                                           "season11","season12","season13","season14","season15","season16","season17","season18","season19","season20",
                                           "season21","season22","season23","season24","season25","season26","season27","season28")

simpsonstotal.variable$original_air_date <- as.factor( simpsonstotal.variable$original_air_date)
simpsonstotal.variable$original_air_date <- as.Date( simpsonstotal.variable$original_air_date, "%m/%d/%y")
str(simpsonstotal.variable)
View(simpsonstotal.variable)

g <- ggplot(simpsonstotal.variable, aes(original_air_date,imdb_rating,colour = year)) +
  geom_point()
simpsonstotal.variable <- as.data.frame(simpsonstotal.variable)
#############################
#IMBD rating with Date
x <- list(
  title = "Date"
)
y <- list(
  title = "IMBD Rating"
)

rating <- plot_ly(simpsonstotal.variable, x = ~simpsonstotal.variable$original_air_date, y = ~simpsonstotal.variable$imdb_rating, 
        color = ~simpsonstotal.variable$season)%>%
  layout(title = "IMBD rating with Date",xaxis = x, yaxis = y)

setwd('/Users/sun93/Documents/ADA/project')
getwd()
htmlwidgets::saveWidget(as.widget(rating), "IMBD_rating_with_Date.html")


############################
#US viewers with Date
x <- list(
  title = "Date"
)
y <- list(
  title = "US viewers(in millions)"
)
views <- plot_ly(simpsonstotal.variable, x = ~simpsonstotal.variable$original_air_date, 
                 y = ~simpsonstotal.variable$us_viewers_in_millions, 
        color = ~simpsonstotal.variable$season)%>%
  layout(title = "US viewers(in millions) with Date",xaxis = x, yaxis = y)


setwd('/Users/sun93/Documents/ADA/project')
getwd()
htmlwidgets::saveWidget(as.widget(views), "US_viewers_with_Date.html")

