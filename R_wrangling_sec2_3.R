#### Sec 2.3 : Webscrapping
rm(list=ls())
# import a webpage into R
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab <- tab[[2]]

tab <- tab %>% html_table
class(tab)

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)


#Guac Recipe


#For the guacamole recipe page, we already have done this and determined that we need the following selectors:
  
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

#You can see how complex the selectors are. In any case we are now ready to extract what we want and create a list:
  
guacamole <- list(recipe, prep_time, ingredients)
guacamole

#Since recipe pages from this website follow this general layout, we can use this code to create a function that extracts this information:
  
  get_recipe <- function(url){
    h <- read_html(url)
    recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
    prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
    ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
    return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  } 

#and then use it on any of their webpages:
  
  get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

#  Introduction: Questions 1-3
#  Load the following web page, which contains information about Major League Baseball payrolls, into R: https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm External link
  
  library(rvest)
  url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
  h <- read_html(url)

#    We learned that tables in html are associated with the table node.  Use the html_nodes() function and the table node type to extract the first table. Store it in an object nodes:
    
    nodes <- html_nodes(h, "table")
#  The html_nodes() function returns a list of objects of class xml_node. We can see the content of each one using, for example, the html_text() function. You can see the content for an arbitrarily picked component like this:
    
    html_text(nodes[[8]])
#  If the content of this object is an html table, we can use the html_table() function to convert it to a data frame:
    
    html_table(nodes[[8]])
#  You will analyze the tables from this HTML page over questions 1-3.
  

    
#    Question 1
#    0.0/2.5 points (graded)
#    Many tables on this page are team payroll tables, with columns for rank, team, and one or more money values.
    
#    Convert the first four tables in nodes to data frames and inspect them.
    
#    Which of the first four nodes are tables of team payroll?
#      Check all correct answers. Look at table content, not column names.      
library(tidyverse)
html_text(nodes[[1]])
html_table(nodes[[1]])


html_text(nodes[[2]])
html_table(nodes[[2]])


html_text(nodes[[3]])
html_table(nodes[[3]])


html_text(nodes[[4]])
html_table(nodes[[4]])


#Question 2
#0.0/2.0 points (graded)
#For the last 3 components of nodes, which of the following are true? (Check all correct answers.)
#Check all correct answers.

html_text(nodes[[19]])
html_table(nodes[[19]])

html_text(nodes[[20]])
html_table(nodes[[20]])

html_text(nodes[[21]])
html_table(nodes[[21]])


tab_1<-html_table(nodes[[10]])%>%filter(X1!="No.")%>%select(X2,X3,X4)%>%rename(Team=X2, Payroll=X3, Average=X4)

tab_2<-html_table(nodes[[19]])%>%filter(X1!="Team")%>%rename(Team=X1, Payroll=X2, Average=X3)

combined<-full_join(tab_1,tab_2,by="Team")

combined


#Question 4
#1 point possible (graded)
#Assign tab to be the html nodes of the "table" class.

#How many tables are in this Wikipedia page?


url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h <- read_html(url)
nodes <- html_nodes(h, "table")


html_table(nodes[[1]],fill=TRUE)

html_table(nodes[[2]],fill=TRUE)

html_table(nodes[[3]],fill=TRUE)

html_table(nodes[[4]],fill=TRUE)

tab5<-html_table(nodes[[5]],fill=TRUE)

html_table(nodes[[6]],fill=TRUE)

html_table(nodes[[7]],fill=TRUE)

html_table(nodes[[8]],fill=TRUE)

html_table(nodes[[9]],fill=TRUE)

html_table(nodes[[10]],fill=TRUE)
