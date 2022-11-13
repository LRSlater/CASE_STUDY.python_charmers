library(rvest)

webpage= read_html("https://livingcost.org/cost/united-states/ca")

numbers = webpage %>% html_nodes("#without-rent+ td .text-center , #rent+ td .text-center , #food+ td .text-center , #transport+ td .text-center , .text-center.text-nowrap , #total+ td .text-center") %>% html_text()
rent =  webpage %>% html_nodes("#without-rent , #rent , #food , #transport , #salary , #total") %>% html_text()

costoflivingca = data.frame(rent, numbers, stringsAsFactors = FALSE)
