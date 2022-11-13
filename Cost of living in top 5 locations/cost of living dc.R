library(rvest)

webpage= read_html("https://livingcost.org/cost/united-states/dc")

numbers = webpage %>% html_nodes("#salary+ td .text-nowrap , #transport+ td .text-center , #food+ td .text-center , #rent+ td .text-center , #without-rent+ td .text-center , #total+ td .text-center") %>% html_text()
rent = webpage %>% html_nodes("#transport , #food , #rent , #without-rent , #salary , #total") %>% html_text()


costofliving = data.frame(rent, numbers, stringsAsFactors = FALSE)
