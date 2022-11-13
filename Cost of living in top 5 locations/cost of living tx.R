library(rvest)

webpage = read_html("https://livingcost.org/cost/united-states/tx")

numbers = webpage %>% html_nodes("#food+ td .text-center , #without-rent+ td .text-center , #rent+ td .text-center , #transport+ td , .text-center.text-nowrap , #total+ td .text-center") %>% html_text()
rent = webpage %>% html_nodes("#salary , #transport , #food , #rent , #without-rent , #total") %>% html_text()

costoflivingtx = data.frame(rent, numbers, stringsAsFactors = FALSE)
