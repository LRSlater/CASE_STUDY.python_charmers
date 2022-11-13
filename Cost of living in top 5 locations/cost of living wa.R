library(rvest)

webpage = read_html("https://livingcost.org/cost/united-states/wa")

numbers = webpage %>% html_nodes(".text-center.text-nowrap , #without-rent+ td .text-center , #rent+ td .text-center , #food+ td .text-center , #transport+ td .text-center , #total+ td .text-center") %>% html_text()
rent = webpage %>% html_nodes("#transport , #food , #rent , #without-rent , #salary , #total") %>% html_text()

costoflivingwa = data.frame(rent, numbers, stringsAsFactors = FALSE)
