library(rvest)
install.packages("Rserve")
library(Rserve)

webpage= read_html("https://www.iihs.org/topics/fatality-statistics/detail/state-by-state#rural-versus-urban")


California = webpage%>% html_nodes("#rural-versus-urban .sticky-thead tr~ tr+ tr th , #rural-versus-urban .sticky-thead .first-column") %>% html_text()
Dc = webpage%>% html_nodes("#rural-versus-urban .even:nth-child(8) td")  %>% html_text()
TX = webpage%>% html_nodes("#rural-versus-urban .even:nth-child(44) td")  %>% html_text()
NY = webpage%>% html_nodes("#rural-versus-urban .odd:nth-child(33) td")  %>% html_text()
WA = webpage%>% html_nodes("#rural-versus-urban .even:nth-child(48) td")  %>% html_text()

