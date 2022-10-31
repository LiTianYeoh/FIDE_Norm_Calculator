library(rvest)
library(dplyr)

player_url<- c(
  'http://chess-results.com/tnr672705.aspx?lan=1&art=9&fed=MAS&flag=30&snr=24',              ##normal 9 rounds
  'https://chess-results.com/tnr576097.aspx?lan=1&art=9&fed=MAS&turdet=YES&flag=30&snr=8',   ## with FM
  'http://chess-results.com/tnr677430.aspx?lan=1&art=9&fed=SRI&turdet=YES&flag=30&snr=20',   ## halfway
  'http://chess-results.com/tnr677435.aspx?lan=1&art=9&fed=IND&turdet=YES&flag=30&snr=26',   ## halfway, women, tpr
  'https://chess-results.com/tnr493357.aspx?lan=1&art=9&fed=MAS&turdet=YES&flag=30&snr=1',   ## bye
  'http://chess-results.com/tnr677435.aspx?lan=1&art=9&fed=MGL&turdet=YES&flag=30&snr=2',    ## halfway, women
  'http://chess-results.com/tnr689902.aspx?lan=1&art=9&snr=10'                               ## weird res
)

player_html<- read_html(player_url[7])

#print(player_html)

main_tables<- html_nodes(player_html, ".CRs1") 

player_info<- data.frame(html_table(main_tables[[1]], header=FALSE), row.names = 1) %>% setNames('Value')
res_table<- data.frame(html_table(main_tables[[2]], header=TRUE)) %>% filter(!is.na(`Rd.`))

print(player_info)
print(res_table)

title_col<- names(res_table)[which(names(res_table)=='Name')-1][1]

req_info<- res_table %>% select(c('Rd.', title_col, 'Name', 'Rtg', 'Res.')) %>% 
  setNames(c('rd', 'title', 'name', 'elo', 'res'))

req_info$res<- req_info$res %>% replace(req_info$res == '½', values = '0.5') %>% 
  replace(req_info$res == '', values = '0.5')

req_info$elo<- as.numeric(req_info$elo)

print(req_info)
str(req_info)
