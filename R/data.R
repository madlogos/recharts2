#' Province-specific GDP 2012-2014 in China
#'
#' A data.frame comprising of province-specific GDP (2012-14) in China.
#' @docType data
#' @name ChinaGDP
#' @format A data.frame (93 * 3) \cr
#' \tabular{lll}{
#'  Var \tab Type \tab Meaning \cr
#'  Year \tab num \tab Years (2012, 2013, 2014, ...) \cr
#'  Prov \tab chr \tab Province names (Guangdong, Beijing, Shanghai, ...) \cr
#'  GDP \tab num \tab GDP in RMB (1103605, 1059587, 967419, ...)
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_31_Map.html}
#' @examples data(ChinaGDP)
#' str(ChinaGDP)
NULL

#' PM2.5 in China Cities (Fictious Data)
#'
#' A data.frame comprising of PM2.5 data in China cities.
#' @docType data
#' @name chinapm25
#' @format A data.frame (199 * 4) \cr
#' \tabular{llll}{
#'  Var \tab Type \tab Meaning \cr
#'  City \tab chr \tab City names (Haimen, Ordos, Zhaoyuan, ...) \cr
#'  Value \tab num \tab PM2.5 values (9, 12, 12, ...) \cr
#'  Lng \tab num \tab Longitude (121, 110, 120, 122, ...) \cr
#'  Lat \tab num \tab Latitude (31.9, 39.6, 37.4, 30, ...)
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_31_Map.html}
#' @examples data(chinapm25)
#' str(chinapm25)
NULL

#' Deutsch Soccer Team And Clubs
#'
#' A data.frame comprising of Deutsch soccer team players and their clubs. It contains
#' two Deutsch team (\code{year} == 2014 and \code{year} == 2016)
#' @docType data
#' @name deutsch
#' @format A data.frame (26 * 5) \cr
#' \tabular{lllll}{
#'  Var \tab Type \tab Meaning \cr
#'  player \tab chr \tab Player names \cr
#'  club \tab chr \tab Club of the players \cr
#'  weight \tab num \tab The weight of the connection between clubs and players \cr
#'  role \tab chr \tab Role of the players ('Fw', 'Mf', 'Gk', 'Df', ...) \cr
#'  year \tab int \tab Year tag of the Deutsch soccer team
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_12_Chord.html}
#' @examples data(deutsch)
#' str(deutsch)
NULL

#' Events Data for eventRiver
#'
#' A data.frame comprising of fictious events.
#' @docType data
#' @name events
#' @format A data.frame (30 * 5) \cr
#' \tabular{lllll}{
#'  Var \tab Type \tab Meaning \cr
#'  series \tab chr \tab Type of the events ("financial", "political") \cr
#'  event \tab chr \tab Title of the events ("Alibaba IPO", ...) \cr
#'  weight \tab num \tab The weight of the events \cr
#'  time \tab Date \tab Time of the event slices \cr
#'  value \tab int \tab Impact value of the event slices
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_05_eventRiver.html}
#' @examples data(events)
#' str(events)
NULL

#' Fictious Flights in China (Beijing, Shanghai, Guangzhou)
#'
#' A list comprising of two data.frame: route among cities and coordinates of the cities.
#' @docType data
#' @name flight
#' @format A data.frame (150 * 2) route \cr
#' \tabular{ll}{
#'  Var \tab Type \tab Meaning \cr
#'  From \tab chr \tab Source of the route (Beijing, Shanghai, ...) \cr
#'  To \tab chr \tab Target of the route (Baotou, Beihai, ...)
#' }
#' A data.frame (114 * 3) coord \cr
#' \tabular{lll}{
#'  Var \tab Type \tab Meaning \cr
#'  Place \tab chr \tab The name of the cities (also in route$From and route$To) \cr
#'  Lng \tab num \tab Longitude of the places \cr
#'  Lat \tab num \tab Latitude of the places
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_31_Map.html}
#' @examples data(flight)
#' str(flight)
NULL

#' Chinese and English Names of Geographic Names
#'
#' A data.frame acting as a dictionary of Chinese-English-names of a bunch of geograhic names.
#' @docType data
#' @name geoNameMap
#' @format A data.frame (3735 * 5) \cr
#' \tabular{lllll}{
#'  Var \tab Type \tab Meaning \cr
#'  ID \tab int \tab ID of the record \cr
#'  EN \tab chr \tab English geographic names \cr
#'  CN \tab chr \tab Chinese translation of the geographic names \cr
#'  FKEY \tab int \tab ID of the parent node of this place \cr
#'  LEVEL \tab int \tab 0: country; 1: province; 2: city; 3: county
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_31_Map.html}
#' @examples data(geoNameMap)
#' str(geoNameMap)
NULL

#' For- and Against- Relationship in Mid-east (2013)
#'
#' A matrix of the relationship (for- or against-) of the political powers involved
#' in Mid-east. You have to re-organize it before processing.
#' @docType data
#' @name mideast
#' @format A matrix (16 * 16) \cr
#' Mutual attitude of the political powers (for or against/weight)
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_12_Chord.html}
#' @examples data(mideast)
#' str(mideast)
NULL

#' Stock Index Records of Shanghai Stock Exchange (2013H1)
#'
#' A data.frame of stock index records of SHSE in 2013H1.
#' @docType data
#' @name stock
#' @format A data.frame (88 * 5) \cr
#' \tabular{lllll}{
#'  Var \tab Type \tab Meaning \cr
#'  data \tab Data \tab Transaction date \cr
#'  open \tab num \tab Open index \cr
#'  close \tab num \tab Close index \cr
#'  low \tab numt \tab Low index \cr
#'  high \tab num \tab High index
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_04_K.html}
#' @examples data(stock)
#' str(stock)
NULL

#' Family Network of the Yus of Shaoxing
#'
#' A list comprising of two data.frame: nodes (people related to the family) and
#' links (their relationship). The Yus have contributed significantly to connecting
#' a lot of most important people in modern China history.
#' @docType data
#' @name yuNetwork
#' @format A data.frame (49 * 3) nodes \cr
#' \tabular{lll}{
#'  Var \tab Type \tab Meaning \cr
#'  name \tab chr \tab The name of the family member/relative \cr
#'  series \tab chr \tab Type of the person ('root', 'node 1', ...) \cr
#'  value \tab num \tab Importance value of the person
#' }
#' A data.frame (49 * 4) links \cr
#' \tabular{llll}{
#'  Var \tab Type \tab Meaning \cr
#'  source \tab chr \tab Starting end of the link between two people \cr
#'  target \tab chr \tab Ending end of the link between two people \cr
#'  relation \tab chr \tab Relationship (father-son, spouse, ...) \cr
#'  weight \tab num \tab Importance of the relationship
#' }
#' @references \url{https://madlogos.github.io/recharts/Basic_Plots_11_Force.html}
#' @examples data(yuNetwork)
#' str(yuNetwork)
NULL