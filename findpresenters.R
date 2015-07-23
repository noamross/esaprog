require(XML)
require(stringi)
require(plyr)
require(pander)

ppath = file.path("eco.confex.com", "eco", "2015", "webprogram")
paper_files = list.files(ppath, recursive=TRUE, pattern="Paper\\d+\\.html")



davisites = adply(paper_files, 1, function(paper) {
  paper_xml = htmlTreeParse(file.path(ppath, paper), useInternalNodes = TRUE, trim=TRUE)
  aff = try(paper_xml['//div[@class="paperauthors"]/div[@class="presenter"]'])
  if(class(try)=="try-error") return(NULL)
  if(length(aff) == 0) return(NULL)
  affiliation = xmlValue(aff[[1]])
  is_davis = stri_detect_regex(affiliation, "(?:university|uc).*davis", stri_opts_regex(case_insensitive=TRUE))
  #presenter = stri_trim_both(xmlValue(paper_xml['//div[@class="paperauthors"]/div[@class="presenter"]/span[@class="name"]'][[1]]))
  if(!is_davis) {
    return(NULL)
  } else {
    record = try(data.frame(
    presenter = stri_trim_both(xmlValue(paper_xml['//div[@class="paperauthors"]/div[@class="presenter"]/span[@class="name"]'][[1]])),
    affiliation = affiliation,
    email = "",
    title = xmlValue(paper_xml['//div[@class="subtitle"]/div[@class="subtext"]'][[1]]),
    session = xmlValue(paper_xml['//div[@class="subtitle"]/span[@class="number"]'][[1]]),
    datetime = strptime(xmlValue(paper_xml['//div[@class="datetime"]'][[1]]), format = "%A, %B %d, %Y: %I:%M %p"),
    location = xmlValue(paper_xml['//div[@class="location"]'][[1]]),
    url = paste0('http://eco.confex.com/eco/2015/webprogram/', paper)))
  }
  email = try(stri_match_first_regex(xmlGetAttr(paper_xml['//div[@class="paperauthors"]/div[@class="presenter"]/span[@class="name"]/a'][[1]], "href"), "(?<=mailto:)[^\\s]+$")[[1]], silent = TRUE)
  if(class(email) != "try-error") record$email = email
  if(class(record)=="try-error") return(NULL)
  return(record)
}, .progress = "time")

names = stri_split_fixed(as.character(davisites$presenter), " ")
names = ldply(names, function(x) {
  if(length(x) == 2) x = c(x[1], "", x[2])
  if(length(x) == 4) x = c(x[1], paste(x[2], x[3]), x[4])
  return(x)
})
names(names) = c("first", "middle", "last")

davisites = cbind(names, davisites)
rm(names)
davisites$last = stri_replace_all_fixed(davisites$last, ",", "")
davisites$presenter = stri_replace_all_fixed(davisites$presenter, ",", "")
davisites = davisites[davisites$last != "Larsen", ]


#devtools::install_github("cpsievert/rdom")
tbl = rdom::rdom("http://ecology.ucdavis.edu/people/Students.aspx")
library(dplyr)
library(tidyr)
ggestudents = readHTMLTable(tbl)[[1]]
ggestudents = ggestudents[-1,]
names(ggestudents) = c("name", "email", "aoe")
ggestudents$last_name = stri_extract_first_regex(ggestudents$name, "(?<=\\s)[^\\s]+$")
ggestudents$last_name[ggestudents$last_name == "Wyk"] = "VanWyk"
ggestudents$last_name[ggestudents$last_name == "Cookingham"] = "Winbourne"
ggestudents = rbind(ggestudents, data.frame(name="K. Ash Zemenick", email="", aoe="", last_name="Zemenick"))

cat(ggestudents[ggestudents$last_name %in% davisites$last,]$email, sep=", ")
davisites$presenter[order(davisites$last)]

davistalks = davisites[davisites$last %in% ggestudents$last, c("presenter", "title", "url", "last")]
davistalks = subset(davistalks, !(presenter %in% c("Louie H. Yang", "Truman P. Young")))
davistalks = davistalks[order(davistalks$last),]
printtable = cbind(as.character(davistalks$presenter), paste0("[", davistalks$title, "](", davistalks$url, ")"))
colnames(printtable) = c("Name", "Title/Link")
a = pandoc.table.return(printtable, split.cells=Inf, split.tables=Inf, style="rmarkdown", justify="left")
cat(a, file="ggetalks.md")

otherdav = davisites[!(davisites$url %in% davistalks$url), c("presenter", "title", "url", "last")]
otherdav = otherdav[order(otherdav$last),]
othertable = cbind(as.character(otherdav$presenter), paste0("[", otherdav$title, "](", otherdav$url, ")"))
b = pandoc.table.return(othertable, split.cells=Inf, split.tables=Inf, style="rmarkdown", justify="left")
cat(b, file="othertalks.md")

# 
# popbio = readHTMLTable('http://www-eve.ucdavis.edu/eve/pbg/People_Students.html')[[1]]
# pbn = ldply(stri_split_fixed(popbio$Name, ", "))
# names(pbn)=c("last", "first")
# popbio = cbind(popbio, pbn)
# cat(paste0(popbio[popbio$last %in% davisites$last,]$`Contact Information`, '@ucdavis.edu'), sep=", ")
# 

# 
# ppath = file.path("eco.confex.com", "eco", "2014", "webprogram")
# session_files = list.files(ppath, recursive=TRUE, pattern="Session\\d+\\.html")
# 
# 
# davis_ses = adply(session_files, 1, function(session) {
#   session_xml = htmlTreeParse(file.path(ppath, session), useInternalNodes = TRUE, trim=TRUE)
#   orgs = try(xpathApply(session_xml, '//div[@class="persongroup"]/div[@class="people"]', xmlValue))
#   if(class(try)=="try-error") return(NULL)
#   if(length(orgs) == 0) return(NULL)
#   orgs = lapply(orgs, function(x) {
#     x = stri_trim_both(x)
#     x = stri_replace_all_regex(x, "\\s+", " ")
#     x = stri_split_regex(x, "(?:\\s+and\\s+|\\s*,\\s+)")
#     x = sapply(x, stri_trim_both)
#     return(x)
#   })
#   orgs = unique(unlist(orgs))
#   if(any(orgs %in% davisites$presenter) | ("Hugh Safford" %in% orgs)) {
#     data.frame(orgs = paste(orgs, collapse=", "),
#                title = xmlValue(session_xml[['//div[@class="subtitle"]/div[@class="subtext"]']]),
#                url = paste0('http://eco.confex.com/eco/2014/webprogram/', session),
#                type = xmlValue(session_xml[['//div[@class="parents"]/a']]))
#   } else {
#     return(NULL)
#   }
#   }, .progress = "time")
# 
# davis_ses = Filter(Negate(is.null), davis_ses) 
# davis_ses$type = stri_replace_first_regex(as.character(davis_ses$type), "ia$", "ium")
# davis_ses$type = stri_replace_first_regex(as.character(davis_ses$type), "s$", "")
# sestab = cbind(as.character(davis_ses$orgs), paste0("[", davis_ses$title, "](", davis_ses$url, ")"), as.character(davis_ses$type))
# colnames(sestab) = c("Organizers", "Session Title/Link", "Type")
# 
# c = pandoc.table.return(sestab, split.cells=Inf, split.tables=Inf, style="rmarkdown", justify="left")
# cat(c, file="davis_sessions.md")
