# The following code produces a temporal directed edgelist of ESA
# collaborations from the 2010-2014 abstracts (from=coauthor, to=presenter).
#
# It assumes that you've downloaded the ESA program to a
# subfolder (eco.confex.com) using the following shell commands:
#
#```
# wget -r http://eco.confex.com/eco/2010/webprogram/
# wget -r http://eco.confex.com/eco/2011/webprogram/
# wget -r http://eco.confex.com/eco/2012/webprogram/
# wget -r http://eco.confex.com/eco/2013/webprogram/
# wget -r http://eco.confex.com/eco/2014/webprogram/
#```

library(plyr)
library(XML)
library(stringi)
library(rlist)
library(pipeR)
library(data.table)
library(magrittr)
library(dplyr)
ppaths = paste0("eco.confex.com/eco/", 2010:2014, "/webprogram")
paper_files = list.files(ppaths, recursive=TRUE, pattern="Paper\\d+\\.html",
                         full.names=TRUE)
names(paper_files) = stri_replace_first_fixed(basename(paper_files), ".html", "")

# Make a list of all abstracts.  Elements of each are
# -   Title
# -   Year
# -   Presenting Author
# -   All other authors

process_abstract = function(paper) {
 paper_xml = htmlTreeParse(paper, useInternalNodes = TRUE, trim=TRUE)
 title = xmlValue(paper_xml[['//div[@class="subtitle"]/div[@class="subtext"]']])
 if(is.na(title)) title = stri_trim_both(xmlValue(paper_xml[['//h2[@class="subtitle"]/text()[last()]']]))
 year = as.integer(stri_match_first_regex(xmlValue(paper_xml[['//div[@class="datetime"]']]), "\\d{4}"))
 presenter = stri_trim_both(xmlValue(paper_xml[['//div[@class="paperauthors"]/div[@class="presenter"]/span[@class="name"]']]))
 coauthors = xpathSApply(paper_xml, '//div[@class="paperauthors"]/div[@class="author"]/span[@class="name"]', xmlValue)
 if(!is.null(coauthors)) coauthors = stri_trim_both(coauthors)
 return(list(title=title, year=year, presenter=presenter, coauthors=coauthors))
}

abstracts = alply(paper_files, 1, function(paper) {
  abstract = try(process_abstract(paper), silent=TRUE)
  return(abstract)
}, .progress = ifelse(interactive(), "time", "none"), .dims=TRUE)


#errors = list.filter(abstracts, class(.) == "try-error")
# Remove errors. These are mostly 404 errors due to broken links within the
# ESA program that were followed by wget
abstracts = list.filter(abstracts, class(.) != "try-error")


# Some unused code for name disambiguation
# all_names = sort(unique(list.mapv(abstracts, stri_trim_both(c(presenter, coauthors)))))
# split_names = stri_split_regex(all_names, "\\s+")
# split_names = list.map(split_names, .[!(. %in% c("III", "II", "Jr", "Jr."))])
# last_names = list.mapv(split_names, tail(., 1))
# first_names = list.mapv(split_names, head(., 1))
# first_initials = stri_sub(first_names, 1,1)
# first_last = paste(first_names, last_names)[order(last_names, first_names)]
# all_names = all_names[order(last_names, first_names)]
# repeats = all_names[duplicated(first_last) | duplicated(first_last, fromLast = TRUE)]

author_temporal_edgelist = abstracts %>>%
  list.filter(!is.null(coauthors)) %>>%
  list.update(id     =    rep(.name, length(coauthors)),
              presenter = rep(presenter, length(coauthors)),
              year      = rep(year, length(coauthors)),
              title     = NULL) %>>%
  list.stack %>>%
  select(2,3,1,4)

names(author_temporal_edgelist)[2] = "coauthor"
save(abstracts, author_temporal_edgelist, file="author_network_data.Rdata")

