##Some initial data exploration on author network data

library(ggplot2)
library(plyr)
library(XML)
library(stringi)
library(rlist)
library(pipeR)
library(data.table)
library(magrittr)
library(dplyr)

load("author_network_data.Rdata")

no_authors = abstracts %>>%
  list.map(list(year=year, authors = 1 + ifelse(is.null(coauthors), 0, length(coauthors)))) %>>%
  list.stack %>>%
  group_by(year, authors) %>>%
  summarize(count=length(authors))

no_auth_sum = no_authors %>>%
  group_by(year) %>>%
  summarize(pct_single = count[which(authors==1)]/sum(count),
            mean       = sum(authors*count)/sum(count))
  
ggplot(no_authors, aes(x=authors, y=count)) +
  geom_bar(stat="identity") +
  facet_wrap(~year) +
  xlim(0,20)

ggplot(no_auth_sum, aes(x=year, y=pct_single)) +
  geom_bar(stat="identity")

single_authors = list.filter(abstracts, is.null(coauthors))

single_author_count = single_authors %>>%
  list.mapv(presenter) %>>%
  table %>>%
  sort(decreasing=TRUE)

top_single_authors = head(single_author_count, 20)
single_author_dist = table(single_author_count)

unsung_authors = author_temporal_edgelist[!(author_temporal_edgelist$coauthors %in% unique(author_temporal_edgelist$presenter)),] %>>%
  dlply(.(year), function(x) head(sort(table(x$coauthors), decreasing=TRUE), 20))


top_authors = abstracts %>>%
  list.map(list(year=year, authors = c(presenter, coauthors))) %>>%
  list.group(year) %>>%
  llply(function(x) list.mapv(x, authors)) %>>%
  list.map(head(sort(table(.), decreasing=TRUE), 20))

single_auths = list.mapv(single_authors, presenter)
solos = sort(table(single_auths[!(single_auths %in% unique(author_temporal_edgelist$coauthors))]), decreasing=TRUE)