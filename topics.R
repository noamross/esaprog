# Load/install necessary packages
library(devtools)
#install_github("kshirley/LDAviz")
#install_github("cpsievert/LDAvis")
library(LDAtools)
library(LDAvis)
library(topicmodels)
library(tm)
library(Rmpfr)
library(plyr)
library(XML)
library(stringi)
library(shiny)
#library(multicore)
#library(doMC)
#registerDoMC(cores=22)

ppath = "eco.confex.com/eco/2014/webprogram"
paper_files = list.files(ppath, recursive=TRUE, pattern="Paper\\d+\\.html")

abs = alply(paper_files, 1, function(paper) {
  paper_xml = htmlTreeParse(file.path(ppath, paper), useInternalNodes = TRUE, trim=TRUE)
  ab = try(xmlValue(paper_xml[['//div[@class="abstract"]']]), silent = TRUE)
  if(class(ab) != "try-error") {
    ab = stringi::stri_replace_all_fixed(ab, "Background/Question/Methods", "")
    ab = stringi::stri_replace_all_fixed(ab, "Results/Conclusions", "")
    return(ab)
  } else {
    return(NULL)
  }
}, .progress = "time")
abs = unlist(compact(abs))

#Preprocess the text and convert to document-term matrix
dtm.control <- list(
  tolower = TRUE,
  removePunctuation = TRUE,
  removeNumbers = TRUE,
  stopwords = stopwords("english"),
  stemming = TRUE,
  wordLengths = c(3, Inf),
  weighting = weightTf
)
corp <- Corpus(VectorSource(unlist(abs, use.names=FALSE)))
dtm <- DocumentTermMatrix(corp, control = dtm.control)
dim(dtm)
dtm <- removeSparseTerms(dtm, 0.99)
dim(dtm)

# Drop documents with little or no text
dtm <- dtm[rowSums(as.matrix(dtm)) > 0, ]

# Fit models and find an optimal number of topics as suggested by Ben Marmick --
# http://stackoverflow.com/questions/21355156/topic-models-cross-validation-with-loglikelihood-or-perplexity/21394092#21394092
harmonicMean <- function(logLikelihoods, precision = 2000L) {
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}
burnin <- 1000
iter <- 1000
keep <- 50
ks <- seq(2, 250, by = 1)
#models <- lapply(ks, function(k) LDA(dtm, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)))
#saveRDS(models, 'topicmodels250.RDS')
models = readRDS('topicmodels250.RDS')
logLiks <- lapply(models, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm <- sapply(logLiks, function(h) harmonicMean(h))
k = sapply(models, function(L) sum(length(L@beta) + length(L@gamma)))
AICs = -2*hm + 2*k
# Find optimal model
library(ggplot2)
library(noamtools)
ldaplot <- ggplot(data.frame(hm, AICs), aes(x=ks, y=-AICs)) + geom_path(lwd=1.5) + theme_nr +
  theme(text = element_text(family='Lato'),
        axis.title.y=element_text(vjust=1, size=16),
        axis.title.x=element_text(vjust=-.5, size=16),
        axis.text=element_text(size=16),
        plot.title=element_text(size=20)) +
  xlab('Number of Topics') +
  ylab('Relative Parsimony of Model (negative AIC)') +
  ggtitle(expression(atop("Latent Dirichlet Allocation Analysis of ESA Program", atop(italic("How many distinct topics in the abstracts?"), ""))))
ldaplot
ggsave('LDA_AIC.png', ldaplot, width=10, height=7)
# Tried to model the no. of distinct topics in the #ESA2014 program. We talk about LOTS of different stuff.
opt <- models[which.min(AICs)][[1]]
top.opt = ks[which.min(AICs)]

# Extract the 'guts' of the optimal model
doc.id <- opt@wordassignments$i
token.id <- opt@wordassignments$j
topic.id <- opt@wordassignments$v
vocab <- opt@terms

# Get the phi matrix using LDAviz
dat <- getProbs(token.id, doc.id, topic.id, vocab, K = max(topic.id), sort.topics = "byTerms")
phi <- t(dat$phi.hat)
# NOTE TO SELF: these things have to be numeric vectors or else runVis() will break...add a check in check.inputs
token.frequency <- as.numeric(table(token.id))
topic.id <- dat$topic.id
topic.proportion <- as.numeric(table(topic.id)/length(topic.id))

# Run the visualization locally using LDAvis
z <- check.inputs(K=max(topic.id), W=max(token.id), phi, token.frequency, vocab, topic.proportion)
json <- with(z, createJSON(K=max(topic.id), phi, token.frequency, 
                   vocab, topic.proportion))



#runShiny(phi, token.frequency, vocab, topic.proportion)
serVis(json, out.dir="esa_lda", open.browser = FALSE)
