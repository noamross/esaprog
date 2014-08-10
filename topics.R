# Load/install necessary packages
library(devtools)
#install_github("ropensci/elife")
#install_github("kshirley/LDAviz")
#install_github("cpsievert/LDAvis")
library(LDAviz)
library(LDAvis)
library(topicmodels)
library(tm)
library(Rmpfr)


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
corp <- Corpus(VectorSource(unlist(abstracts, use.names=FALSE)))
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
ks <- seq(20, 40, by = 1)
models <- lapply(ks, function(k) LDA(dtm, k, method = "Gibbs", control = list(burnin = burnin, iter = iter, keep = keep)))
logLiks <- lapply(models, function(L)  L@logLiks[-c(1:(burnin/keep))])
hm <- sapply(logLiks, function(h) harmonicMean(h))

# Find optimal model
plot(ks, hm, type = "l")
opt <- models[which.max(hm)][[1]]

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
runVis()














###############################################################
############ 'Verify' topics
###############################################################

#str(max.col(dat$theta.hat)) # This is equivalent to dat$main.topic (except the latter doesn't include prior weights) shouldn't this use reodered topics?
theta <- dat$theta.hat[, dat$topic.order]
colnames(theta) <- paste(1:dim(theta)[2])
maxes <- apply(theta, 1, max)
o <- order(maxes, decreasing = TRUE)
maxes[o][1:5] # Top 5 'most distinguished' documents (e.g., most mass on a single topic) 
w.max <- apply(theta, 1, which.max)
w.max[o][1:5] # The topic responsible for that mass

# Check that order of abstracts and subject areas match
stopifnot(all(names(abs) == names(areas)))
areas[o][1:5] # Subject areas for top 5
abs[o][1:5]
