# Load libraries----
library (tm)
library (topicmodels)
library (slam)
library (wordcloud)
library (dplyr)
library (ggplot2)
library (tibble)
library (gtools)

# Pre-process the data ----
# Define industries/technologies for the companies went public (based on full txt)
all.files <- list.files(path = "ipos_2nd_qtr_2008_2019_nouns_adj", pattern = "*.txt", full.names = T)

# Create a data frame (for full txts)
all.files <- as.data.frame(all.files) 
colnames(all.files) <- "path"
all.files <- mutate(all.files, d = as.vector(path)) 
all.files <- tidyr::separate(all.files, d, c("cik", "year", "month", "day"), "-")
all.files <- all.files[,c("cik","year","month","day","path")]
all.files$cik <- gsub("(.+?)/(.+?)", "\\2", all.files$cik) 
all.files$day <- gsub("(.+?).txt", "\\1", all.files$day)
all.files <- mutate_at(all.files ,c("year", "month", "day"), as.numeric)

# Define the running sample for training
training <- all.files %>% 
  filter(year < 2011) %>% 
  arrange(year, month, day)

sample.files <- as.character(training$path)

# Topic modelling----
# Construct the corpus
sample.cvector <- vector()
for (i in c(1:length(sample.files))){
  x <- read.delim(sample.files[i],
                  encoding="UTF-8",
                  header = T,
                  sep = "\t")
  
  sample.cvector[i] <- paste(x$token,  collapse = " ")
  sample.cvector[i] <- gsub(".+table.contents(.+?)summary(.+?)", "\\1", sample.cvector[i], ignore.case = T)
  }

# Construct DTM based on the training corpus data
dtm.test <- DocumentTermMatrix(Corpus(VectorSource(sample.cvector)),
                            control = list(removePunctuation = T,
                                           stopwords = T,
                                           tolower = T,
                                           removeNumbers = T,
                                           wordLengths=c(4, 20),
                                           bounds = list(global = c(15,100))))

# Clean DTM by the terms with low number of frequences
dtm.training <- dtm.test[row_sums(dtm.test) > 10,]
training.del <- rownames(dtm.test[row_sums(dtm.test) <= 10,])

# Constructing test LDA
topic.test <- LDA(dtm.training, 
             k = 30, 
             method = "Gibbs",
             control = list(
               seed = 1234, 
               burnin = 100,  
               iter = 300,  
               keep = 1,
               save = F,
               verbose = 10))

topic.test@loglikelihood
dim(topic.test@gamma)

# Find the key terms for the topic
beta <- exp(topic.test@beta) # log of probability reported
nrow(beta)

# Define words and its frequency
training.terms <-list()
training.prob <- list()

for (i in c(1:topic.test@k)){
  training.terms[[i]] <- head(topic.test@terms[order(beta[i,], decreasing = T)], 75)
  training.prob[[i]] <- head(sort(beta[i,], decreasing = T), 75)
}

training.terms.df <- data.frame(matrix(unlist(training.terms),
                                 nrow=length(training.terms),
                                 byrow=T))

# Run for the recent years ----
recent.ipo <- all.files %>% 
  filter(year > 2016) %>% 
  arrange(year, month, day)

files <- as.character(recent.ipo$path)

# Construct the corpus
cvector <- vector()
for (i in c(1:length(files))){
  x <- read.delim(files[i],
                  encoding="UTF-8",
                  header = T,
                  sep = "\t")
  
  cvector[i] <- paste(x$token,  collapse = " ")
  cvector[i] <- gsub(".+?Table.Contents(.+)", "\\1", cvector[i], ignore.case = T)
}

# Construct DTM based on the data from the recent years
dtm.a <- DocumentTermMatrix(Corpus(VectorSource(cvector)),
                               control = list(removePunctuation = T,
                                              stopwords = T,
                                              tolower = T,
                                              removeNumbers = T,
                                              wordLengths=c(4, 20),
                                              bounds = list(global = c(10,75))))

# Clean DTM
dtm <- dtm.a[row_sums(dtm.a) > 10,]
del.companies.recent <- rownames(dtm.a[row_sums(dtm.a) <= 10,])

# Constructing LDA
topic <- LDA(dtm, 
                  k = 21, 
                  method = "Gibbs",
                  control = list(
                    seed = 1234, 
                    burnin = 100,  
                    iter = 300,  
                    keep = 1,
                    save = F,
                    verbose = 10))

# Top terms per topic
beta <- exp(topic@beta)

recent.terms <-list()
recent.prob <- list()

for (i in c(1:topic@k)){
  recent.terms[[i]] <- head(topic@terms[order(beta[i,], decreasing = T)], 75)
  recent.prob[[i]] <- head(sort(beta[i,], decreasing = T), 75)
}

recent.terms.df <- data.frame(matrix(unlist(recent.terms),
                                  nrow=length(recent.terms),
                                  byrow=T))

# Compare new topics to the old ones----
# Correlation table
topic.cor <- data.frame(matrix(nrow = topic@k,
                               ncol = topic.test@k))


# Fill in the table with the cosim data
CosineSimilarity <- function(a,b){
  sum(a*b)/sqrt(sum(a^2)*sum(b^2))}

# Calculate the cosim based on the term frequencies of the topics' keywords
for (i in c(1:topic@k)){
  for (j in c(1:topic.test@k)){
    
    training.topic <- data.frame(matrix(data = training.prob[[j]],
                                        nrow = 1,
                                        ncol = ncol(training.terms.df),
                                        dimnames = list(1,training.terms[[j]])))
    
    recent.topic <- data.frame(matrix(data = recent.prob[[i]],
                                        nrow = 1,
                                        ncol = ncol(recent.terms.df),
                                        dimnames = list(2,recent.terms[[i]])))
  
    comb <- smartbind(training.topic, recent.topic)
    comb[is.na(comb)] <- 0

      topic.cor[i,j] <- CosineSimilarity(comb[1,], 
                                         comb[2,])
    }
}

# Maximum correlation between the old themes and the new themes
max(topic.cor)

# Find themes with least correlation (threshold is manually set to 10%)
apply(topic.cor[1:nrow(topic.cor), ], 1, max)
x <- which(apply(topic.cor[1:nrow(topic.cor), ], 1, max)<0.1)

# Dataframe with the emerging industties and top 20 most frequent keywords
emerging.industries <- recent.terms.df[x,c(1:20)]

# Create a wordcloud for the every theme
for (i in x){
  png(filename = paste0(i,".png"),
      width = 425, height = 425, units = "px", pointsize = 16,
      bg = "white")
  
    wordcloud(words = recent.terms[[i]],
            freq = recent.prob[[i]],
            scale = c(4, .5),
            random.order = F,
            rot.per = 0.5,
            colors = brewer.pal(8, "Dark2"))  

  dev.off()
}

# Topic distributhion per document decade ago----
gamma <- topic.test@gamma
dim(gamma)

topic.distr <- list()
sample.cvector <- sample.cvector[-as.numeric(training.del)]
training <- training[-as.numeric(training.del),]

for (i in c(1:nrow(training))){
  topic.distr[[i]] <- gamma[i,]*100
}

# Topic distribution (columns) by document (rows)
topic.distr <- data.frame(matrix(unlist(topic.distr),
                                 nrow=length(topic.distr),
                                 byrow=T))

colnames(topic.distr) <- c(1:topic.test@k) # can be changed to the topic titles

training$topic <-  NA

for (i in c(1:nrow(training))){
  training$topic[i] <- colnames(sort(topic.distr[i,], decreasing = T, )[1])
}

# Topic distributhion per document in recent years----
gamma <- topic@gamma
dim(gamma)

topic.distr <- list()

for (i in c(1:length(cvector))){
  topic.distr[[i]] <- gamma[i,]*100
}

# Topic distribution (columns) by document (rows)
topic.distr <- data.frame(matrix(unlist(topic.distr),
                                 nrow=length(topic.distr),
                                 byrow=T))

colnames(topic.distr) <- c(1:topic@k) # can be changed to the topic titles

recent.ipo$topic <-  NA

for (i in c(1:nrow(recent.ipo))){
  recent.ipo$topic[i] <- colnames(sort(topic.distr[i,], decreasing = T, )[1])
}

recent.ipo$names <- rownames(recent.ipo)

recent.ipo %>% 
  filter(topic %in% x)

# KWIC to apply to the found technologies----
# To look into the word use, chang it at line 270 and rename the result table at line 294
# We used KWIC for the terms: ecommerce, blockchain, cybersecurity, biopharm, etc.  
business.des <-  sapply(cvector, 
                        function(i) scan(text =i,
                                         what = "character",
                                         quote = ""))
head (business.des[[1]])

# Creating table out of tokenized dataset that match the keyword we look into
index.env <-  sapply(business.des, 
                     function(i) grep("biopharm.+", i, ignore.case = T))

make.KWIC <-  function(index.env, business.des, n, doc.nr){
  KWIC <- tibble (left = sapply(index.env,
                                function(i) {paste(business.des[(i-n):(i-1)], collapse = " ")}),
                  keyword = business.des[index.env],
                  right = sapply(index.env,
                                 function(i) {paste(business.des[(i+1):(i+n)], collapse = " ")}),
                  doc.nr = doc.nr,
                  position.in.text = index.env/(length(business.des)*0.01))
  return(KWIC)
}

result <-  list()

for(i in 1:length(business.des)){
  result[[i]] <- make.KWIC(index.env[[i]],
                           business.des[[i]],
                           n = 3,
                           doc.nr = i)
}
merged.results <- do.call ("rbind", result)

# Filter by the companies belonging to the topic
biopharm <- merged.results %>% 
  filter(merged.results$doc.nr %in% recent.ipo$names)