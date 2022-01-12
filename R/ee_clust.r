############ INCLUDES##############
library(stringr)
library(RTextTools)
library(topicmodels)
library(SnowballC)
library(tm)
library(cluster)
library(skmeans)
library(lsa)
library(slam)
##################################

#Specify where to save all generated data (intermediate)
data.prefix <- "../results/"
if(!dir.exists(data.prefix))
    dir.create(data.prefix, showWarnings = TRUE, recursive = FALSE, mode = "0777")

# Definitions
extendedstopwords <- c("a", "aaaaa", "aaaaaa", "aaaaaaa", "aaaaaaaa", "aaaaaaaaaa", "about", "above", "across", "after", "again", "against", "all", "almost", "alone", "along", "already", "also", "although", "always", "am", "among", "an", "and", "another", "any", "anybody", "anyone", "anything", "anywhere", "are",  "aren't", "around", "as", "ask", "asked", "asking", "asks", "at", "away", "b", "back", "be", "became", "because", "become", "becomes", "been", "before", "began", "behind", "being", "beings", "below", "best", "better", "between", "big", "both", "but", "by", "c", "came", "can", "cannot", "can't", "case", "cases", "certain", "certainly", "clear", "clearly", "come", "could", "couldn't", "d", "did", "didn't", "differ", "different", "differently", "do", "does", "doesn't", "doing", "done", "don't", "down", "downed", "downing", "downs", "during", "e", "each", "early", "either", "end", "ended", "ending", "ends", "enough", "even", "evenly", "ever", "every", "everybody", "everyone", "everything", "everywhere", "f", "face", "faces", "fact", "facts", "far", "felt", "few", "find", "finds", "first", "for", "four", "from", "full", "fully", "further", "furthered", "furthering", "furthers", "g", "gave", "general", "generally", "get", "gets", "give", "given", "gives", "go", "going", "good", "goods", "got", "great", "greater", "greatest", "group", "grouped", "grouping", "groups", "h", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "her", "here", "here's", "hers", "herself", "he's", "high", "higher", "highest", "him", "himself", "his", "how", "however", "how's", "i", "i'd", "if", "i'll", "i'm", "important", "in", "interest", "interested", "interesting", "interests", "into", "is", "isn't", "it", "its", "it's", "itself", "i've", "j", "just", "k", "keep", "keeps", "kind", "knew", "know", "known", "knows", "l", "large", "largely", "last", "later", "latest", "least", "less", "let", "lets", "let's", "like", "likely", "long", "longer", "longest", "m", "made", "make", "making", "man", "many", "may", "me", "member", "members", "men", "might", "more", "most", "mostly", "mr", "mrs", "much", "must", "mustn't", "my", "myself", "n", "necessary", "need", "needed", "needing", "needs", "never", "new", "newer", "newest", "next", "no", "nobody", "non", "noone", "nor", "not", "nothing", "now", "nowhere", "number", "numbers", "o", "of", "off", "often", "old", "older", "oldest", "on", "once", "one", "only", "open", "opened", "opening", "opens", "or", "order", "ordered", "ordering", "orders", "other", "others", "ought", "our", "ours", "ourselves", "out", "over", "own", "p", "part", "parted", "parting", "parts", "per", "perhaps", "place", "places", "point", "pointed", "pointing", "possible", "q", "quite", "r", "rather", "really", "right",  "s", "said", "same", "saw", "say", "says", "see", "seem", "seemed", "seeming", "seems", "sees",  "shall", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't",  "since", "small",  "so", "some", "somebody", "someone", "something", "somewhere", "state", "states", "still", "such", "sure", "t", "take", "taken", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "therefore", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "thing", "things", "think", "thinks", "this", "those", "though", "thought", "thoughts", "three", "through", "thus", "to", "today", "together", "too", "took", "toward",  "two", "u", "under", "until", "up", "upon", "us", "use", "used", "uses", "v", "very","via", "w", "want", "wanted", "wanting", "wants", "was", "wasn't", "way", "ways", "we", "we'd", "well", "we'll",  "went", "were", "we're", "weren't", "we've", "what", "what's", "when", "when's", "where", "where's", "whether", "which", "while", "who", "whole", "whom", "who's", "whose", "why", "why's", "will", "with", "within", "without", "won't", "work", "worked", "working", "works", "would", "wouldn't", "x", "y", "yes", "yet", "you", "you'd", "you'll", "your", "you're", "yours", "yourself", "yourselves", "you've", "z")
extendedstopwords <- c(extendedstopwords, gsub("'", "", grep("'", extendedstopwords, value = T)))

#Function used internally to replace all non-alphanumeric characters.
purify <- function(x) str_replace_all(x, "[^[:alnum:]]", " ")

#Function to replace all urls with sapce.
remove.urls <- function(x) str_replace_all(x, "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+#]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+", " ")

#Define a class to hold clustering solutions:
setClass(Class = "Solution",
         slots = c(original.data = "data.frame",
                   centers = "data.frame",
                   dendrogram = "list", #???
                   sil_gran = "data.frame", #silhouette at different levels of granularity
                   silhouette = "ANY",
                   calinski_harabasz = "numeric",
                   c_index = "numeric",
                   dunn_index = "numeric",
                   runtime = "ANY",
                   call = "ANY"))

# Gets all data in the passed folder
# Adds a column showing the project name labeled as 'project'
# Parameters:
# path: path to a folder containing data
# type: type of data (train, valid, test)
# project: project name. 
#          if NULL, the function reads all the files found in the directory
# Returns: 
# an R data frame containing the data
get_data <- function(path, type, project = NULL, LHC.variant = 'LHC-SE') {
    for (file in list.files(path)) {
        project_name <- substr(
            file,
            0,
            unlist(gregexpr("-", file)) - 1
        )
        data_type <- substr(
            file,
            unlist(gregexpr("-", file)) + 1,
            unlist(gregexpr(".csv", file)) - 1
        )

        # Check if current file is of the required type (train, valid, test)
        if (data_type != type) next
        
        # Check if current file belongs to the required project,
        # otherwise, skip. 
        if ( ! is.null(project) && project_name != project) next

        temp <- read.csv(paste(path, file, sep = "/"), header = TRUE, stringsAsFactors = FALSE)[c('issuekey', 'storypoint', 'title', 'description_text')]
        #  concatenate title and description to get issue-context (text).
        temp$text <- paste(temp$title, temp$description_text, sep = ' ')

        # add project name
        temp <- cbind(temp, project = project_name)
        # remove columns that are no longer needed
        temp <- subset(temp, select = -c(title, description_text))

        # read and add extra features used in other variants
        if(LHC.variant == 'LHC-TC-SE' | LHC.variant == 'LHC-TC-TFIDF-SE'){
            #  add issue-context length (to be used with LHC-TC-SE)
            temp$length <- nchar(temp$text)
            
            # read and add the 1-hot matrix of Type and Component 
            if(file.exists(paste0(path, project_name, '-', data_type,'_features.csv'))){
                
                extra.features <- read.csv(paste0(path, project_name, '-', data_type,'_features.csv'), header = TRUE, stringsAsFactors = FALSE)
                cat("1-hot matrix of Type and Component + story length is read and added to features.\n")
                temp <- cbind(temp, extra.features[,-c(1)])
            }
        }

        if (!exists("dataset")) {
            dataset <- temp
        }
        else {
            dataset <- rbind(dataset, temp)
        }
        rm(temp)
    }
    return(dataset)
}

# Given a path to a folder, retrieve a list of unique project names in dataset:
get_project_names <- function(path){
    dataset <- list()
    for (file in list.files(path)) {
        project_name <- substr(
            file,
            0,
            unlist(gregexpr("-", file)) - 1
        )

        dataset <- rbind(dataset, project_name)
    }
    return(unique(dataset))
}

# function to cluster the data
# Data: array or matrix of issue with all their columns (needed for validation)
# Distance: Distance matrix (as.dist()); if null, it is calculated
# Method: Hierarchical clustering agglomeration method
# can be: "ward.D", "ward.D2", "single", "complete", "average",
# "mcquitty", "median" or "centroid". We used "ward.D2" in our study.
# ev: The evaluation method based on which to select best k. Can be either 'Sil'
# for silhouette or 'MAE' or 'MdAE' for minimum mean/median absolute error. 
# when selecting 'MAE' or 'MdAE', a validation set must be passed. 
# Returns:
# object:  The text list with added column containing assigned cluster number
#          The cluster silhouettes
cluster_h <- function(data, test, valid, dtm, FE = "LDA", distance = NULL, verbose = F, 
                    method = "ward.D2", ev = "sil", project_name = NULL, lda_model = NULL)
{
    if(is.null(project_name))
        project_name <- "All_projects"

    dataset_size <- dim(dtm$train)[1]
    vocabulary_size <- dim(dtm$train)[2]
    
    if (verbose) {
        cat("Evaluation Based-on: ", ev, "\n")
        cat("Corpus Dimensions: ", dim(dtm$train), "\n")
    }
    #If distance matrix was not passed, calculate distance (using cosine)
    #This is a slow step, if distance to be calculated more than once,
    #calculate it beforehand and pass it. Or if you need other than cosine. 
    if (is.null(distance)) {
        if (verbose)
            cat("Calculating distance matrix..", "\n")
        start.time <- Sys.time()
        distance <- as.dist(skmeans_xdist(dtm$train))
        # cat(summary(distance), "\n")
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        file_name <- paste(data.prefix, project_name, "_distance_", FE, ".rda", sep = "")
        save(distance, file = file_name)
        cat("Distance matrix saved to ", file_name, "\n")
        if (verbose == T)
            cat("Time taken to calculate distance matrix: ", time.taken, "\n")
    }

    #Hierarchical Clustering:
    start.time <- Sys.time()
    dendrogram <- hclust(distance, method = method)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    if (verbose == T)
        cat("Time taken to cluster: ", time.taken, "\n")
    file_name <- paste(data.prefix, project_name, "_dendrogram_", FE, ".rda", sep = "")
    save(dendrogram, file = file_name)
    cat("Dendrogram saved to ", file_name, "\n")
    
    # Loop through dendrogram, finding the k that produceds the maximum silhouette
    eval_gran <- data.frame()
    # define the sequence by which the loop iterates (no need to test at every point)
    step <- trunc(dataset_size * 0.1)
    ks <- seq(3, dataset_size-step, by = step)
    if (verbose)
        cat("Looping through dendrogram to plot silhouette scores..", "\n")
    for (i in ks) {
        # Cut the tree at level i:
        current <- cutree(dendrogram, k = i)

        # Calculate the evaluative measure:
        sil <- summary(silhouette(current, distance))$avg.width
        data$labels <- current
        # call validate (but on validation set not test set)
        evals <- validate(data = data, test = valid, dtm.train = dtm$train, dtm.test = dtm$valid, eval.method = ev)$mae_mdae
        
        # Combine cluster number and cost together, write to df
        eval_gran <- rbind(eval_gran, cbind(i, sil, evals[1], evals[2]))
        if (verbose)
            cat("\rDone loop : ", i)
    }
    names(eval_gran) <- c("granularity", "silhouette", "MAE", "MdAE")
    
    #  save the result of cut-off point exploration 
    file_name <- paste(data.prefix, project_name,"_gran_", FE, ".rda", sep = "")
    save(eval_gran, file = file_name)
    cat("\nGranularity evaluation table is saved to ", file_name, "\n")

    # Plot the analysis for different ks:
    file_name <- paste(data.prefix, project_name, "_gran_plot_", FE, ".pdf", sep = "")
    pdf(file_name)
    matplot(eval_gran$granularity, 
            cbind(eval_gran$silhouette, eval_gran$MAE, eval_gran$MdAE), 
            type = c("b"),
            pch=1,
            col = 1:4,
            xlab = "Number of Clusters",
            ylab = "Evaluation Metrics: Silhouette, MAE and MdAE",
            main = paste("Cluster Quality for ", project_name)) #plot
    legend("right", legend = c("Silhouette", "MAE", "MdAE"), col=1:4, pch=1) 
    dev.off()
    cat("Plot successfully generated to", file_name, "\n")

    # Report the best k found, based on the cut-off k-selection strategy 
    if (verbose) {
        if(ev == "sil"){
            k <- eval_gran[which.max(eval_gran$silhouette), ]
            cat("\nBest K is ", k$granularity, " Producing ", ev, " of ", k$silhouette, "\n")
        }
        else if (ev == "MAE") {
           k <- eval_gran[which.min(eval_gran$MAE), ]
           cat("\nBest K is ", k$granularity, " Producing ", ev, " of ", k$MAE, "\n")
        }
        else if (ev == "MdAE"){
            k <- eval_gran[which.min(eval_gran$MdAE), ]
            cat("\nBest K is ", k$granularity, " Producing ", ev, " of ", k$MdAE, "\n")
        }
    }

    # return the clusters after cutting it from the best level found (k)
    return(cutree(dendrogram, k = k$granularity))
}

#Function that builds the vector space, using the specified weighting
vsm <- function(data, weighting = weightTf, verbose = T) {

    # Pre-processing the text
    # Removing URLs
    data <- lapply(data, remove.urls)
    # Removing non alphabetic and numeric chars
    data <- lapply(data, purify)

    # The rest of the pre-processing happens in LDA function, thus they are passed in using a dtm.control object
    dtm.control <- list(
        # convert text to lowercase
        tolower = T,
        # remove punctuation
        removePunctuation = T,
        # do not remove numbers
        removeNumbers = F,
        # remove stop-words
        stopwords = c(stopwords("english"), extendedstopwords),
        # do not perfrom stemming
        stemming = F,
        # remove alone chars in the text 
        wordLengths = c(2, Inf),
        weighting = weighting
    )
    corp <- Corpus(VectorSource(as.vector(data)))
    dtm <- DocumentTermMatrix(corp, control = dtm.control)

    if (verbose)
        cat("Corpus Dimensions, before cleaning: ", dim(dtm), "\n")

    # Remove empty documents (issue-context) and unused terms (terms with zero TF score)
    dtm <- dtm[,col_sums(dtm) > 0]
    dtm <- dtm[row_sums(dtm) > 0,]

    dtmm <- as.matrix(dtm)

    if (verbose)
        cat("Corpus Dimensions, after cleaning: ", dim(dtm), "\n")

    termFreqs <- colSums(dtmm)
    stopifnot(!any(termFreqs == 0))
    docLens <- rowSums(dtmm)
    stopifnot(!any(docLens == 0))
    return(list(data = data, dtm = dtm, dtmm = dtmm,
                term_freq = termFreqs, doc_lengths = docLens))

}

#Function that builds a vector space out of LDA topics
# t is number of topics (if known), leave null to calculate the t that produces 
# the least perplexity.
# if t is null, need to send validation data as well.
lda <- function(data, valid = NULL, t = NULL) {
    cat("Generating LDA Model..\n")
    data <- vsm(data)
    valid <- vsm(valid)
    if (is.null(t)) {
        t_res <- find_best_t(data$dtm, valid$dtm)
        cat("Best t: ", t_res[1,1], " prodcuced perplexity: ", t_res[1,2], "\n")
        t <- t_res[1,1]
    }
    start.time <- Sys.time()
    lda_model <- LDA(data$dtm, k = t, method = "Gibbs",
                    control = list(alpha = 1 / t,
                    delta = 0.1,
                    burnin = 50, iter = 500, keep = 50, #####TUNE#####
                    verbose = 100))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    cat("Time taken to generate final LDA Model: ", time.taken, "\n")
    file_name <- paste(data.prefix, "lda_", t, ".rda", sep = "")
    save(lda_model, file = file_name)
    cat("Final LDA model saved to ", file_name, "\n")
    p <- perplexity(lda_model, valid$dtm)
    cat("The perplexity of this model is ", p, "\n")

    return(lda_model)

}

find_best_t <- function(training, test) {
    start.time <- Sys.time()
    ts <- seq(15, 2000, by = 250) 
    models <- lapply(ts, function(t) LDA(training, k = t, method = "Gibbs",
                        control = list(alpha = 1/t, delta = 0.1,
                        burnin = 50, iter = 300, keep = 50, 
                        verbose = 10)))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    cat("Time taken to generate LDA models: ", time.taken, "\n")
    perps <- sapply(models, perplexity, test)
    pdf(paste(data.prefix,"perplexity_graph.pdf", sep=""))
    plot(ts, perps, xlab = "Number of topics", ylab = "Perplexity")
    dev.off()

    h <- cbind(ts, perps)
    h <- as.data.frame(h)
    t <- h[which.min(h$perps), ]

    return(t)

}


# Data: must have the cluster labels included. 

validate <- function(data, test, dtm.train, dtm.test, eval.method = 'MdAE'){
    # Find statistics per cluster:
    means <- aggregate(list(sp = data$storypoint), list(label = data$labels), mean)
    medians <- aggregate(list(sp = data$storypoint), list(label = data$labels), median)

    # Calculate the distance between the two:
    distance <- skmeans_xdist(dtm.test, dtm.train)

    # Now, for each row, find the index of the issue
    #that has the minimum distance value to the current one:
    closest <- apply(distance, 1, which.min)
    closest.labels <- data$labels[closest]

    # Now, construct a dataframe that, for each issue (row)
    #contains the sp, closest.sp, mean.cluster.sp, and median.cluster.sp
    results <- data.frame(closest = closest,
            sp = test$storypoint,
            closest.sp = data$storypoint[closest],
            mean.cluster.sp = means$sp[closest.labels],
            median.cluster.sp = medians$sp[closest.labels]
            )

    # compute the median absolute error (MdAE) of the results, used in best cut-off k selection based on MdAE
    ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
    mae <- mean(ae.sp.cluster.median)
    mdae <- median(ae.sp.cluster.median)

    return(list(results = results, mae_mdae = c(mae, mdae)))
}

get_dtm_lda <- function(training_text, validation_text, testing_text, lda_model) {

    dtm.train <- vsm(training_text, verbose = F)$dtmm
    dtm.train <- posterior(lda_model, dtm.train)$topics
    
    dtm.valid <- vsm(validation_text, verbose = F)$dtmm
    dtm.valid <- posterior(lda_model, dtm.valid)$topics
    
    dtm.test <- vsm(testing_text, verbose = F)$dtmm
    dtm.test <- posterior(lda_model, dtm.test)$topics
   
    return(list(train = dtm.train, valid = dtm.valid, test = dtm.test))
}

get_dtm_tfidf <- function(training_text, testing_text, valid_text =NULL) {
    
    #Note: need to recalculate DTM so vocabulary would include
    #both sets.
    
    train_size <- dim(training_text)[1]
    test_size <- dim(testing_text)[1]
    if(!is.null(valid_text))
        valid_size <- dim(valid_text)[1]
    else
        valid_size <- 0
    
    if(!is.null(valid_text))
        combined <- rbind(training_text, valid_text, testing_text)
    else
        combined <- rbind(training_text, testing_text)
    
    stopifnot(dim(combined)[1] == train_size + test_size  + valid_size)
    dtm_t <- vsm(combined, verbose = F)$dtmm
    
    #Now separate the dtms:
    dtm_t.train <- dtm_t[1:train_size, ]
    dtm_t.test <- dtm_t[-(1:(train_size+valid_size)), ]
    dtm_t.valid <- dtm_t[(train_size+1):(train_size+valid_size), ]
    
    stopifnot(dim(dtm_t.train)[1] == train_size)
    stopifnot(dim(dtm_t.valid)[1] == valid_size)
    stopifnot(dim(dtm_t.test)[1] == test_size)
    
    return(list(train = dtm_t.train, test = dtm_t.test, valid = dtm_t.valid))
}