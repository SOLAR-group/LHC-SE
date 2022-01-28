##############################################
# Authors: Afnan Al-Subaihin and Vali Tawosi #
# ############################################

# How to run from terminal:
# Rscript lda_project_specific.R '../../Tawosi dataset/' '../results/lda_2265.rda' 'MAE' 'LHC-SE'

args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 4) {
  stop("Exactly four arguments must be supplied: path to data directory, path to LDA model (.rda), evaluation method (one of 'MAE', 'MdAE', or 'sil'), and LHC variant (one of 'LHC-SE', 'LHC-TC-SE', or 'LHC-TC-TFIDF-SE')",
       call.=FALSE)
}


data_path <- args[1]
load(args[2]) #lda_model
evaluation <- args[3]
LHC.variant <- args[4]


source("ee_clust.r")

# Get a list of project names:
projects <- get_project_names(data_path)

# Loop through all projects:
for (project_name in projects) {
  
    cat(paste("\n############################# PROJECT", project_name, "#############################\n", sep=" "))
    cat(paste("Now working on Project ", project_name, "...\n", sep=""))
    data <- get_data(data_path, "train", project_name, LHC.variant)
    cat("Train Corpus Dimensions: ", dim(data), "\n")
    valid <- get_data(data_path, "valid", project_name, LHC.variant)
    cat("Validation Corpus Dimensions: ", dim(valid), "\n")
    test <- get_data(data_path, "test", project_name, LHC.variant)
    cat("Testing Corpus Dimensions: ", dim(test), "\n")

    if(LHC.variant != 'LHC-SE'){
    # preparing the extra features (extra features include binary indicators for Issue Type, Components, and title+description length, and 1-hot matrix of the occurance )
      data.extra <- subset(data, select = -c(issuekey, storypoint, text, project))
      data.rows  <- dim(data.extra)[1]
      data.extra <- matrix(data = unlist(data.extra), nrow = data.rows, byrow = F)
      
      stopifnot(sum(is.na(as.numeric(unlist(data.extra))))==0)
  
      valid.extra<- subset(valid, select = -c(issuekey, storypoint, text, project))
      valid.rows <- dim(valid.extra)[1]
      valid.extra<- matrix(data = unlist(valid.extra), nrow = valid.rows, byrow = F)
      
      stopifnot(sum(is.na(as.numeric(unlist(valid.extra))))==0)
  
      test.extra <- subset(test, select = -c(issuekey, storypoint, text, project))
      test.rows  <- dim(test.extra)[1]
      test.extra <- matrix(data = unlist(test.extra), nrow = test.rows, byrow = F)
    }
    
    cat("Fitting LDA model to training, testing and validation sets..\n")
    dtm_lda <- get_dtm_lda(as.matrix(data$text), as.matrix(valid$text), as.matrix(test$text), lda_model = lda_model)

    if(LHC.variant == 'LHC-TC-TFIDF-SE'){
      cat("Getting document-term matrix from tf-idf vsm for training, testing and validation sets..\n")
      dtm_tfidf <- get_dtm_tfidf(as.matrix(data$text), as.matrix(test$text), as.matrix(valid$text))
    }

    dtm = list()
    
    # if LHC.variant is 'LHC-SE', we use only lda features
    if(LHC.variant == 'LHC-SE'){
      dtm <- dtm_lda
    }
    # if LHC.variant is 'LHC-TC-SE', join the lda features with type, component and length 
    else if(LHC.variant == 'LHC-TC-SE'){
      dtm$train = cbind(dtm_lda$train, data.extra)
      dtm$valid = cbind(dtm_lda$valid,valid.extra)
      dtm$test = cbind(dtm_lda$test, test.extra)
    }
    # if LHC.variant is 'LHC-TC-TFIDF-SE', join the lda features with tfidf, and type, component and length 
    else if(LHC.variant == 'LHC-TC-TFIDF-SE'){
      dtm$train = cbind(dtm$train, dtm_tfidf$train, data.extra)
      dtm$valid = cbind(dtm$valid, dtm_tfidf$valid, valid.extra)
      dtm$test = cbind(dtm$test, dtm_tfidf$test, test.extra) 
    }

    stopifnot(dim(dtm$train)[2]==dim(dtm$valid)[2],dim(dtm$valid)[2]==dim(dtm$test)[2]) 
    
    #Carry out clustering. The code is in 'ee_cluster.r'
    data$labels <- cluster_h(data, test, valid, dtm,
                FE = "LDA",
                verbose = T,
                project_name = project_name,
                ev = evaluation,
                lda_model = lda_model)

    #Find statistics per cluster:
    results <- validate(data = data, test = test, dtm.train = dtm$train, dtm.test = dtm$test)$results

    # Save estimations
    write.csv(results, 
              file = paste0('../results/', project_name, '_results.csv'),
              row.names = F)  
    
    # Print estimation statistics
    ae.sp.closest <- abs(results$sp - results$closest.sp)
    cat("\nStory Point - Absolute Error when matching with closest point:\n")
    cat(summary(ae.sp.closest))
    cat("\nMean of Absolute Error: ", mean(ae.sp.closest))
    cat("\nMedian of Absolute Error: ", median(ae.sp.closest), "\n")
    
    ae.sp.cluster.mean <- abs(results$sp - results$mean.cluster.sp)
    cat("\nStory Point - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.sp.cluster.mean))
    cat("\nMean of Absolute Error: ", mean(ae.sp.cluster.mean))
    cat("\nMedian of Absolute Error: ", median(ae.sp.cluster.mean), "\n")
    
    ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
    cat("\nStory Point - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.sp.cluster.median))
    cat("\nMean of Absolute Error: ", mean(ae.sp.cluster.median))
    cat("\nMedian of Absolute Error: ", median(ae.sp.cluster.median), "\n")
    
    cat("\n########################################################################\n")
    
} #End loop through projects.
