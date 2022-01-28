##############################################
# Authors: Afnan Al-Subaihin and Vali Tawosi #
# ############################################

# How to run from terminal:
# Rscript generate_lda_model.R [PATH TO DATASET]
args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Exactly one argument must be supplied: path to data directory.", call.=FALSE)
}

data_path <- args[1]
source("ee_clust.r")

data <- get_data(data_path, "train")
cat("Train Corpus Dimensions: ", dim(data), "\n")
valid <- get_data(data_path, "valid")
cat("Validation Corpus Dimensions: ", dim(valid), "\n")

#Generate model:
lda(as.matrix(data$text), as.matrix(valid$text))

# Since this step takes long, we provide the best lda_model achieveved with the data. The model can be found at ../results/lda_2265.rda