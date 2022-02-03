# LDA-based Hierarchical Clustering for Story point Estimation (LHC-SE)
Using clustering of textual features to estimate effort in agile projects. 

This repository is supplementary to the paper entitled "[Investigating the Effectiveness of Clustering for Story Point Estimation](https://solar.cs.ucl.ac.uk/pdf/tawosi2022saner.pdf)", authored by Vali Tawosi, Afnan Al-Subaihin, and Federica Sarro, which is accepted for presentation at the 29th IEEE International Conference on Software Analysis, Evolution and Reengineering (SANER 2022). It contains the R source code and datasets used in the study.


# How to train the LDA model:

`Rscript generate_lda_model.R [PATH TO DATASET]`

Since this step may take long, we provide the best lda_model achieveved with the Tawosi Dataset. The model can be found at ../results/lda_2265.rda in this repository.

# How to run the estimation (from terminal):
Extract the dataset fles and run:

`Rscript lda_project_specific.R [PATH TO DATASET] [PATH TO LDA MODEL] [Cluster Building Strategy:'MAE', 'MdAE', or 'sil'] [Algorithm Variant: 'LHC-SE', 'LHC-TC-SE', or 'LHC-TC-TFIDF-SE']`
