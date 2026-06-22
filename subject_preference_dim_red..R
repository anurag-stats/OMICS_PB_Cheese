#' ---
#' output:
#'   pdf_document: default
#'   html_document: default
#' ---
#' The major problem in the subject preference data is that we have the data for preference of cheeses,
#' but the preference about dairy cheese alone are too sparse as they are, so we'll reduce dimensions
#' and extract some meaningful features that better represent the signal.
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
library(psych)
library(dplyr)
library(tidyr)
library(ggplot2)
library(factoextra)
library(FactoMineR)

#' 
## ---------------------------------------------------------------------------------------------------------------
df <- read.csv("Subject_Data.csv")
head(df)

#' 
## ---------------------------------------------------------------------------------------------------------------

cheese_vars <- colnames(df)[9:18]

X <- df %>%
  dplyr::select(all_of(cheese_vars))

#Sanity Check: Ensuring 0-1 coding
X <- X %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

# Optional sanity checks
stopifnot(all(sapply(X, function(z) all(z %in% c(0, 1, NA)))))


#' 
#' 
#' Since these variables have a dichotomous structure, we will use tetrachoric correlation, it assumes
#' there is reason you choose 0 or 1, i.e. there is a continuous variable which represents your tendency
#' to like a certain cheese, so a herirachical model, which is giving you 0 or 1 now, we are looking
#' at the correlation between these hidden tendency variables. 
#' Given 0 your tendency comes from distribution A and given 1, tendency comes from distribution Y.
#' We will find correlation between X and Y.
#' 
#' Likelihood of liking each dairy cheese:
#' 
## ---------------------------------------------------------------------------------------------------------------
like_rates <- colMeans(X, na.rm = TRUE)
print(round(like_rates, 3))


#' 
#' 
#' Tetrachoric Correlation Matrix:
## ---------------------------------------------------------------------------------------------------------------
tet_out <- psych::tetrachoric(as.matrix(X))
R_tet <- tet_out$rho
print(round(R_tet, 2))

## ---------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(reshape2)

dist_mat <- as.dist(1 - R_tet)
hc <- hclust(dist_mat)
tetra_mat_ord <- R_tet[hc$order, hc$order]

tetra_long <- melt(tetra_mat_ord)

ggplot(tetra_long, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "steelblue",
    mid = "white",
    high = "firebrick",
    midpoint = 0,
    limits = c(-1, 1),
    name = "Tetrachoric\ncorrelation"
  ) +
  coord_fixed() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_blank()
  )

#' 
#' Once we have these we will perform heirarchical clustering to see which cheeses are similar to each
#' other, using a dendogram. Distance metric for similarity is 1-r, it converts strong positive 
#' correlation into small distance, so cheeses with highly similar liking patterns are placed close 
#' together and are likely to merge early in the dendrogram.
#' if Cheddar and Gouda have a high tetrachoric correlation, then 1-r is small, so the algorithm 
#' treats them as neighbors. If Blue Cheese and American have weak or negative association, then their 
#' distance is larger, so they stay apart until much later.
#' 
## ---------------------------------------------------------------------------------------------------------------
D <- as.dist(1 - R_tet)
hc <- hclust(D, method = "average")

plot(hc, main = "Cheese clustering from tetrachoric correlations",
     xlab = "", sub = "", cex = 0.9)

#' Things that are in the same child node are very similar to each other (theoretically)
#' 
#' We will go with 4 clusters based on visual checks of the dendogram.
#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
cluster_4 <- cutree(hc, k = 4)
print(cluster_4)

#' 
#' 
#' 
#' Factor Analysis uncovers hidden variables that represent real relationships. 
#' What broader taste traits explain the whole response pattern?
#' 
## ---------------------------------------------------------------------------------------------------------------
fa2 <- psych::fa(R_tet, nfactors = 2, n.obs = nrow(X), fm = "minres", rotate = "oblimin")
fa3 <- psych::fa(R_tet, nfactors = 3, n.obs = nrow(X), fm = "minres", rotate = "oblimin")
fa4 <- psych::fa(R_tet, nfactors = 4, n.obs = nrow(X), fm = "minres", rotate = "oblimin")

print(fa2$loadings, cutoff = 0.25)
print(fa3$loadings, cutoff = 0.25)
print(fa4$loadings, cutoff = 0.25)


#' 
#' 
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
psych::fa.parallel(x = as.matrix(X), cor = "tet", fa = "fa", n.iter = 50)


#' Parallel analysis indicates use of 2 latent factors, eigen values from 3rd factor on are somewhat higher for the resampled and simulated data than our actual data, so they will probably not add much information.
#' 
#' 
#' 
#' We will use 2 latent factors, the 2 latent factors also explain about 60% of the variability.
#' 
## ---------------------------------------------------------------------------------------------------------------
fa2 <- fa(R_tet, nfactors = 2, n.obs = nrow(X), fm = "minres", rotate = "oblimin")
print(fa2$loadings, cutoff = 0.30)
print(fa2$Phi)


#' 
#' Mozzarella and cream cheese load very strongly on the first latent factor, while Parmesan, Gouda and Blue cheese also show moderately strong loadings on the first latent factor. Cheddar loads very strongly on the second latent factor, while Gouda, Pepper Jack, Feta and Swiss cross load moderately on both latent factors. 
#' It is difficult to see a real structure here due to the cross loadings so we'll use the latent factors as covariates and make inferences on them.
#' 
#' My opinion: The latent factors don't correspond to individual distinctive types of cheeeses as I had hoped. Factor 1 may reflect broad openness to creamy and distinctive cheeses (blue cheese here does seem a little weird) and factor 2 may correspond to firmer, savory, cheeses which people may use on a daily basis. Factor 2 is kind of dominated by cheddar, this may be due to the fact that cheddar is universally liked and 91% of subjects like Cheddar Cheese, so this may not a
#' 
#' When making inferences, remember the dependence of individual cheeses of latent factors:
#' MR1 : Mozzarella, Cream cheese, parmesan, blue cheese, Cream Cheese
#' MR2: Cheddar, Gouda, Swiss, Pepper Jack, Feta
#' 
#' Gouda, Swiss, Pepper Jack and Feta cross load so it's difficult to say, based on feature importance scores of MR's, whether they are the reason. They are mixed items not pure indicators of either latent factor
#' 
#' PS: A 3 factor solution was considered too, but heavy correlation was observed between the factors and we can't orthogonally rotate these since I do expect their to be correlation in cheese types, also cheddar single handedly dominated an entire latent factor, we somewhat see that in 2 factors too, but it was much more severe.
#' Adding the latent factors to our subject level data:
#' 
## ---------------------------------------------------------------------------------------------------------------
scores <- factor.scores(
  x = X,
  f = fa2,
  method = "tenBerge"
)$scores

df$Cheese_MR1 <- scores[,1]
df$Cheese_MR2 <- scores[,2]

head(df)


#' 
#' So going forward followings are the variables we should include for subject level data:
#' 1. Unique Panelist ID
#' 2. Frequency_of_PB_Cheese
#' 3. Age
#' 4. Cheese_MR1
#' 5. Cheese_MR2
#' 
#' 
## ---------------------------------------------------------------------------------------------------------------
write.csv(df, "Subject_Level_Dairy_Cheese_latent_factors.csv")

