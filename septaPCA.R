#-- PCA -------------------------------------------------------------------------------------------
# install.packages("GGally")
# install.packages("ggridges")
library(tidyverse)
library(skimr)
library(GGally)
library(ggridges)

setwd("~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-")
location <- "~/Documents/_SCHOOL/_Drexel/STAT 642 - Data Mining/Assignments/Will-I-Be-Late-/data"

# Colorblind-friendly palette with grey:
cbGray <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbBlack <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## Step 1: Explore data
  data <- readRDS(paste(location,"model_data_dummies.RDS", sep="/"))
  data$delay <- as.numeric(data$delay)-1
  summary(data)

  scaled <- data.frame(scale(data))
  summary(scaled)

  ## Step 2: Define problem in terms of PCs

  # calculate correlation matrix for original variables
  xRho <- cor(scaled)

  # covariance matrix
  xSigma <- cov(scaled)

  xVar <- diag(xSigma)  # Variance of each variable
  pctXVar <- xVar/sum(xVar)  # proportion of total variance
  # pctXVar
    # Standardized, so equal variance

## Step 3: Compute eigenvalues & eigenvectors
  eigenvalues <- eigen(xSigma)$values     # variance accounted for by each PC
  eigenvectors <- eigen(xSigma)$vectors   # vectors of weights for each PC (columns are PCs)

  # define principal components
    y1 <- as.matrix(scaled) %*% (eigenvectors[,1])
    y2 <- as.matrix(scaled) %*% (eigenvectors[,2])
    # ...

    y <- as.matrix(scaled) %*% (eigenvectors)

## Step 4: double-check variances of PC vs. original and all 7 properties
  xVarSum <- sum(xVar)

  yVarSum <- sum(eigenvalues)
  pctYVar <- eigenvalues/sum(eigenvalues)

  # A: Variance of ith PC = ith Eigenvalue
    cat(var(y1),' = ',eigenvalues[1], sep = "")
    cat(var(y2),' = ',eigenvalues[2], sep = "")
    # ...

  # C: Sum of original vs PC variances is equivalent
    cat(xVarSum,' = ', yVarSum, sep = "")

  # E:  e_ik is proportional to correlation between Y_i and X_k
    eigenvectors[,1]
    cor(y1,scaled)  # proportional to above; == diag below
    eigenvectors[,1] * sqrt(eigenvalues[1]) / sqrt(diag(xVar))

  # F: With standardized variables, if eigenvalue < 1,
    # repeat with standardized variables [see below]

  # G: are they uncorrelated?
    ySigma <- var(y)
    yVar <- diag(ySigma)
    yRho <- cor(y)
    yRho  # Note: #.#####e-16 until 40th PC is effectively 0.  Thus, uncorrelated.



## Step 5: Visual plots & interpretation
  # How many PCs do we want?
    eigenvalues
    eigenvalues > 1 # identify values above 1
    # using eigenvalue > 1 heuristic, we'd use only 3 PCs!

  # scree plot for standardized variables (looking at first 10 PCs:
  ggdf <- as.data.frame(cbind(c(1:10), xVar[1:10], yVar[1:10]))
  ggplot(ggdf) +
    geom_point(aes(V1, V2), color = '#56B4E9') +
    geom_line(aes(V1, V2), color = '#56B4E9') +
    geom_point(aes(V1, V3), color = '#E69F00') +
    geom_line(aes(V1, V3), color = '#E69F00') +
    ggtitle("Scree Plot on Standardized Data") +
    xlab("Principal Components") +
    ylab("Variance") +
    theme_bw()
    # blue is original variables
    # orange is Principal components

  # using the scree plot, there's a nice elbow at 3 and 5

  # take a look at PC loadings
  index <- as.data.frame(t(eigenvectors[,1:5]))
  colnames(index) <- names(scaled)
  rownames(index) <- c(1:5)
  index
  write.csv(index, paste(location,"PCA loadings.csv",sep="/")) # to review

#-------------------------------
### Plot matrix crossing all PCs?
  # install.packages("GGally")
  library(GGally)

  ggdf <- as.data.frame(eigenvectors) # cols are PCs
  rownames(ggdf) <- colnames(data)
  colnames(ggdf) <- c(1:81)

  # pairwise plot the loadings of the first 7 PCs described above
  ggpairs(ggdf[,1:5], aes(alpha = 0.05),
          upper = list(continuous = "points", combo = "facetdensity"),
          lower = list(continuous = "blank", combo = "facetdensity"),
          title = "Principal Component Variable Loadings",
          xlab = "PCs 1 - 5", ylab = "PCs 1 - 5") +
    theme_bw()
  # this isn't super helpful since ggpairs does not allow us to set scales or
  # apply labels to points, but we can see how points seem to move/group by PC


#-------------------------------
### Relationship btwn delay and variables

### for weather data
library(package)
continuous <- cbind(data["delay"], data[,77:81])
corrplot(cor(continuous), tl.col = "black")


### for OTP data
require(reshape2)

# hours
ggdata <- cbind(data["delay"],data[,grep("hour", names(data), value=TRUE)])
ggdata$id <- 1:nrow(data)
ggdata <- melt(ggdata, c("id", "delay"))
head(ggdata)

hours <- ggplot(data = ggdata, aes(x = value)) +
  geom_density(aes(fill = factor(delay)), alpha = 0.6) +
  facet_wrap(~ variable, ncol = 6, scales = "free") +
  scale_fill_manual(name = "delay",
                    labels=c("No", "Yes"),
                    values = cbGray) +
  theme(legend.position = "bottom") +
  theme_bw()

# day
ggdata <- cbind(data["delay"],data[,grep("day", names(data), value=TRUE)])
ggdata$id <- 1:nrow(data)
ggdata <- melt(ggdata, c("id", "delay"))
head(ggdata)

days <- ggplot(data = ggdata, aes(x = value)) +
  geom_density(aes(fill = factor(delay)), alpha = 0.6) +
  facet_wrap(~ variable, ncol = 6, scales = "free") +
  scale_fill_manual(name = "delay",
                    labels=c("No", "Yes"),
                    values = cbGray) +
  theme(legend.position = "bottom") +
  theme_bw()

# month
ggdata <- cbind(data["delay"],data[,grep("month", names(data), value=TRUE)])
ggdata$id <- 1:nrow(data)
ggdata <- melt(ggdata, c("id", "delay"))
head(ggdata)

months <- ggplot(data = ggdata, aes(x = value)) +
  geom_density(aes(fill = factor(delay)), alpha = 0.6) +
  facet_wrap(~ variable, ncol = 6, scales = "free") +
  scale_fill_manual(name = "delay",
                    labels=c("No", "Yes"),
                    values = cbGray) +
  theme(legend.position = "bottom") +
  theme_bw()

# origin
ggdata <- cbind(data["delay"],data[,grep("origin", names(data), value=TRUE)])
ggdata$id <- 1:nrow(data)
ggdata <- melt(ggdata, c("id", "delay"))
head(ggdata)

origins <- ggplot(data = ggdata, aes(x = value)) +
  geom_density(aes(fill = factor(delay)), alpha = 0.6) +
  facet_wrap(~ variable, ncol = 6, scales = "free") +
  scale_fill_manual(name = "delay",
                    labels=c("No", "Yes"),
                    values = cbGray) +
  theme(legend.position = "bottom") +
  theme_bw()

# next
ggdata <- cbind(data["delay"],data[,grep("next", names(data), value=TRUE)])
ggdata$id <- 1:nrow(data)
ggdata <- melt(ggdata, c("id", "delay"))
head(ggdata)

nexts <- ggplot(data = ggdata, aes(x = value)) +
  geom_density(aes(fill = factor(delay)), alpha = 0.6) +
  facet_wrap(~ variable, ncol = 6, scales = "free") +
  scale_fill_manual(name = "delay",
                    labels=c("No", "Yes"),
                    values = cbGray) +
  theme(legend.position = "bottom") +
  theme_bw()

hours
days
months
origins
nexts
