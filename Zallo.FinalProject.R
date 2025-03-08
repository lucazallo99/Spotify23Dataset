#Final Assignment 

####Importing and viewing dataset####
ds.spotify <- read.csv("C:/Users/lucaz/Downloads/archive (2)/spotify_2023.csv")
View(ds.spotify)
str(ds.spotify)
options(scipen=999)

##### DATA PREPROCESSING AND FEATURE ENGINEERING #####
ds.spotify$streams <- as.numeric(ds.spotify$streams)
head(ds.spotify)
ds.spotify$in_shazam_charts <- as.numeric(ds.spotify$in_shazam_charts)
ds.spotify$in_deezer_playlists <- as.numeric(ds.spotify$in_deezer_playlists)
ds.spotify$key <- as.factor(ds.spotify$key)
ds.spotify$mode <- as.factor(ds.spotify$mode)
colSums(is.na(ds.spotify))
ds.spotify$in_deezer_playlists <- NULL
ds.spotify$in_shazam_charts <- NULL
ds.spotify$streams[is.na(ds.spotify$streams)] <- mean(ds.spotify$streams, na.rm = TRUE)
str(ds.spotify)
summary(ds.spotify)
dim(ds.spotify)
new_column_names <- c("danceability", "valence", "energy", "acousticness", "instrumentalness", "liveness", "speechiness")
columns_to_rename <- c(16, 17, 18, 19, 20, 21, 22)
colnames(ds.spotify)[columns_to_rename] <- new_column_names
ds.spotify$instrumentalness <- NULL

##### EDA & DATA VISUALIZATION#####
#Displaying top 10 songs: Charts to be extended, view Marketing Research Class
library(ggplot2)
library(scales)  # For formatting numbers

# Sort data by streams and get top 10
sorted_data <- ds.spotify[order(-ds.spotify$streams), ]
top_ten <- head(sorted_data, 10)

# Create bar plot with formatted labels
ggplot(top_ten, aes(x = reorder(track_name, streams), y = streams, fill = streams)) +
  geom_bar(stat = "identity", width = 0.6, show.legend = FALSE) +  # Gradient color
  geom_text(aes(label = label_number(scale = 1e-9, suffix = "B")(streams)),  
            hjust = -0.1, size = 4) +  # Convert to billions and position text
  coord_flip() +  # Flip for readability
  theme_minimal() +  # Clean theme
  labs(title = "Top 10 Songs by Streams", x = "Track Name", y = "Streams (Billions)") +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Tilt x-axis labels
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")  # Center title
  ) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Gradient effect
  scale_y_continuous(labels = label_number(scale = 1e-9, suffix = "B"),  # Format y-axis labels
                     expand = expansion(mult = c(0, 0.1)))  # Adjust y-axis limits


#t-testing number of streams with other variables turned into categorical

#streams vs release year
ds.spotify$release_category <- cut(ds.spotify$released_year,
                                   breaks = c(-Inf, 2022, Inf),
                                   labels = c("Before 2023", "2023"),
                                   include.lowest = TRUE)
mean.streams <- mean(ds.spotify$streams)
sd.streams <- sd(ds.spotify$streams)
x.streams <- ds.spotify$streams
y.streams <- dnorm(x.streams,mean = mean.streams,sd = sd.streams)
plot(x.streams, y.streams)
ds.spotify$release_category <- as.factor(ds.spotify$release_category)
var.test(ds.spotify$streams ~ ds.spotify$release_category, ds.spotify)
t.test(ds.spotify$streams ~ ds.spotify$release_category, ds.spotify, var.equal=F)

#streams vs artist count
ds.spotify$artist_count_cat <- cut(ds.spotify$artist_count, breaks = c(-Inf, 1, Inf), labels = c("1", "More than 1"), include.lowest = TRUE)
ds.spotify$artist_count_cat <- as.factor(ds.spotify$artist_count_cat)
var.test(ds.spotify$streams ~ ds.spotify$artist_count_cat, ds.spotify)
t.test(ds.spotify$streams ~ ds.spotify$artist_count_cat, ds.spotify, var.equal=F)

#streams vs mode
var.test(ds.spotify$streams ~ ds.spotify$mode, ds.spotify)
t.test(ds.spotify$streams ~ ds.spotify$mode, ds.spotify, var.equal=F)


#ANOVA
#preparing data

# Create a factor with four levels representing the four seasons
ds.spotify$season <- cut(ds.spotify$released_month,
                         breaks = c(0, 3, 6, 9, 12),
                         labels = c("Winter", "Spring", "Summer", "Fall"),
                         include.lowest = TRUE)
ds.spotify$season <- as.factor(ds.spotify$season)

#streams vs season
aovseason <- aov(streams~season, ds.spotify)
anova(aovseason)
coef(aovseason)
confint(aovseason)
TukeyHSD(aovseason)

#stream vs bpm category
breaks <- seq(65, 206, length.out = 6)
ds.spotify$bpm_category <- cut(ds.spotify$bpm, breaks = breaks, labels = c("Low", "Moderate", "Medium", "Moderately High", "High"), include.lowest = TRUE)
ds.spotify$bpm_category <- as.factor(ds.spotify$bpm_category)
aovbpm <- aov(streams~bpm_category, ds.spotify)
anova(aovbpm)
coef(aovbpm)
confint(aovbpm)
TukeyHSD(aovbpm)

#streams vs danceability
breaks_danceability <- quantile(ds.spotify$danceability, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$danceability_category <- cut(ds.spotify$danceability, breaks = breaks_danceability, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$danceability_category <- as.factor(ds.spotify$danceability_category)
aovdanc <- aov(streams~danceability_category, ds.spotify)
anova(aovdanc)
coef(aovdanc)
confint(aovdanc)
TukeyHSD(aovdanc)

#streams vs valence
breaks_valence <- quantile(ds.spotify$valence, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$valence_category <- cut(ds.spotify$valence, breaks = breaks_valence, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$valence_category <- as.factor(ds.spotify$valence_category)
aovalen <- aov(streams~valence_category, ds.spotify)
anova(aovalen)
coef(aovalen)
confint(aovalen)
TukeyHSD(aovalen)

#streams vs energy
breaks_energy <- quantile(ds.spotify$energy, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$energy_category <- cut(ds.spotify$danceability, breaks = breaks_energy, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$energy_category <- as.factor(ds.spotify$energy_category)
aovdanc <- aov(streams~energy_category, ds.spotify)
anova(aovdanc)
coef(aovdanc)
confint(aovdanc)
TukeyHSD(aovdanc)

#streams vs acousticness
breaks_acousticness <- quantile(ds.spotify$acousticness, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$acousticness_category <- cut(ds.spotify$acousticness, breaks = breaks_acousticness, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$acousticness_category <- as.factor(ds.spotify$acousticness_category)
aovacoust <- aov(streams~acousticness_category, ds.spotify)
anova(aovacoust)
coef(aovacoust)
confint(aovacoust)
TukeyHSD(aovacoust)

#streams vs liveness
breaks_liveness <- quantile(ds.spotify$liveness, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$liveness_category <- cut(ds.spotify$liveness, breaks = breaks_liveness, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$liveness_category <- as.factor(ds.spotify$liveness_category)
aovliv <- aov(streams~liveness_category, ds.spotify)
anova(aovliv)
coef(aovliv)
confint(aovliv)
TukeyHSD(aovliv)

#streams vs speechiness
breaks_speechiness <- quantile(ds.spotify$speechiness,probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$speechiness_category <- cut(ds.spotify$speechiness, breaks = breaks_speechiness, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$speechiness_category <- as.factor(ds.spotify$speechiness_category)
aovspeech <- aov(streams~speechiness_category, ds.spotify)
anova(aovspeech)
coef(aovspeech)
confint(aovspeech)
TukeyHSD(aovspeech)

#Chi-square analysis
#regroup streams 
breaks_streams <- quantile(ds.spotify$streams,probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
ds.spotify$streams_category <- cut(ds.spotify$streams, breaks = breaks_streams, labels = c("Low", "Moderate", "Medium", "High"), include.lowest = TRUE)
ds.spotify$streams_category <- as.factor(ds.spotify$streams_category)
min_index <- which.min(ds.spotify$streams)
ds.spotify <- ds.spotify[-min_index, ]

#checking associations between categorical variables and residuals analysis 
#streams vs key 
chisq <- chisq.test(table(ds.spotify$streams_category, ds.spotify$key)) 
library(corrplot) 
corrplot(chisq$residuals, is.cor = FALSE)  

#streams vs season 
subset_before_2023 <- ds.spotify[ds.spotify$release_category == "Before 2023", ]
contingency_table <- table(subset_before_2023$streams_category, subset_before_2023$season)
chisq1 <- chisq.test(contingency_table)
chisq1
corrplot(chisq1$residuals, is.cor = FALSE) 

#streams vs danceability
chisq2 <- chisq.test((table(ds.spotify$streams_category, ds.spotify$danceability_category)) )
chisq2
corrplot(chisq2$residuals, is.cor = FALSE) 

#streams vs bpm category
chisq2 <- chisq.test((table(ds.spotify$streams_category, ds.spotify$danceability_category)) )
chisq2
corrplot(chisq2$residuals, is.cor = FALSE) 

#season vs bpm category
chisq3 <- chisq.test((table(ds.spotify$season, ds.spotify$bpm_category)) )
chisq3
corrplot(chisq3$residuals, is.cor = FALSE) 

#season vs danceability 
chisq4 <- chisq.test((table(ds.spotify$season, ds.spotify$danceability_category)) )
chisq4
corrplot(chisq4$residuals, is.cor = FALSE) 

#decades of 2000s vs musical variables
#release decade vs bpm category
ds.spotifydec <- data.frame(ds.spotify)
ds.spotifydec$release_decade <- as.factor(cut(ds.spotifydec$released_year,
                                        breaks = c(2000, 2010, 2020, 2023),
                                        labels = c("2000-2010", "2010-2020", "2020-2023"),
                                        include.lowest = TRUE))
chisq5 <- chisq.test((table(ds.spotifydec$release_decade, ds.spotifydec$bpm_category)) )
chisq5
corrplot(chisq5$residuals, is.cor = FALSE) 

#vs danceability
chisq6 <- chisq.test((table(ds.spotifydec$release_decade, ds.spotifydec$danceability_category)) )
chisq6
corrplot(chisq6$residuals, is.cor = FALSE) 

#vs mode
chisq7 <- chisq.test((table(ds.spotifydec$release_decade, ds.spotifydec$mode)) )
chisq7
corrplot(chisq7$residuals, is.cor = FALSE) 

#vs key
chisq8 <- chisq.test((table(ds.spotifydec$release_decade, ds.spotifydec$key)) )
chisq8
corrplot(chisq8$residuals, is.cor = FALSE) 

#vs acousticness
chisq9 <- chisq.test((table(ds.spotifydec$release_decade, ds.spotifydec$acousticness_category)) )
chisq9
corrplot(chisq9$residuals, is.cor = FALSE) 

#vs energy
chisq10 <- chisq.test((table(ds.spotifydec$release_decade, ds.spotifydec$energy_category)) )
chisq10
corrplot(chisq10$residuals, is.cor = FALSE) 

#LINEAR REGRESSION MODEL
#streams vs all musical variables
library(dplyr)
colnames(ds.spotify)
numerical_df <- ds.spotify %>%
  select_if(is.numeric)
numerical_df$released_day <- NULL
numerical_df$released_month <- NULL
numeric_cols <- sapply(numerical_df, is.numeric)
model <-lm(streams ~ ., data = numerical_df)
model
summary(model)

#multicollinearity
library(car)
numerical_df_mult = numerical_df
model1 <- lm(streams ~ ., data=numerical_df_mult)
model1
vif(model1)
correlation_matrix <- cor(numerical_df)
correlation_matrix
corrplot(correlation_matrix, type = "lower", method = "pie", diag = FALSE )

#RESIDUAL VS FITTED
autoplot(model)

#Stepwise Approach AIC
library(olsrr) 
model2 <- lm(streams ~ ., data = numerical_df) 
ols_step_both_aic(model2) 
ols_step_both_aic(model2, details = TRUE) 

#LOGISTIC REGRESSION
# Create the "Top100" column based on the ranking
ds.spotify$Top100 <- ifelse(rank(-ds.spotify$streams, ties.method = "min") <= 100, "Yes", "No")
ds.spotify$Top100 <- as.factor(ds.spotify$Top100)

set.seed(123)
sample <- sample.int(n = nrow(ds.spotify), size = floor(.70*nrow(ds.spotify)), replace = F)
trainset1 <- ds.spotify[sample, ]
testset1  <- ds.spotify[-sample, ]
dim(trainset1)
dim(testset1)

#Top 100 vs musical variables
fit_topmus <- glm(Top100 ~ bpm + mode + danceability + valence + energy + acousticness + liveness + speechiness, data = trainset1, family=binomial)
summary(fit_topmus)

#Top 100 vs playlist info variables 
fit_topplay <- glm(Top100 ~ in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts + in_deezer_charts, data = trainset1, family=binomial)
summary(fit_topplay)

#Top 100 vs release date
fit_toprelease <- glm(Top100 ~ released_year + released_month + released_day, data = trainset1, family=binomial)
summary(fit_toprelease)

#Top 100 vs all numerical variables
fit_topall <- glm(Top100 ~ artist_count + released_year + released_month + released_day + bpm + mode + danceability + valence + energy + acousticness + liveness + speechiness + in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts + in_deezer_charts, data = trainset1, family=binomial)
summary(fit_topall)

#anova 
anova_musplay <- anova(fit_topmus, fit_topplay, test = "Chisq")
anova_musrel <- anova(fit_topmus, fit_toprelease, test = "Chisq")
anova_musall <- anova(fit_topmus, fit_topall, test = "Chisq")
anova_playrel <- anova(fit_topplay, fit_toprelease, test = "Chisq")
anova_playall <- anova(fit_topplay, fit_topall, test = "Chisq")
anova_relall <- anova(fit_toprelease, fit_topall, test = "Chisq")

anova_musplay
anova_musrel
anova_musall
anova_playrel
anova_playall
anova_relall

#predictions
testset1$predmus <- predict(fit_topmus,testset1, type="response")
testset1$predplay <- predict(fit_topplay,testset1, type="response")
testset1$predrel <- predict(fit_toprelease,testset1, type="response")
testset1$predall <- predict(fit_topall,testset1, type="response")
testset1$predictmus <- rep("Yes", nrow(testset1))
testset1$predictplay <- rep("Yes", nrow(testset1))
testset1$predictrel <- rep("Yes", nrow(testset1))
testset1$predictall <- rep("Yes", nrow(testset1))
testset1$predictmus[testset1$predmus<0.5]<-"No"
testset1$predictplay[testset1$predplay<0.5]<-"No"
testset1$predictrel[testset1$predrel<0.5]<-"No"
testset1$predictall[testset1$predall<0.5]<-"No"
testset1$predictmus <- as.factor(testset1$predictmus)
testset1$predictmus <- factor(testset1$predictmus, levels = c("No", "Yes"))
testset1$predictplay <- as.factor(testset1$predictplay)
testset1$predictplay <- factor(testset1$predictplay, levels = c("No", "Yes"))
testset1$predictrel <- as.factor(testset1$predictrel)
testset1$predictrel <- factor(testset1$predictrel, levels = c("No", "Yes"))
testset1$predictall <- as.factor(testset1$predictall)
testset1$predictall <- factor(testset1$predictall, levels = c("No", "Yes"))
table(testset1$predictmus, testset1$Top100)
table(testset1$predictplay, testset1$Top100)
table(testset1$predictrel, testset1$Top100)
table(testset1$predictall, testset1$Top100)
mean(testset1$predictmus == testset1$Top100)
mean(testset1$predictplay == testset1$Top100)
mean(testset1$predictrel == testset1$Top100)
mean(testset1$predictall == testset1$Top100)

library(caret)

conf_matrix_mus <- confusionMatrix(testset1$predictmus, testset1$Top100, positive = 'Yes')
conf_matrix_play <- confusionMatrix(testset1$predictplay, testset1$Top100, positive = 'Yes')
conf_matrix_rel <- confusionMatrix(testset1$predictrel, testset1$Top100, positive = 'Yes')
conf_matrix_all <- confusionMatrix(testset1$predictall, testset1$Top100, positive = 'Yes')
conf_matrix_mus
conf_matrix_play
conf_matrix_rel
conf_matrix_all


#Use stepwise approach to find the “best” model 
library(tidyverse)
library(mlbench)
library(dplyr)

#Select the most contributive variables:
library(MASS)
step.model <- fit_topall %>% stepAIC(trace = FALSE)
coef(step.model)

#Predictions
probabilities <- predict(step.model, testset1, type = "response") 
predicted.classes <- ifelse(probabilities>0.5, "Yes", "No")
# Prediction accuracy
observed.classes <- testset1$Top100 
predicted.classes <- factor(predicted.classes, levels = levels(observed.classes))
observed.classes <- factor(testset1$Top100, levels = levels(predicted.classes))
mean(predicted.classes == observed.classes)
conf_matrix_step <- confusionMatrix(predicted.classes, observed.classes, positive = 'Yes')
conf_matrix_step

#Cross validation of the playlist model
# define training control
train_control <- trainControl(method = "cv", number = 10)

# train the model on training set
fitplay.cross <- train(Top100 ~ in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts + in_deezer_charts,
             data = trainset1,
             trControl = train_control,
             method = "glm",
             family=binomial())


predplaycross <-predict(fitplay.cross, newdata=testset1)

cm_1 <- confusionMatrix(table(as.factor(predplaycross), testset1$Top100), mode = "everything", positive = "Yes")
df_results <- data.frame('sensitivity' = c(as.numeric(cm_1$byClass)[1]),
                         'specificity' = c(as.numeric(cm_1$byClass)[2]),
                         'precision' = c(as.numeric(cm_1$byClass)[5]),
                         'recall' = c(as.numeric(cm_1$byClass)[6]),
                         'f1' = c(as.numeric(cm_1$byClass)[7]),
                         'balaced_accuracy' = c(as.numeric(cm_1$byClass)[11]),
                         'accuracy' = c(as.numeric(cm_1$overall)[1]))

cm_1
df_results

#Cross validation of the stepwise approach model
# define training control
train_control1 <- trainControl(method = "cv", number = 10)

# train the model on training set
stepcross <- train(Top100 ~ artist_count + valence + liveness + in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts,
             data = trainset1,
             trControl = train_control1,
             method = "glm",
             family=binomial())


predstepcross <-predict(stepcross, newdata=testset1)

cm_2 <- confusionMatrix(table(as.factor(predstepcross), testset1$Top100), mode = "everything", positive = "Yes")
df_results2 <- data.frame('sensitivity' = c(as.numeric(cm_2$byClass)[1]),
                         'specificity' = c(as.numeric(cm_2$byClass)[2]),
                         'precision' = c(as.numeric(cm_2$byClass)[5]),
                         'recall' = c(as.numeric(cm_2$byClass)[6]),
                         'f1' = c(as.numeric(cm_2$byClass)[7]),
                         'balaced_accuracy' = c(as.numeric(cm_2$byClass)[11]),
                         'accuracy' = c(as.numeric(cm_2$overall)[1]))

cm_2
df_results2
#First model is the best one 

#DECISION TREE AND RANDOM FOREST (key factors affecting streams and release date)
ds.spotify$Top100 <- NULL
set.seed(123)
sample <- sample.int(n = nrow(ds.spotify), size = floor(.70*nrow(ds.spotify)), replace = F)
trainset <- ds.spotify[sample, ]
testset  <- ds.spotify[-sample, ]
dim(trainset)
dim(testset)
streams.rp = rpart(streams_category ~ artist_count + released_year + released_month + released_day + bpm + mode + danceability + valence + energy + acousticness + liveness + speechiness + in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts + in_deezer_charts, data=trainset)
streams.rp

season.rp = rpart(season ~ bpm + mode + danceability + valence + energy + acousticness + liveness + speechiness, data=trainset)
season.rp

printcp(streams.rp)
plotcp(streams.rp)
summary(streams.rp)
printcp(season.rp)
plotcp(season.rp)
summary(season.rp)


#We can show the structure of the tree by plotting it.
plot(streams.rp)
text(streams.rp, all=TRUE, use.n = TRUE)
plot(season.rp)
text(season.rp, all=TRUE, use.n = TRUE)


streams.pred <- predict(streams.rp, testset, type="class")
season.pred <- predict(season.rp, testset, type="class")
#Then, we can create a classification table (Confusion Matrix)
table(testset$streams_category, streams.pred)
mean(testset$streams_category == streams.pred)
table(testset$season, season.pred)
mean(testset$season == season.pred)
#Not worth proceeding

#ACCURACY OF THE PREDICTION MODEL
positive_class <- "High"
confusionMatrix(table(streams.pred, testset$streams_category), mode = "everything", positive = positive_class)

#PRUNING THE TREE
min(streams.rp$cptable[,"xerror"])
xerr<-which.min(streams.rp$cptable[,"xerror"])
xerr
xcp = streams.rp$cptable[xerr,"CP"]
xcp
prune.tree1 <- prune(streams.rp, cp= xcp)
plot(prune.tree1)
text(prune.tree1, all=TRUE , use.n=TRUE)
#With the new model, in a similar vein we did above, we can generate a classification table based on the pruned tree model
streams.pred1 <- predict(prune.tree1, testset, type="class")
table(testset$streams_category, streams.pred1)
mean(testset$streams_category == streams.pred1)

#Or you use confusionMatrix():
confusionMatrix(table(streams.pred1, testset$streams_category), mode = "everything", positive = positive_class)

#Similar to the decision tree module above, we fit the random forest classifier with the training set
streams.rf = randomForest(streams_category ~ artist_count + released_year + released_month + released_day + bpm + mode + danceability + valence + energy + acousticness + liveness + speechiness + in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts + in_deezer_charts, data = trainset, importance = T)
streams.rf

streams.pred2 = predict(streams.rf, testset)
#Similarly, we can get the accuracy of the prediction
table(testset$streams_category, streams.pred2)
mean(testset$streams_category == streams.pred2)

#Or you use confusion Matrix():
confusionMatrix(table(streams.pred2, testset$streams_category), mode = "everything", positive = positive_class)

#Next we can plot the mean square error of the forest object to view the incremental improvement of the model along with the number of trees (by default we have 500).
plot(streams.rf)
importance(streams.rf)
varImpPlot(streams.rf)

#one last trial to check if rain forest performance improves by using only 3 categories for streams
num_quantiles <- 3
ds.spotify$streams_category2 <- cut(ds.spotify$streams, breaks = quantile(ds.spotify$streams, probs = seq(0, 1, 1/num_quantiles), na.rm = TRUE), labels = c("Low", "Medium", "High"), include.lowest = TRUE)
set.seed(123)
sample <- sample.int(n = nrow(ds.spotify), size = floor(.70*nrow(ds.spotify)), replace = F)
trainset <- ds.spotify[sample, ]
testset  <- ds.spotify[-sample, ]
dim(trainset)
dim(testset)
streams2.rf = randomForest(streams_category2 ~ artist_count + released_year + released_month + released_day + bpm + mode + danceability + valence + energy + acousticness + liveness + speechiness + in_spotify_playlists + in_spotify_charts + in_apple_playlists + in_apple_charts + in_deezer_charts, data = trainset, importance = T)
streams2.rf
streams2.pred = predict(streams2.rf, testset)
table(testset$streams_category2, streams2.pred)
mean(testset$streams_category2 == streams2.pred)
confusionMatrix(table(streams2.pred, testset$streams_category2), mode = "everything", positive = positive_class)
plot(streams2.rf)
importance(streams2.rf)
varImpPlot(streams2.rf)
#Better Model



