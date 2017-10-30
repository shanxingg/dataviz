library(ggplot2)
movie <- read.csv("movie.csv")

# Does higher rating movie also have a higher adjusted gross revenue?
# data cleaning
movie_1 <- movie
movie_1$Adjusted_Gross2 <- as.numeric(gsub(",","",movie_1$Adjusted_Gross))
movie_1$MovieLens_Rating2 <- movie_1$MovieLens_Rating*2 # adjust MovieLens Rating to the same scale as IMDb's
boxplot(movie_1$Adjusted_Gross2, horizontal = TRUE) # use boxplot to understand the data
outliers <- boxplot(movie_1$Adjusted_Gross2, horizontal = TRUE)$out # identify outliers
outliers_index <- match(outliers, movie_1$Adjusted_Gross2) # find index of outliers
movie_1_no_outliers <- movie_1[-outliers_index,] # re-create a movie dataset without outliers
# graph
ggplot(movie_1_no_outliers, aes(y=Adjusted_Gross2))+
  geom_point(color="#CC79A7", aes(x=as.numeric(IMDb_Rating), shape="IMDb"))+
  geom_smooth(se=FALSE, method=lm, color="#CC79A7", aes(x=as.numeric(IMDb_Rating)))+
  geom_point(aes(x=as.numeric(MovieLens_Rating2), shape="MovieLens"), color="#56B4E9")+
  geom_smooth(aes(x=as.numeric(MovieLens_Rating2)), se=FALSE, method=lm, color="#56B4E9")+
  guides(shape=guide_legend("Rating Type", override.aes = list(color=c("#CC79A7", "#56B4E9"))))+
  scale_x_continuous(breaks = c(1:10))+
  scale_y_continuous(breaks = seq(0,1200,200))+
  labs(x="Rating", y="Adjusted Gross Revenue")+
  ggtitle("Adj. Gross Revenue VS. Rating")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))
# Compute correlation coefficient to comfirm result
cor(as.numeric(movie_1$IMDb_Rating), movie_1$Adjusted_Gross2) # correlation coefficient of IMDb_Rating
cor(as.numeric(movie_1$MovieLens_Rating), movie_1$Adjusted_Gross2) # correlation coefficient of MovieLens_Rating



# Shall we make a short movie or a long movie?
# Profit VS. Runtime_min
# data cleaning
movie_2 <- movie
movie_2$Profit2 <- as.numeric(gsub(",","",movie_2$Profit))
boxplot(movie_2$Profit2, horizontal = TRUE) # use boxplot to understand the data
outliers_2 <- boxplot(movie_2$Profit2, horizontal = TRUE)$out # identify outliers
outliers_index_2 <- match(outliers_2, movie_2$Profit2) # find index of outliers
movie_2_no_outliers <- movie_2[-outliers_index_2,] # re-create a movie dataset without outliers
# graph
ggplot(movie_2_no_outliers, aes(x=Runtime_min, y=Profit2))+
  geom_point(alpha=0.5)+
  geom_smooth(se=FALSE, color="#56B4E9", method=lm)+
  labs(x="Movie Run Time(min)", y="Profit")+
  ggtitle("Profit VS. Movie Run Time")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))
# Compute correlation coefficient to comfirm result
cor(movie_2$Runtime_min, movie_2$Profit2) # correlation coefficient

# Adj. Gross Revenue VS. Runtime_min
# data cleaning
movie_2_Revenue <- movie
movie_2_Revenue$Adjusted_Gross2 <- as.numeric(gsub(",","",movie_2_Revenue$Adjusted_Gross))
boxplot(movie_2_Revenue$Adjusted_Gross2, horizontal = TRUE) # use boxplot to understand the data
outliers_2 <- boxplot(movie_2_Revenue$Adjusted_Gross2, horizontal = TRUE)$out # identify outliers
outliers_index_2 <- match(outliers_2, movie_2_Revenue$Adjusted_Gross2) # find index of outliers
movie_2_no_outliers <- movie_2_Revenue[-outliers_index_2,] # re-create a movie dataset without outliers
# graph
ggplot(movie_2_no_outliers, aes(x=Runtime_min, y=Adjusted_Gross2))+
  geom_point(alpha=0.5)+
  geom_smooth(se=FALSE, color="#56B4E9", method=lm)+
  labs(x="Movie Run Time(min)", y="Adjusted Gross Revenue")+
  ggtitle("Adjusted Gross Revenue VS. Movie Run Time")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))
# Compute correlation coefficient to comfirm result
cor(movie_2_Revenue$Runtime_min, movie_2_Revenue$Adjusted_Gross2) # correlation coefficient

# Budget VS. Runtime_min
# data cleaning
movie_2_Budget <- movie
movie_2_Budget$Budget2 <- as.numeric(gsub(",","",movie_2_Budget$Budget))
boxplot(movie_2_Budget$Budget2, horizontal = TRUE) # use boxplot to understand the data
outliers_2 <- boxplot(movie_2_Budget$Budget2, horizontal = TRUE)$out # identify outliers
outliers_index_2 <- match(outliers_2, movie_2_Budget$Budget2) # find index of outliers
movie_2_no_outliers <- movie_2_Budget[-outliers_index_2,] # re-create a movie dataset without outliers
# graph
ggplot(movie_2_no_outliers, aes(x=Runtime_min, y=Budget2))+
  geom_point(alpha=0.5)+
  geom_smooth(se=FALSE, color="#56B4E9", method=lm)+
  labs(x="Movie Run Time(min)", y="Budget")+
  ggtitle("Budget VS. Movie Run Time")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))
# Compute correlation coefficient to comfirm result
cor(movie_2_Budget$Runtime_min, movie_2_Budget$Budget2) # correlation coefficient



# If a movie does well in US, does it also usually do well overseas?
# data cleaning
movie_3 <- movie
movie_3$US_rev2 <- as.numeric(gsub(",","",movie_3$US_rev))
movie_3$Overseas_rev2 <- as.numeric(gsub(",","",movie_3$Overseas_rev))
boxplot(movie_3$US_rev2, horizontal = TRUE) # use boxplot to understand the data
outliers_3_UC <- boxplot(movie_3$US_rev2, horizontal = TRUE)$out # identify outliers
boxplot(movie_3$Overseas_rev2, horizontal = TRUE) # use boxplot to understand the data
outliers_3_Overseas <- boxplot(movie_3$Overseas_rev2, horizontal = TRUE)$out # identify outliers
outliers_index_3<- unique(c(match(outliers_3_UC, movie_3$US_rev2), match(outliers_3_Overseas, movie_3$Overseas_rev2))) # find index of outliers
movie_3_no_outliers <- movie_3[-outliers_index_3,] # re-create a movie dataset without outliers
# graph
ggplot(movie_3_no_outliers, aes(x=US_rev2, y=Overseas_rev2))+
  geom_point(alpha=0.5)+
  geom_smooth(se=FALSE, color="#56B4E9", method=lm)+
  labs(x="US Revenue", y="Overseas Revenue")+
  ggtitle("Overseas Revenue VS. US Revenue")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))
# Compute correlation coefficient to comfirm result
cor(movie_3$US_rev2, movie_3$Overseas_rev2) # correlation coefficient


