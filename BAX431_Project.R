library(ggplot2)
movie <- read.csv("movie.csv")
movie$Adjusted_Gross2 <- as.numeric(gsub(",","",movie$Adjusted_Gross))

# Does higher rating movie also have a higher adjusted gross revenue?
movie$MovieLens_Rating2 <- movie$MovieLens_Rating*2 # adjust MovieLens Rating to the same scale as IMDb's
boxplot(movie$Adjusted_Gross2, horizontal = TRUE) # use boxplot to understand the data
outliers <- boxplot(movie$Adjusted_Gross2, horizontal = TRUE)$out # identify outliers
outliers_index <- match(outliers, movie$Adjusted_Gross2) # find index of outliers
movie_no_outliers <- movie[-outliers_index,] # re-create a movie dataset without outliers
# graph
ggplot(movie_no_outliers, aes(x=as.numeric(IMDb_Rating), y=Adjusted_Gross2))+
  geom_point(color="#CC79A7", aes(shape="IMDb"))+
  geom_smooth(se=FALSE, method=lm, color="#CC79A7")+
  geom_point(aes(x=as.numeric(MovieLens_Rating2), shape="MovieLens"), color="#56B4E9")+
  geom_smooth(aes(x=as.numeric(MovieLens_Rating2)), se=FALSE, method=lm, color="#56B4E9")+
  guides(shape=guide_legend("Rating Type", override.aes = list(color=c("#CC79A7", "#56B4E9"))))+
  scale_x_continuous(breaks = c(1:10))+
  scale_y_continuous(breaks = seq(0,1200,200))+
  labs(x="Rating", y="Adjusted Gross Revenue")+
  ggtitle("Adj. Gross Revenue VS. Rating")+
  theme(plot.title = element_text(hjust=0.5, face="bold"))
# Compute correlation coefficient to comfirm result
cor(as.numeric(movie$IMDb_Rating), movie$Adjusted_Gross2) # correlation coefficient of IMDb_Rating
cor(as.numeric(movie$MovieLens_Rating), movie$Adjusted_Gross2) # correlation coefficient of MovieLens_Rating


# Shall we make a short movie or a long movie?








