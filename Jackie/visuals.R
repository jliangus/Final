#Basic Visualization
library(ggplot2)
countries = wine %>%
  group_by(country) %>%
  count() 

top_countries = countries %>%
  filter(n>500)

plot <- ggplot(countries, aes(reorder(country,n),n)) + 
  geom_bar(stat = "identity")] + coord_flip() + 
  labs(title = "Countries with at Least 500 Reviews by Number of Reviews", y = "Number of Reviews", x = "Country")
plot

plot2 <- ggplot(wine, aes(x=points)) + geom_histogram(binwidth = 1, color= 'white') + 
  coord_cartesian(xlim = c(75, 100)) +
  labs(title ="Review Distribution", x = "Review Score", y = "Number of Reviews") +
  scale_x_continuous(breaks=seq(75,100, by = 1))

plot2

plot3 <- ggplot(wine, aes(x=price)) + geom_histogram(binwidth = 5, color= 'white') + 
  coord_cartesian(xlim = c(0, 150)) +
  labs(title ="Wines under $150", x = "Price", y = "Count")
plot3



