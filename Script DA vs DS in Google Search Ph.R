#Data Science vs Data Analytics, Popularity by Google Search in the Philippines
#by: Paolo G. Hilado
#Required packages
library(gtrendsR)
library(reshape2)
library(plyr)

#Getting Data from Google Trends
google.trends = gtrends(c("Data Science"), geo = "PH", gprop = "web", time = "all")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL
write.csv(google.trends, file = "Data Science Trends.csv")

google.trends = gtrends(c("Data Analysis"), geo = "PH", gprop = "web", time = "all")[[1]]
google.trends = dcast(google.trends, date ~ keyword + geo, value.var = "hits")
rownames(google.trends) = google.trends$date
google.trends$date = NULL
write.csv(google.trends, file = "Data Analysis Trends.csv")

#Cleaning and Tidying Data
a <- read.csv("Data Science Trends.csv")
b <- read.csv("Data Analysis Trends.csv")
colnames(a)[1] <- "Timeline"
colnames(b)[1] <- "Timeline"
a$Timeline <- as.Date(a$Timeline)
b$Timeline <- as.Date(b$Timeline)
p <- format(as.Date(a$Timeline, format - "%d/%m/%Y"), "%Y")
h <- format(as.Date(b$Timeline, format - "%d/%m/%Y"), "%Y")
a <- cbind(a, p)
b <- cbind(b, h)
colnames(a)[3] <- "Year"
colnames(b)[3] <- "Year"
x <- ddply(a, "Year", summarise, sum = sum(Data.Science_PH))
y <- ddply(b, "Year", summarise, sum = sum(Data.Analysis_PH))
x <- x[1:14,]
y <- y[1:14,]
colnames(x)[2] <- "Counts"
colnames(y)[2] <- "Counts"
c <- as.numeric(x$Year)
x <- cbind(x, c)
c <- as.numeric(x$Year)
y <- cbind(y, c)
Search <- rep(1, times = 14)
x <- cbind(x, Search)
Search <- rep(2, times = 14)
y <- cbind(y, Search)
g <- rbind(x, y)
g$Search <- as.factor(g$Search)
g$Search <- factor(g$Search,
levels = c("1", "2"),
labels = c("DS", "DA"))

#Plotting the line Graph
plot(g$c, g$Counts, type = "n", xlab = "Year", ylab = "Popularity", main = "Data Science vs Data Analytics Google Search in Ph", xaxt = "n", ylim = c(70,500))
with(subset(g, Search == "DS"), points(c, Counts, col = "darkorchid", type = "o", pch = 16))
with(subset(g, Search == "DA"), points(c, Counts, col = "deeppink", type = "o", pch = 16))
axis(1, at = 1:28, label = g$Year)
text(x = g$c, y = g$Counts, label = g$Counts, pos = 3, cex = 0.8 )
legend("bottomleft", pch = 15, col = "deepskyblue", legend = "Hilado,P.(2018)", cex = 0.6)
legend("topright", pch = 16, col = c("darkorchid", "deeppink"), legend = c("Data Science", "Data Analysis"))
