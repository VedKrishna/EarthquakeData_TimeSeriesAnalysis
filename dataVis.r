library(ggplot2)

# Set plot options
options(repr.plot.width=7, repr.plot.height=4)
#histogram for magnitude vs frequency
hist(df_eq$mag, breaks=30, main="", xlab="EQ magnitude", ylab="Frequency", col="skyblue")
# Add labels to the axes
xlabel("EQ magnitude", cex.lab=1.3)
ylabel("Frequency", cex.lab=1.3)



#barplot for depth vs frequency
barplot(df_eq$depth, main="", xlab="Records", ylab="EQ Depth", col="skyblue")
#histogram for depth
hist(df_eq$depth,breaks=30, main="", xlab="EQ magnitude", ylab="Frequency", col="skyblue")

#Histogram depicting the longitudes at which earthquake occur
hist(df_eq$longitude, breaks=30, main="", xlab="EQ longitude", ylab="Frequency", col="skyblue")
xlabel("EQ longitude", cex.lab=1.3)
ylabel("Frequency", cex.lab=1.3)

#Similarly for latitude
hist(df_eq$latitude, breaks=30, main="", xlab="EQ latitude", ylab="Frequency", col="skyblue")
xlabel("EQ latitude", cex.lab=1.3)
ylabel("Frequency", cex.lab=1.3)

