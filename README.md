# Clusters of Economic Freedom over time

This project attempts to cluster countries based on their [Economic Freedom of the World](https://www.fraserinstitute.org/studies/economic-freedom-of-the-world-2019-annual-report) index scores in a way that each country's cluster membership is as stable as possible from one year to the next.

The approach is to apply kmeans clustering to one year (the latest year data is available for since it includes the most countries), then use the cluster centroids for calculating the next year. 
