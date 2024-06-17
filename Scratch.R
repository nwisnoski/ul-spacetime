library(dplyr)
class(b)
b_df <- t(as.data.frame(b))
target_value <- 1
b_fn <- colnames(b_df)[apply(b_df, 2, function(x) any(x %in% target_value))]
c <- colnames(b_df[,sapply(b_df, is.numric = 1)])

b_fn <- which(b_df$PA == 1 )


view(b_fn)
b<-((t(row_to_names(t(data_frame(OTU= colnames(soil.OTUs.DNA), PA= soil.OTUs.DNA[1,])),1)))
    
view(b)

#stupid graph that did not work

#INACTIVE SOIL OTUs
#use 

#group soil OTUs in RNA, DNA, always/never



UL.ts.RNA.Rel <- decostand(UL.ts.OTUs.RNA, "total")

#plot rel abundance 0.1-1
rel.ab.1 <- colnames(UL.ts.RNA.Rel, UL.ts.RNA.Rel> 0.1 )
#create object to base subsetting, then which fun for subsetting
#get an average rel abundance and then plot
plot(UL.ts.OTUs.RNA %in% rel.ab.1)



#Classifying Soil OTUs


soil.OTUs.RNA<- decostand(subset(UL.ts.OTUs.RNA, select = colnames(in.soils)), method = "pa")
soil.OTUs.DNA<- subset(UL.ts.OTUs.DNA.PA, select = colnames(in.soils))

a<- t(data_frame(OTU= colnames(soil.OTUs.RNA), PA= soil.OTUs.RNA[1,])) |> 
  row_to_names(1)
b<- decostand((t(row_to_names(t(data_frame(OTU= colnames(soil.OTUs.DNA), PA= soil.OTUs.DNA[1,])),1))), method = "pa")





#subsetting middle RG
middle<- c("RGD08", "RGD09", "RGD10", "RGD11", "RGc08", "RGc09", "RGc10", "RGc11")
RG.middle.OTU <-OTUs.REL[c("RGD08", "RGD09", "RGD10", "RGD11", "RGc08", "RGc09", "RGc10", "RGc11"),]
RG.middle.DNA <- as.data.frame(t(colMeans(RG.middle.OTU[str_which(rownames(RG.middle.OTU), "RGD"),])))
RG.middle.sb <- RG.middle.DNA[, which(RG.middle.DNA[1,] > 0 & RG.middle.DNA[1,] %in% RG.avg.ac)]
RG.avg.sb <- colnames(RG.middle.sb)
RG.middle.RNA <- as.data.frame(t(colMeans(RG.middle.OTU[str_which(rownames(RG.middle.OTU), "RGc"), ])))
RG.middle.Ac <- RG.middle.RNA[, which(RG.middle.RNA[1, ] > 0)]
RG.avg.ac<- colnames(RG.middle.Ac)

RG.avg.soil.ac <- RG.middle.Ac[, which(colnames(RG.middle.Ac) %in% soil.taxa)]
RG.avg.soil.sb<- RG.middle.sb[, which(colnames(RG.middle.sb) %in% soil.taxa)]
RG.middle.Ac[1, 1:10]

x<- colMeans(RG.middle.OTU[str_which(rownames(RG.middle.OTU), "RGc"), ])
v<- as.data.frame(x)
RG.middl
sb.OTUs <- colnames(which(OTUs) %in% !RG.avg.ac)
sb<- select(RG.middle.DNA %in% !RG.avg.ac)

a <- RG.middle.DNA |> filter(RG.middle.DNA[, 1] >0)
options(expressions = 90000)
zeroDNA<- RG.middle.DNA |> filter_all(all_vars(. == 0))

RG.middle.sb <- RG.middle.DNA |> 
  select_if(~all(. >0)) |>
  select(!RG.avg.ac) |> 
  filter(rowSums(.) == ncol(.))
  
RG.middle.sb <- RG.middle.DNA |> 
  select(where(~. >0)) |> 
  select(!matches(RG.avg.ac))


##classifying middle OTUs based on rel. abundance

RG.clsfd.ac <- RG.middle.Ac |>
  t() |> 
  as_data_frame() |> 
  rename(rel.ab = V1) |> 
  mutate(ab.class = if_else(rel.ab< exp(-10), 6, 
                            if_else(rel.ab < exp(-9), 5, 
                            if_else(rel.ab < exp(-8), 4, 
                                    if_else(rel.ab < exp(-6), 3, 
                                            if_else(rel.ab < exp(-4), 2, 
                                                    if_else(rel.ab < exp(-2), 1, 12)))))))


row.names(RG.clsfd.ac) <- names(RG.middle.Ac)


RG.clsfd.sb<- RG.middle.sb |>
  t() |> 
  as.data.frame() |> 
  rename(rel.ab = V1) |>
  mutate(ab.class = if_else(rel.ab< exp(-10), 6, 
                            if_else(rel.ab < exp(-9), 5, 
                            if_else(rel.ab < exp(-8), 4, 
                                    if_else(rel.ab < exp(-6), 3, 
                                            if_else(rel.ab < exp(-4), 2, 
                                                    if_else(rel.ab < exp(-2), 1, 12)))))))
row.names(RG.clsfd.sb) <- names(RG.middle.sb)
RG.clsfd.sb<- rownames_to_column(RG.clsfd.sb, var= "OTU")
  

row.names(RG.clsfd.sb) <- names(RG.middle.sb)
RG.clsfd.ac<- rownames_to_column(RG.clsfd.ac, var = "OTU")
RG.clsfd.sb<- rownames_to_column(RG.clsfd.sb, var = "OTU")
Ul.
a<- RG.clsfd.sb |>
  filter(OTU %in% soil.taxa) |>
  select(ab.class) |> 
  table()
b<- RG.clsfd.ac |>
  filter(OTU %in% soil.taxa) |>
  select(ab.class) |> 
  table()

result <- OTU.ts.long |> 
  filter(OTU %in% RG.avg.ac) |> 
  filter(OTU %in% soil.taxa) |> 
  left_join(RG.clsfd.ac, by = "OTU")

left_join(OTU.ts.long, RG.clsfd.ac, by = "OTU")


OTU.ts.long |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% common.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

OTU.ts.long |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% rare.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

OTU.ts.long |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% common.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

OTU.ts.long |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% rare.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)


#looking into seed bank dynamics

OTU.ts.long.sb<- UL.ts.OTUs.DNA |> 
  as_tibble() |> 
  rownames_to_column( var= "sample.id" ) |> 
  mutate(sample.id= as.integer(sample.id)) |> 
  pivot_longer(names_to = "OTU", values_to = "relative.abundance", cols = -sample.id) |> 
  left_join(env.ts.data, by= "sample.id" ) |> 
  filter(!OTU %in% UL.ts.OTUs.RNA)

OTU.ts.long.sb |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% common.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

OTU.ts.long.sb |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% rare.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

OTU.ts.long.sb |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% common.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

OTU.ts.long.sb |> filter(OTU %in% RG.avg.sb) |> 
  filter(OTU %in% rare.in.soils) |>
  left_join(RG.clsfd.sb, by = "OTU") |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU))+
  geom_line(alpha = 0.3)+
  scale_y_log10()+
  facet_wrap(~ab.class)

#making seedbank long form

SB.ts.OTUs <- colnames(SB.ts.OTUs.PA)[colSums(SB.ts.OTUs.PA) > 0]
UL.ts.OTUs.sb<- UL.ts.OTUs.DNA[, colnames(UL.ts.OTUs.DNA) %in% SB.ts.OTUs]


OTU.ts.long.sb<- UL.ts.OTUs.sb |>
  as_tibble() |> 
  rownames_to_column( var= "sample.id" ) |> 
  mutate(sample.id= as.integer(sample.id)) |> 
  pivot_longer(names_to = "OTU", values_to = "relative.abundance", cols = -sample.id) |> 
  left_join(env.ts.data, by= "sample.id" ) 


#attempting to get sorted RNA and DNA presence absences to recognise clusters

X<- data_frame(fraction= rowSums(UL.ts.OTUs.RNA.PA[,colSums(UL.ts.OTUs.RNA.PA)>0])/11824)
plot(x= rownames(X), y= X$fraction, type = "l")
X1<- data_frame(fraction= rowSums(UL.ts.OTUs.RNA.PA)/83343)
plot(x= rownames(X1), y= X1$fraction, type = "l")

Y<- data_frame(fraction= rowSums(UL.ts.OTUs.DNA.PA[,colSums(UL.ts.OTUs.DNA.PA)>0])/19399)
plot(x= rownames(Y), y= Y$fraction, type = "l")
Y1<- data_frame(fraction= rowSums(UL.ts.OTUs.DNA.PA)/83343)
plot(x= rownames(Y1), y= Y1$fraction, type = "l")

Z<- data_frame(fraction= rowSums(SB.ts.OTUs.PA[,colSums(SB.ts.OTUs.PA)>0])/12130)
plot(x= rownames(Z), y= Z$fraction, type = "l")
Z1<- data_frame(fraction= rowSums(SB.ts.OTUs.PA)/83343)
plot(x= rownames(Z1), y= Z1$fraction, type = "l")


Q<- t(UL.ts.OTUs.RNA.PA[ ,colnames(UL.ts.OTUs.RNA.PA) %in% RG.avg.ac])
Q2<- t(data_frame(UL.ts.OTUs.RNA.PA[,colSums(UL.ts.OTUs.RNA.PA)>0]))

dist_matrix <- vegdist(Q2, method = "jaccard")
# Assuming your distance matrix is named 'dist_matrix'

# Check for missing or infinite values
if (any(is.na(dist_matrix)) || any(is.infinite(dist_matrix))) {
  # Handle missing or infinite values
  dist_matrix[is.na(dist_matrix) | is.infinite(dist_matrix)] <- 0  # Replace with 0 or any other appropriate value
}

# Perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "complete")

# Assuming you want 3 clusters
clusters <- cutree(hclust_result, k = 5)
names(clusters) <- colnames(UL.ts.OTUs.RNA.PA[,colSums(UL.ts.OTUs.RNA.PA)>0])
# Assuming you want to plot a heatmap
heatmap(Q2, Colv = NA, Rowv = NA, col = c("white", "black"), scale = "none", 
        labRow = NA, labCol = NA, show_row_names = TRUE)

library(dplyr)

# Assuming 'clusters' is a vector of cluster assignments
cluster_fractions <- as.data.frame(Q) %>%
  mutate(cluster = clusters, fraction = colMeans() %>%
  group_by(cluster) %>%
  summarize(fraction_within_cluster = mean(fraction))
library(ggplot2)

# Create a dataframe for plotting
plot_data <- rbind(data.frame(cluster = "Overall", fraction = overall_fraction),
                   cluster_fractions)

# Plot the fractions
ggplot(plot_data, aes(x = cluster, y = fraction, fill = cluster)) +
  geom_bar(stat = "identity") +
  labs(x = "Cluster", y = "Fraction of Active OTUs") +
  ggtitle("Fraction of Active OTUs Overall and Within Each Cluster")




library(dplyr)

# Assuming your presence-absence matrix is named 'pa_matrix'
# Assuming your cluster assignments are stored in a vector named 'cluster_vector'

# Convert the presence-absence matrix to a dataframe
df <- as.data.frame(Q)

# Add the cluster assignment as a column in the dataframe
df$Cluster <- clusters

# Reshape the data from wide to long format
df_long <- df %>%
  rownames_to_column(var = "Timestamp") %>%
  pivot_longer(cols = -c(Timestamp, Cluster), names_to = "OTU", values_to = "Presence")

# Calculate the fraction of present OTUs for each timestamp and cluster
fraction_data <- df_long %>%
  group_by(Timestamp, Cluster) %>%
  summarize(Fraction = sum(Presence) / n())

# Create a line plot of the fraction of present OTUs over time, grouped by cluster
ggplot(fraction_data, aes(x = Timestamp, y = Fraction, color = as.factor(Cluster))) +
  geom_line() +
  labs(x = "Timestamp", y = "Fraction of Present OTUs") +
  ggtitle("Change in Fraction of Present OTUs over Time") +
  scale_color_discrete(name = "Cluster")


library(cluster)

# Create an empty table to store the cluster results
cluster_table <- data.frame(ID = 1:50, Cluster = NA)

for (k in 2:50) {
  # Calculate the distance matrix
  dist_matrix <- vegdist(Q2, method = "jaccard")
  
  # Check for missing or infinite values
  if (any(is.na(dist_matrix)) || any(is.infinite(dist_matrix))) {
    # Handle missing or infinite values
    dist_matrix[is.na(dist_matrix) | is.infinite(dist_matrix)] <- 0  # Replace with 0 or any other appropriate value
  }
  
  # Perform hierarchical clustering
  hclust_result <- hclust(dist_matrix, method = "complete")
  
  # Obtain the clusters for the current iteration
  clusters <- cutree(hclust_result, k = k)
  sill<- silhouette(clusters, dist = dist_matrix)
  
  # Assign cluster names to the corresponding IDs
  names(clusters) <- colnames(UL.ts.OTUs.RNA.PA[, colSums(UL.ts.OTUs.RNA.PA) > 0])
  
  # Store the cluster results in the table
  cluster_table$Cluster[k] <- summary(sill)$avg.width
}



library(dplyr)
library(ggplot2)

# Assuming your presence-absence matrix is named 'pa_matrix'
# Assuming your cluster assignments are stored in a data table named 'cluster_table' with columns 'ID' and 'Cluster'

# Convert the presence-absence matrix to a dataframe
df <- as.data.frame(Q2)

plot(cluster_table)

#for 18 clusters
  # Calculate the distance matrix
  dist_matrix <- vegdist(Q2, method = "jaccard")
  
  # Check for missing or infinite values
  if (any(is.na(dist_matrix)) || any(is.infinite(dist_matrix))) {
    # Handle missing or infinite values
    dist_matrix[is.na(dist_matrix) | is.infinite(dist_matrix)] <- 0  # Replace with 0 or any other appropriate value
  }
  
  # Perform hierarchical clustering
  hclust_result <- hclust(dist_matrix, method = "complete")
  
  # Obtain the clusters for the current iteration
  clusters <- cutree(hclust_result, k = 18)
  sill<- silhouette(clusters, dist = dist_matrix)
  
  # Assign cluster names to the corresponding IDs
  names(clusters) <- colnames(UL.ts.OTUs.RNA.PA[, colSums(UL.ts.OTUs.RNA.PA) > 0])
  
  # Store the cluster results in the table
  cluster_table$Cluster[k] <- summary(sill)$avg.width


# Iterate over cluster numbers

  # Subset the cluster assignment data for the current cluster
  cluster_data <- cluster_table[cluster_table$Cluster == 18, "ID"]
  
  # Convert cluster_data to character vector
  cluster_data <- as.character(cluster_data)
  
  # Check if cluster_data contains valid column names in df
  valid_columns <- intersect(cluster_data, colnames(df))
  
  if (length(valid_columns) > 0) {
    # Subset the data based on the cluster assignment
    cluster_subset <- df[, valid_columns]
    
    # Reshape the data from wide to long format
    df_long <- cluster_subset %>%
      rownames_to_column(var = "Timestamp") %>%
      pivot_longer(cols = -Timestamp, names_to = "OTU", values_to = "Presence")
    
    # Calculate the fraction of present OTUs for each timestamp and cluster
    fraction_data <- df_long %>%
      group_by(Timestamp) %>%
      summarize(Fraction = sum(Presence) / n())
    
    # Create a line plot of the fraction of present OTUs over time for the current cluster
    plot_title <- paste("Cluster", k)
    plot_filename <- file.path('".', paste("cluster_", k, ".png", sep = ""))  # File path with the plots directory
    
    p <- ggplot(fraction_data, aes(x = Timestamp, y = Fraction)) +
      geom_line() +
      labs(x = "Timestamp", y = "Fraction of Present OTUs") +
      ggtitle(plot_title)
    
    ggsave(filename = plot_filename, plot = p)
  }
}


#long RNA.PA

OTU.ts.long.pa<- UL.ts.OTUs.RNA.PA |> as_tibble() |>
  select(all_of(clusters.df$OTU)) |> 
  rownames_to_column( var= "sample.id" ) |> 
  mutate(sample.id= as.integer(sample.id)) |> 
  pivot_longer(names_to = "OTU", values_to = "pa", cols = -sample.id) |> 
  left_join(clusters.df, by = "OTU")


df<- as.data.frame(t(Q2))
names(df) <- colnames(UL.ts.OTUs.RNA.PA[, colSums(UL.ts.OTUs.RNA.PA) > 0])
df.1<- df |> select(any_of(clsfd.ac.1$OTU))
dist_matrix.1<- vegdist(t(df.1), method= "jaccard")


# Check for missing or infinite values
if (any(is.na(dist_matrix.1)) || any(is.infinite(dist_matrix.1))) {
  # Handle missing or infinite values
  dist_matrix[is.na(dist_matrix) | is.infinite(dist_matrix)] <- 0  # Replace with 0 or any other appropriate value
}

# Perform hierarchical clustering
hclust_result.1 <- hclust(dist_matrix.1, method = "complete")

# Obtain the clusters for the current iteration
clusters.1 <- cutree(hclust_result.1, k = 3)
sill.1<- silhouette(clusters.1, dist = dist_matrix.1)

# Assign cluster names to the corresponding IDs
names(clusters.1) <- colnames(select(df, any_of(clsfd.ac.1$OTU)))

summary(sill.1)$avg.width

clusters.df.1<- as.data.frame(clusters.1)
clusters.df.1$OTU<- rownames(clusters.df.1)

           
                                        

OTU.ts.long |> filter(OTU %in% colnames(df.1)) |> 
  left_join(clusters.df.1, by = "OTU") |>
  filter(clusters.1== 1) |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU ))+
  geom_line(alpha = 0.3)

OTU.ts.long |> filter(OTU %in% colnames(df.1)) |> 
  left_join(clusters.df.1, by = "OTU") |>
  filter(clusters.1== 2) |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU ))+
  geom_line(alpha = 0.3)

OTU.ts.long |> filter(OTU %in% colnames(df.1)) |> 
  left_join(clusters.df.1, by = "OTU") |>
  filter(clusters.1== 3) |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU ))+
  geom_line(alpha = 0.3)



#using dtw clust package

df<- as.data.frame(t(Q2))
names(df) <- colnames(UL.ts.OTUs.RNA.PA[, colSums(UL.ts.OTUs.RNA.PA) > 0])
df.1<- df |> select(any_of(clsfd.ac.1$OTU))




# Check for missing or infinite values
if (any(is.na(dist_matrix.1b)) || any(is.infinite(dist_matrix.1b))) {
  # Handle missing or infinite values
  dist_matrix[is.na(dist_matrix.1b) | is.infinite(dist_matrix.1b)] <- 0  # Replace with 0 or any other appropriate value
}

# Perform hierarchical clustering
clust_result.1b <- tsclust(t(df.1), distanced= "dtw_basic")

# Obtain the clusters for the current iteration
clusters.1 <- cutree(hclust_result.1, k = 3)
sill.1<- silhouette(clusters.1, dist = dist_matrix.1)

# Assign cluster names to the corresponding IDs
names(clusters.1) <- colnames(select(df, any_of(clsfd.ac.1$OTU)))

summary(sill.1)$avg.width

clusters.df.1<- as.data.frame(clusters.1)
clusters.df.1$OTU<- rownames(clusters.df.1)




OTU.ts.long |> filter(OTU %in% colnames(df.1)) |> 
  left_join(clusters.df.1, by = "OTU") |>
  filter(clusters.1== 1) |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU ))+
  geom_line(alpha = 0.3)

OTU.ts.long |> filter(OTU %in% colnames(df.1)) |> 
  left_join(clusters.df.1, by = "OTU") |>
  filter(clusters.1== 2) |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU ))+
  geom_line(alpha = 0.3)

OTU.ts.long |> filter(OTU %in% colnames(df.1)) |> 
  left_join(clusters.df.1, by = "OTU") |>
  filter(clusters.1== 3) |> 
  ggplot(aes(x= sample.id, y= relative.abundance, group = OTU ))+
  geom_line(alpha = 0.3)



#Cluster loop code

for (k in 2:50) {
  # Calculate the distance matrix
  dist_matrix <- vegdist(Q2, method = "jaccard")
  
  # Check for missing or infinite values
  if (any(is.na(dist_matrix)) || any(is.infinite(dist_matrix))) {
    # Handle missing or infinite values
    dist_matrix[is.na(dist_matrix) | is.infinite(dist_matrix)] <- 0  # Replace with 0 or any other appropriate value
  }
  
  # Perform hierarchical clustering
  hclust_result <- hclust(dist_matrix, method = "complete")
  
  # Obtain the clusters for the current iteration
  clusters <- cutree(hclust_result, k = k)
  sill<- silhouette(clusters, dist = dist_matrix)
  
  # Assign cluster names to the corresponding IDs
  names(clusters) <- colnames(UL.ts.OTUs.RNA.PA[, colSums(UL.ts.OTUs.RNA.PA) > 0])
  
  # Store the cluster results in the table
  cluster_table$Cluster[k] <- summary(sill)$avg.width
}


plot(cluster_table)

write.csv(cluster_table, file= "/Users/tj/git/ul-spacetime/cluster_table/clst.ts.rna.p.csv", row.names = FALSE)

assign("clst.ts.rna.p", read.csv("/Users/tj/git/ul-spacetime/cluster_table/clst.ts.rna.p.csv"))

plot(clst.ts.rna.p)
```


Plotting a cluster table to find best fits for patterns in RG.avg

```{r}
# Create an empty table to store the cluster results
cluster_table.avg <- data.frame(ID = 1:50, Cluster = NA)

Q<- t(UL.ts.OTUs.RNA.PA[ ,colnames(UL.ts.OTUs.RNA.PA) %in% RG.avg.ac])

for (k in 2:50) {
  # Calculate the distance matrix
  dist_matrix <- vegdist(Q, method = "jaccard")
  
  # Check for missing or infinite values
  if (any(is.na(dist_matrix)) || any(is.infinite(dist_matrix))) {
    # Handle missing or infinite values
    dist_matrix[is.na(dist_matrix) | is.infinite(dist_matrix)] <- 0  # Replace with 0 or any other appropriate value
  }
  
  # Perform hierarchical clustering
  hclust_result <- hclust(dist_matrix, method = "complete")
  
  # Obtain the clusters for the current iteration
  clusters <- cutree(hclust_result, k = k)
  sill<- silhouette(clusters, dist = dist_matrix)
  
  # Assign cluster names to the corresponding IDs
  names(clusters) <- colnames(UL.ts.OTUs.RNA.PA[, colSums(UL.ts.OTUs.RNA.PA) %in% RG.avg.ac])
  
  # Store the cluster results in the table
  cluster_table.avg$Cluster[k] <- summary(sill)$avg.width
}


plot(cluster_table.avg)

write.csv(cluster_table.avg, file= "/Users/tj/git/ul-spacetime/cluster_table/clst.ts.rna.avg.csv", row.names = FALSE)

assign("clst.ts.rna.p", read.csv("/Users/tj/git/ul-spacetime/cluster_table/clst.ts.rna.avg.csv"))

plot(clst.ts.rna.p)


#filtering transact data fractionally
RG.OTUs <- OTUs[str_which(rownames(OTUs), "RG"),]

# separate RNA and DNA dynamics
RG.OTUs.RNA <- RG.OTUs[str_which(rownames(RG.OTUs), "RGD"),]
RG.OTUs.DNA <- RG.OTUs[str_which(rownames(RG.OTUs), "RGc"),]

RG.OTUs.DNA.PA <- decostand(RG.OTUs.DNA + RG.OTUs.RNA, "pa")
RG.OTUs.RNA.PA <- decostand(RG.OTUs.RNA, "pa")

RG.1<- RG.OTUs.RNA.PA[1, RG.OTUs.RNA.PA[1,]==1, drop= FALSE]


for (k in 1:18) {
  
  # Create a dynamic variable name
  a <- paste("RG.", k, sep = "")
  
  b<- RG.OTUs.RNA.PA[k, RG.OTUs.RNA.PA[k, ] == 1, drop = FALSE]
  
  # Subset the data based on the dynamic variable name
  assign(a, b[, colnames(b) %in% soil.taxa])
  
}

name.3.2 <- clusters.3.2 |> filter(OTU %in% soil.taxa)

name.2.2 <- clusters.2.2 |> filter(OTU %in% soil.taxa)

sample(colnames(AAB), size = 25, replace = FALSE) #code for random sampling



#code to load and save an excel sheet as a tibble
install.packages("readxl")
library(readxl)
ul.dna.soilotus.always <- read_excel("/Users/tj/Desktop/Research/Microbial Ecology/sortedotus.xlsx", 
                                     sheet = "ul.dna.soilotus.always")

#soil classification code

RG.clsfd.ac <- RG.middle.Ac |>
  t() |>
  as_tibble() |> 
  rename(rel.ab = V1) |> 
  mutate(ab.class = if_else(rel.ab< exp(-10), 6, 
                            if_else(rel.ab < exp(-9), 5, 
                                    if_else(rel.ab < exp(-8), 4, 
                                            if_else(rel.ab < exp(-6), 3, 
                                                    if_else(rel.ab < exp(-4), 2, 
                                                            if_else(rel.ab < exp(-2), 1, 12)))))))




# trying to see %contribution separarey from rare and common

OTU.ts.long<- OTU.ts.long |> 
  mutate(abundance.type= if_else(relative.abundance >= 0.001, 1, 0))

always.common.abundance <- OTU.ts.long |> 
  filter(OTU %in% new.sorted.otu.always$OTU) |>
  filter(abundance.type == 1) |> 
  aggregate(relative.abundance ~ sample.id, 
                             FUN = sum)
always.rare.abundance <- OTU.ts.long |> 
  filter(OTU %in% new.sorted.otu.always$OTU) |>
  filter(abundance.type == 0) |> 
  aggregate(relative.abundance ~ sample.id, 
            FUN = sum)

aggregate.abundance.soil.otus <- always.abundance |> 
  rename(aggregate.abundance = relative.abundance) |>
  mutate(common.abundance = always.common.abundance$relative.abundance) |>
  left_join(always.rare.abundance, by = 'sample.id') |> 
  rename(rare.abundance = relative.abundance) |> 
  mutate(rare.abundance = replace_na(rare.abundance, 0))
