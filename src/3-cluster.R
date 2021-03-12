# cluster analysis
library(mgcv)
library(tidyverse)
library(janitor)
library(cluster)

clusters_arg <- 6

# read
if (file.exists("./../outputs/parking_turnover.RData")) {
  print("All the necessary objects are being read into the environment.")
  for (file in list.files(path = "./../outputs", pattern="*.RData")){
    print(file)
    assign(gsub(".RData", "",file),
           readRDS(paste("./../outputs", file, sep = "/")))
  }

} else {
  print("Need to run the read and clean scripts...")
  source("./1-load.R")
  source("./2-clean.R")
  print("...all the necessary objects now available in environment.")
}

# PAM cluster analysis
df1 <- parking_turnover %>%
  select(marker_year, whour, count) %>%
  arrange(marker_year, whour)

ma1 <- table(df1$marker_year, df1$whour)

colnames(ma1) <- paste0("H", 0:167)

names(dimnames(ma1)) <- c("marker_year", "whour")

df2 <- pam(ma1, clusters_arg, metric='manhattan')

cluster_marker <- tibble(marker_year=row.names(ma1), cluster_number=df2$clustering)

saveRDS(cluster_marker, file = "../outputs/cluster_marker.RData")

df3 <- ma1 %>%
  as_tibble() %>%
  transmute(marker_year = marker_year, whour = whour, n = n) %>%
  arrange(marker_year)

cluster_count <- df3 %>%
  left_join(cluster_marker) %>%
  group_by(whour, cluster_number) %>%
  summarise(prob = mean(n), N = n()) %>%
  ungroup %>%
  mutate(cluster_number = paste0('Type ', cluster_number, ' (', N, ')'), whour = as.numeric(sub('H', '', whour)))

saveRDS(cluster_count, file = "../outputs/cluster_count.RData")

df4 = NULL

for(i in 1:clusters_arg) {
  df4 <- df3 %>%
    left_join(df5, by = "marker_year") %>%
    filter(cluster_number == i) %>%
    mutate(whour = as.numeric(sub("H", "", whour))) %>%
    gam(n ~ s(whour, bs = 'cc'), data=., knots=list(whour=c(0, 168))) %>%
    predict(newdata = data.frame(whour=0:168), se.fit=TRUE) %>%
    data.frame(whour = 0:168, intensity = .$fit, se=.$se.fit, cluster_number=i) %>%
    rbind(., df4)
}

cluster_intensity <- df4 %>%
  mutate(hi = intensity + 2 * se,
         lo = intensity - 2 * se,
         zero = 0,
         cluster = cluster_number,
         cluster_number = paste0('Cluster ', cluster_number)) %>%
  filter(whour < 168)

saveRDS(cluster_intensity, file = "../outputs/cluster_intensity.RData")