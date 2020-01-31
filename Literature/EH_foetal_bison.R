# Create tables and plots for foetal bison remains in
# Early Holocene contexts

# Ryan Breslawski, rbreslawski@smu.edu



# Load libraries
library(ggplot2)
library(patchwork)

# Import data
d <- read.csv("EH-foetal-bison.csv", header=T, stringsAsFactors=F)
d$Med.cal.age <- as.numeric(d$Med.cal.age)

# Each row in data frame d is a component. This chunk of code
# finds the number of sites represented in these components
# and prints the site names to the R console. It assumes that
# every multi-component site is represented in multiple rows,
# and components are separated from sites by a colon.
sites <- sapply(d$Component, function(x) strsplit(x, ":")[[1]][1])
sites <- unique(sites)
cat(paste("\n--", length(sites), "Sites --\n\n"))
for(i in sites) cat(paste(i, "\n"))

# Data subsets and convert character strings of ages to numeric
# values. This will convert "none" strings to NA values.
d_unfluted <- d[which(d$Point.style=="unfluted"), ]
d_10k <- d[which(d$Med.cal.age < 1e4), ]
d_11k <- d[which(d$Med.cal.age < 1.1e4), ]

# Data subsets for more recent excavations
d_recent <- d[which(d$Collection.issues=="no"),]
d_recent_unfluted <- d_recent[which(d_recent$Point.style=="unfluted"), ]
d_recent_10k <- d_recent[which(d_recent$Med.cal.age < 1e4), ]
d_recent_11k <- d_recent[which(d_recent$Med.cal.age < 1.1e4), ]

# Place datasets in list
d_list <- list(d=d, d_unfluted=d_unfluted, d_10k=d_10k, d_11k=d_11k,
               d_recent=d_recent, d_recent_unfluted=d_recent_unfluted,
               d_recent_10k=d_recent_10k, d_recent_11k=d_recent_11k)

# Loop through lists and export csv contingency tables
for(i in 1:length(d_list)){
  
  d_table <- table(d_list[[i]][,c(3, 4)])
  
  write.csv(d_table, file=paste0(names(d_list[i]), ".csv"))
  
}

# Create geometry for plotting the frequencies of components
bins <- cbind(seq(8e3, 13e3, by=500), seq(8.5e3, 13.5e3, by=500))
hists <- lapply(1:nrow(bins), function(x){
  
  ymax <- length(which(d$Med.cal.age >= bins[x,1] &
                         d$Med.cal.age < bins[x,2]))
  
  if(bins[x,1] < 10e3){
    period <- "EH"
  }else if(bins[x,1] < 1.1e4){
    period <- "PHT"
  }else{
    period <- "LP"
  }
  
  return(data.frame(ymin=0, ymax=ymax, 
                    xmin=bins[x,1], xmax=bins[x,2],
                    period=period))
  
})
hists <- do.call("rbind", hists)

# Create plots
freq_plot <- ggplot(hists, aes(ymin=ymin, ymax=ymax,
                           xmin=xmin, xmax=xmax))+
  geom_rect(aes(fill=period), color="grey", alpha=0.7)+
  scale_x_reverse(breaks=seq(14000, 8000, by=-1000))+
  scale_y_continuous(breaks=1:15)+
  labs(x="Median cal yr BP", y="n components")+
  scale_fill_manual(values=c("LP"="blue", "EH"="red", 
                             "PHT"="purple"))+
  theme(panel.background=element_blank(),
        panel.grid.major.y=element_line(color="grey"),
        panel.grid.minor.y=element_blank(),
        panel.grid.minor.x=element_line(color="grey"),
        panel.grid.major.x=element_line(color="grey"),
        legend.position="none")

ggsave("Bison_PHT_components.jpeg", plot=freq_plot, 
       device="jpeg", width=6, height=3, units="in",
       dpi=300)
