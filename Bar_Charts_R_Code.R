# Libraries needed

library(readxl)
library(ggplot2)
library(tidyverse)

# setting the working directory

setwd('C:/Users/Felipe/Dropbox/Studies/Certifications/IBM Data Science Professional Certificate/Module 9 - Capstone/Week_04')

df = read_xlsx('rio_grouped_num_main.xlsx')[-1]

# obtaining the labels to be used in the graphs
# in order not to pollute the labels with 137 observations, the strategy is
# to only plot the labels for every 10 observationds.

nghds = list()

for (i in 1:NROW(df)){
  if (i == 1){
    nghds[i] <- df$Neighborhood[i]
  }
  else if (i %% 10 == 0){
    nghds[i] <- df$Neighborhood[i]
  }
  else if (i == 137){
    nghds[i] <- df$Neighborhood[i]
  }
  else {
    nghds[i] <- ""
  }
}


lab_names = data.frame(nghds, stringAsFactors = F) %>% 
  t()  %>% `rownames<-`(NULL) 


# First, we need to gather the columns of venue type into one single column:

df_sum_tidy = gather(df,"Main_Venue",'Value',
                      -c("Neighborhood","Neighborhood Latitude","Neighborhood Longitude")) %>%
               arrange(.,Neighborhood)


# Ploting the Horizontal Stacked Bar Chart - Main Venue Type Percentage per Neighborhood

bar_mean = ggplot(data = df_sum_tidy, aes(y = Value, x = Neighborhood, 
                               fill = Main_Venue)) +
       geom_bar(data = df_sum_tidy, aes(y = Value, x = Neighborhood, fill = Main_Venue),
                              stat="identity", position = "fill") + 
        
       scale_x_discrete(labels = lab_names) +
       scale_y_continuous(labels = function(x) paste0(x*100, "%"))
  
bar_mean = bar_mean +
  
  theme(axis.ticks.x=element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", vjust = 0.75, hjust = 0.5),
        legend.title= element_text(color = "black", face = "bold")) +
  
  coord_flip()
  

bar_mean = bar_mean + 
                        scale_fill_discrete(name  ="Main Venue",
                                                    
                            breaks=c("arts_entertainment", "building",
                                                             "education","event",
                                                             "food","night","parks_outdoors",
                                                             "parks_outdoors","shops"),
                            labels=c("Arts and Entertainment", "Building",
                                                             "Education", "Event", "Food",
                                                             "Food", "Nightlife",
                                                             "Parks and Outdoors","Shops")) +
                            
                            labs(title = "Percentage of each Main Venue Type per Neighborhood") +
  
                            ylab("Percentage")


bar_mean

## Obtaining the Dodge Bar Graph with the Sum of Main Venues per Neighborhood


bar_sum = ggplot(data = df_sum_tidy, aes(y = Value, x = Neighborhood, 
                                           fill = Main_Venue)) +
  
  geom_bar(data = df_sum_tidy, aes(y = Value, x = Neighborhood, fill = Main_Venue),
           stat="identity", position = "dodge") + 
  
  scale_x_discrete(labels= lab_names) 

bar_sum = bar_sum +
  
  theme(axis.ticks.x=element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", vjust = 0.75, hjust = 0.5),
        legend.title= element_text(color = "black", face = "bold")) +
  
  coord_flip() 


bar_sum = bar_sum +  scale_fill_discrete(name  ="Main Venue",
                                          
                            breaks=c("arts_entertainment", "building",
                                  "education","event",
                                  "food","night","parks_outdoors",
                                  "parks_outdoors","shops"),
                            labels=c("Arts and Entertainment", "Building",
                                  "Education", "Event", "Food",
                                  "Food", "Nightlife",
                                  "Parks and Outdoors","Shops")) +
  
          labs(title = "Counts of Main Venue Type per Neighborhood") +

          ylab("Counts")
                    
bar_sum
bar_sum + scale_y_continuous(limits = c(0, 50))
