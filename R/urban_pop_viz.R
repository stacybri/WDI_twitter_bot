# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(tidyverse)
library(wbggeo)
library(wbgmaps)
library(ggthemes)
library(gganimate)
#others
library(here)
library(wbstats)
library(Hmisc)
library(ggthemes)
library(scales)
library(hrbrthemes)
gc()

#set working directory
dir <- here()

#read in twitter api info
source(paste(dir,'passwords.R',sep="/"), local=TRUE)

wdi_ind <- 'NY.GDP.MKTP.KD.ZG'

for (wdi_ind in c('SL.AGR.EMPL.ZS', 'SP.URB.TOTL.IN.ZS', 'SL.EMP.VULN.ZS')) {
gc()
# create token named "twitter_token"
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret,
  access_token = access_token,
  access_secret = access_secret)


#Post Tweet !
#post_tweet("This is a bot that posts figures and charts based on the World Bank's World Development Indicators database. This account is not an official product of the World Bank and does not reflect its views.")

###
# functions
###


#Now map the result
quality = "high"
maps <- wbgmaps::wbgmaps[[quality]]

country_metadata <- wb_countries()
indicator_metadata <- wb_indicators() %>%
  filter(source_id==2)



wdi_mapper  <- function(data, indicator, title,text) {
  
  
  
  
  map_df <- get(data)
  
  
    wdi_df <- wb_data(
      indicator=ind,
      country='countries_only',
      start_date=1991,
      end_date=2020,
      mrv=30,
      gapfill=TRUE,
      return_wide = F
    ) %>%
      mutate(value=if_else(!(abs(value - median(value, na.rm=T)) > 2*sd(value, na.rm=T)),value,as.numeric(NA))) %>%
      mutate(date=as.integer(date)) 
      
    
    p <- ggplot() +
      geom_map(data = wdi_df, aes(map_id = iso3c, fill = value, group=date), map = maps$countries) + 
      geom_polygon(data = maps$disputed, aes(long, lat, group = group), fill = "grey80") + 
      geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white")  +
      geom_path(data = maps$boundaries,
                aes(long, lat, group = group),
                color = "white",
                size = 0.1,
                lineend = maps$boundaries$lineend,
                linetype = maps$boundaries$linetype) +
      scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
      scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
      scale_fill_distiller(palette = "blues",
                           direction=1,
                           breaks = pretty_breaks(n = 7)
      )  +
      coord_equal() +
      theme_map(base_size=18) +
      labs(
        title=str_wrap(title,100),
        subtitle= "Year: {frame_time}",
        caption = paste('Source: World Bank World Development Indicators. ', ind,sep=""),
        fill='Value'
      ) +
      transition_time(date)
    
    
  
  
  
  
  
  tmp <- tempfile(fileext = ".png")
  
  if (rand_num!=5) {
    ggsave(tmp, p, width=10, height=8)
  } else if (rand_num==5) {
    
    animate(p, nframes=200, width=800, height=600 )
    anim_save(tmp)
    
  }
  
  post_tweet(status=text,
             media=tmp)
  
}



###
# Tweet
###

#open original database of tweets
final_tweet_df<- read_csv( paste(dir,'tweets_database.csv',sep="/"))

#choose 5 randomly selected indicators
indicators_selected_df <- indicator_metadata %>%
  filter(indicator_id==wdi_ind)

#population data
pop_df <- wb_data(
  indicator='SP.POP.TOTL',
  start_date=1961,
  end_date=2020,
  mrv=1,
  return_wide = F
) %>%
  mutate(population = value) %>%
  select(iso3c, population)



for (ind in (indicators_selected_df$indicator_id)) {
  
  row <- which(indicators_selected_df$indicator_id==ind) #get indicator position
  indicator_name <- indicators_selected_df$indicator[row] #get indicator name
  indicator_descript <- indicators_selected_df$indicator_desc[row] #get indicator description
  
  #get topics
  indicator_topic <- indicators_selected_df$topics[row][[1]] 
  
  if (nrow(indicator_topic)>0) {
    indicator_topic <- indicator_topic %>%
      mutate(value=str_remove_all(value," "),
             value=paste("#",value, sep=""))
  } else {
    indicator_topic <- ""
  }
  
  indicator_topic <- paste(indicator_topic$value, collapse = " ")
  
  
  title <- paste(indicator_name)
  text <- paste( indicator_descript,indicator_topic, '. Learn more at https://data.worldbank.org/indicator/',ind, sep="")
  
  #check if text is more than 280 characters
  if (nchar(text)>280) {
    text <- paste( title,indicator_topic, '. Learn more at https://data.worldbank.org/indicator/',ind, sep="")
  }
  
  #get data
  
  #pull data
  wdi_df <- wb_data(
    indicator=ind,
    start_date=2000,
    end_date=2020,
    mrv=1,
    return_wide = F
  ) %>%
    left_join(pop_df) %>%
    left_join(country_metadata) 
  
  
  #make sure popualation coverage at least 40%
  pop_cov <- wdi_df %>%
    mutate(total_pop=sum(population, na.rm=T)) %>%
    filter(!is.na(value)) %>%
    ungroup() %>%
    summarise(pop_cov=sum(population, na.rm=T)/max(total_pop))
  
  #randomly choose either a map, bar chart by region , or bar chart by income
  rand_num <- 5
  
  #make sure popualation coverage at least 40%
  
    
    wdi_mapper('wdi_df',ind,title,text )
    
    ####
    # save info to csv
    ####
    
    ## lookup status_id
    my_timeline <- get_timeline(rtweet:::home_user())
    
    ## ID for reply
    tweet_id <- my_timeline$status_id[1]
    
    tweet_df <- data.frame(
      
      indicator_id=ind,
      indicator_name=indicator_name,
      indicator_descript=indicator_descript,
      text=text,
      tweet_id=as.numeric(tweet_id),
      tweet_rand_num=rand_num
      
    )
    
    final_tweet_df <- final_tweet_df %>%
      bind_rows(tweet_df)
    
  
  
  
  
  
  
}

write_excel_csv(final_tweet_df, paste(dir,'tweets_database.csv',sep="/"))

## like tweets referencing World Development Indicators
# rt <- search_tweets("World Development Indicators")
# r <- lapply(rt$user_id, post_favorite)

}