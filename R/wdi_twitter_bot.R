# load twitter library - the rtweet library is recommended now over twitteR
library(rtweet)
# plotting and pipes - tidyverse!
library(tidyverse)
library(wbggeo)
library(wbgmaps)
library(ggthemes)

#others
library(here)
library(wbstats)
library(Hmisc)
library(ggthemes)

gc()

#set working directory
dir <- here()

#read in twitter api info
source(paste(dir,'passwords.R',sep="/"), local=TRUE)



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
  
  #randomly choose either a map, bar chart by region , or bar chart by income
  rand_num <- sample(1:3, 1)
  
  if (rand_num==1) {
      p <- ggplot() +
        geom_map(data = map_df, aes(map_id = iso3c, fill = value), map = maps$countries) + 
        geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") + 
        geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white")  +
        geom_path(data = maps$boundaries,
                  aes(long, lat, group = group),
                  color = "white",
                  size = 0.1,
                  lineend = maps$boundaries$lineend,
                  linetype = maps$boundaries$linetype) +
        scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
        scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
        scale_fill_distiller(palette = "Blues",
                             direction=1
                             )  +
        coord_equal() +
        theme_map(base_size=12) +
        labs(
          title=str_wrap(title,100),
          subtitle= 'Data Point is for last year available',
          caption = paste('Source: World Bank World Development Indicators. ', ind,sep=""),
          fill='WDI Indicator Value'
    )
  } else if (rand_num==2) {
  #add histogram by region 
      p <- map_df %>%
        group_by(region) %>%
        filter(region!='Aggregates') %>%
        mutate(Value= wtd.mean(value, weights = population, na.rm=T),
               Label = paste(scales::comma(round(Value,1)))) %>%
        ggplot(aes(x=Value, y=region, fill=region)) +
        geom_bar(stat="identity",position='dodge') +
        geom_text(aes(label=Label)) +
        labs(
          title=str_wrap(paste(title, 'By Region', sep=" - "),100),
          caption = paste('Source: World Bank World Development Indicators. ', ind,sep=""),
          subtitle= 'Data Point is for last year available',
          fill='Region'
        ) +
        ylab('Region') +
        theme_bw() +
        theme(legend.position = 'top')
  } else if (rand_num==3) {
  
  #by income
    
  income <- c("Low income", "Lower middle income","Upper middle income","High income")
  
  p <- map_df %>%
    group_by(income_level) %>%
    filter(region!='Aggregates') %>%
    mutate(Value=wtd.mean(value, weights = population, na.rm=T),
           Label = paste(scales::comma(round(Value,1)))) %>%
    ggplot(aes(x=Value, y=income_level, fill=income_level)) +
    geom_bar(stat="identity",position='dodge') +
    geom_text(aes(label=Label)) +
    labs(
      title=str_wrap(paste(title, 'By Income', sep=" - "),100),
      caption = paste('Source: World Bank World Development Indicators. ', ind,sep=""),
      subtitle= 'Data Point is for last year available',
      fill="Income Group"
    ) +
    scale_y_discrete(limits = income) +
    ylab('Income') +
    theme_bw() +
    theme(legend.position = 'top')

  }

  
 
 
 tmp <- tempfile(fileext = ".png")
 ggsave(tmp, p, width=10, height=8)
 

 post_tweet(status=text,
            media=tmp)
 
}


indicators_selected_df <- indicator_metadata %>%
  sample_n(5)

#population data
pop_df <- wb_data(
  indicator='SP.POP.TOTL',
  start_date=2000,
  end_date=2020,
  mrv=1,
  return_wide = F
) %>%
  mutate(population = value) %>%
  select(iso3c, population)



for (ind in indicators_selected_df$indicator_id) {
  
  row <- which(indicators_selected_df$indicator_id==ind) #get indicator position
  indicator_name <- indicators_selected_df$indicator[row] #get indicator name
  indicator_descript <- indicators_selected_df$indicator_desc[row] #get indicator description
  
  title <- paste(indicator_name)
  text <- paste(indicator_descript, ' Learn more at https://data.worldbank.org/indicator/',ind, sep="")
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
    
  
  #make sure popualation coverage at least 40%
  if (pop_cov$pop_cov >= 0.4) {
    
    wdi_mapper('wdi_df',ind,title,text )
  
  }
  
}


tmp <- tempfile(fileext = ".png")


#write indicator to csv


