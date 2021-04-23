
## Group G: Student Loans

# set up
## please add your required things here
library(tidyverse) # tidyr
#library(ggplot2)
library(ggridges)
library(shinydashboard)

library(magrittr)
library(stats)
library(manifestoR)
library(readtext)
library(SnowballC)
library(tidytext)      # for data manipulation
library(qdapDictionaries)
library(base64enc)
library(RColorBrewer)
library(ECharts2Shiny)
library(wesanderson)
library(rtweet)
library(ggmap)
library(sp)
library(leaflet)
library(tm)
library(htmlwidgets)
library(htmltools)
library(leaflet.extras)
library(zoo)
library(wesanderson)
library(lubridate)
library(tigris) #for polygon shape file
#devtools::install_version("wordcloud2", version = "0.2.0", repos = "https://cran.r-project.org")
library(wordcloud2)
library(XML)
library(RCurl)
library("readxl")
library(ggthemes)
library(ggrepel)
library(viridis)
library(hrbrthemes)
library(plotly)
library(RgoogleMaps)
library(tmap)
library(devtools)
library('scales')
library(DT)
library(highcharter)
library(treemap)
library(leaflet.providers)
library(quantmod)
library(treemapify)

#profvis({
library(shinydashboard)

## import data
load("data/repay_rate_ave_df.Rdata")
load("data/repay_rate_dist_df.Rdata")

## import data - Debt and College Scorecard 
load("data/sc_data.Rdata")

###Tweets Data
Tweets_state <- readRDS('data/Geotweets_state_cleaned.RDS') %>%
  mutate(id=row_number())
tweets_tidy <- readRDS("data/tweets_tidy.RDS")
tweets_tidy_wc <- readRDS("data/tweets_tidy_wc.RDS")

SearchTrend <- read_csv('data/SearchTrend.csv', col_names = TRUE, skip = 2) %>%
  rename(Total_Searches = 'student loan forgiveness: (United States)') %>%
  mutate(Month = zoo::as.yearmon(Month))

Schools <- read_csv('data/2010_2019_student_debt.csv') %>%
  select(INSTNM, UGDS,CITY,LATITUDE, LONGITUDE, STABBR, ADM_RATE, DEBT_MDN, Year_Ending) %>%
  filter(!is.na(ADM_RATE)) %>% subset(DEBT_MDN !='PrivacySuppressed') %>% 
  transform(DEBT_MDN = as.numeric(DEBT_MDN)) %>% 
  dplyr::mutate(DEBT_MDN = ifelse(is.na(DEBT_MDN), 0, DEBT_MDN)) %>% 
  mutate(DEBT_MDN_STUDENT = DEBT_MDN*UGDS)

sc <- Schools %>% filter(Year_Ending == 2019)%>%
  dplyr::mutate(uni_rank = case_when(
    ADM_RATE < 0.2 ~ 'highly selective/elite',
    ADM_RATE < 0.3 ~ 'more selective',
    ADM_RATE < 0.5 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>% mutate(uni_rank = factor(uni_rank, levels=c('not selective', 'less selective', 'selective', 'more selective', 'highly selective/elite')))

## save styles and themes
source("ourtheme.R")


### Plots ###

# Student Loans
## Arielle: Repayment Rates
### Figure 1: Repayment Rates Median by Class
repay_rate_ave <- ggplot(repay_rate_ave_df,
                         aes(x = cohort_year, y = rates, group = class, color = class)) +
  geom_line() + geom_point() +
  # labels
  ggtitle("Cohort Performance in First Year of Repayment") +
  xlab("\nFiscal Cohort Year") + ylab("Average Proportion of Fiscal Cohort\nto Decline Loan Balance\n") +
  labs(caption = "\n*Fiscal Cohort: group of students who begin repaying their loans in the same year") +
  scale_color_manual(name = "Family Income:", labels = c("High", "Medium", "Low"), values = GreenPal) +
  scale_y_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  ourtheme

### figure 2: repayment rate distributions
repay_rate_dist <- ggplot(repay_rate_dist_df, aes(x = rates, y = fct_rev(factor(cohort_year)), fill = class)) +
  ggridges::geom_density_ridges(scale = 2, alpha = 0.80) +
  
  # labels
  ggtitle("Proportion of University Fiscal Cohorts Whose Loan\nBalances Declined in Year One of Repayment\n") +
  ylab("Fiscal Cohort\n") + xlab(NULL) +
  scale_fill_brewer(name = "Family Income:", labels = c("High", "Middle", "Low"), palette = "Greens", direction = -1) +
  scale_x_continuous(labels = scales::dollar_format(suffix = "%", prefix = "")) +
  scale_x_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(reverse = TRUE)) +
  ourtheme


## Connie: Student Debts
### Figure 1: <table_1>
sc_dt <- sc %>% subset(DEBT_MDN !='PrivacySuppressed') %>% transform(DEBT_MDN = as.numeric(DEBT_MDN)) %>% group_by(uni_rank) %>% mutate(`Number of Universities` = n()) %>% ungroup() %>% mutate(DEBT_MDN_STUDENTS = DEBT_MDN*UGDS) %>% group_by(uni_rank) %>% mutate(`Median Student Loans` = paste('$',round(sum(DEBT_MDN_STUDENTS, na.rm=TRUE)/sum(UGDS, na.rm=TRUE),2))) %>%
  mutate(`Min Acceptance Rate` = percent(min(ADM_RATE))) %>% mutate(`Max Acceptance Rate` = percent(max(ADM_RATE))) %>% ungroup() %>%
  dplyr::mutate(`University Selectivity` = str_to_title(uni_rank)) %>% 
  group_by(`University Selectivity`,`Median Student Loans`,`Number of Universities`,`Min Acceptance Rate`,`Max Acceptance Rate`) %>%
  summarize() %>% 
  dplyr::mutate(Description=paste(`University Selectivity`,"\n",
                                  'Acceptance Range:',"\n",
                                  `Min Acceptance Rate`,'-',`Max Acceptance Rate`,"\n",
                                  `Number of Universities`,'Universities'), sep ="\n") 

#table_1 <- datatable(sc_dt,style = "default",filter = 'top',  caption = 'Universities and Selectivity')

### Figure 2: <treemap>
ShortPuBuGn <- c("#87BA8F","#008D70","#008AC6","#6271B3","#2F4858")

treemap <- ggplot(sc_dt, aes(area=`Number of Universities`, fill=`University Selectivity`, label=Description)) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", grow = F) + 
  scale_fill_manual(values=c("#2F4858","#008D70","#6271B3","#87BA8F","#008AC6")) +
  #elite,less selective,more selective,not selective,somewhat selective
  theme(text=element_text(size=16,  family="serif"),legend.position = "none")


  # treemap(index="Description",
  #         vSize="Number of Universities",
  #         type="index",
  #         fontsize.labels=c(12, 8),
  #         palette =  viridis(5),
  #         border.col="white",
  #         title = 'Universities and Selectivity')

### Figure 3: <admissions_scatter>
admissions_scatter <- sc %>% subset(DEBT_MDN !='PrivacySuppressed') %>% transform(DEBT_MDN = as.numeric(DEBT_MDN)) %>%
  ggplot(., aes(x=ADM_RATE, y=DEBT_MDN,color=uni_rank)) +
  geom_point(pch=21) +
  geom_smooth(color='navy', se = FALSE) +
  scale_color_manual(values=ShortPuBuGn)+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background= element_rect(fill="white")) +
  ylim(-100,30000) + xlim(-0.1,1) +
  scale_y_discrete(limits=c(0,10000,20000,30000), labels=c('0','10','20','30')) +
  scale_x_continuous(labels = percent)+
  labs(x='Admissions Rate', y='Median Loan Amount per Student\n(thousands)',
       title='Student Debt and Admissions Rate (2019)',
       color='Selectivity')
### Figure 4: <plot_line_plotly>
# CPI Inflation Rates - Got Average Yearly Inflation Rate for Scaling for Student Debt 
quantmod::getSymbols("CPIAUCSL", src='FRED')
avg.cpi <- apply.yearly(CPIAUCSL, mean)
cf <- as.data.frame(avg.cpi/as.numeric(avg.cpi['2009'])) 
cf$Year_Ending <- format(as.Date(row.names(cf), format="%Y-%m-%d"),"%Y")
# Merged for Inflation 
sc_time_df <- Schools %>% group_by(`Year_Ending`) %>% mutate(`Average Annual Student Debt - National` = sum(DEBT_MDN_STUDENT,na.rm=TRUE)/sum(UGDS,na.rm=TRUE)) %>% ungroup() %>% 
  dplyr::mutate(uni_rank = case_when(
    ADM_RATE < 0.2 ~ 'elite/highly selective',
    ADM_RATE < 0.3 ~ 'more selective',
    ADM_RATE < 0.5 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>%
  mutate(uni_rank = factor(uni_rank, levels=c('not selective', 'less selective', 'selective', 
                                              'more selective', 'elite/highly selective'))) %>%
  group_by(uni_rank,Year_Ending) %>% 
  mutate(`Average Annual Student Debt (by Selectivity)` = sum(DEBT_MDN_STUDENT,na.rm=TRUE)/sum(UGDS,na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(`Year_Ending`,`Average Annual Student Debt (by Selectivity)`,
           uni_rank,`Average Annual Student Debt - National`) %>% summarize() %>% 
  merge(cf) %>% 
  mutate(`Adjusted Average Annual Student Debt` = `Average Annual Student Debt (by Selectivity)`/
           CPIAUCSL) %>% 
  mutate(`Adjusted Average Annual Student Debt - Composite` = `Average Annual Student Debt - National`/
           CPIAUCSL)
#More data manipulation
sc_df <- sc_time_df %>% group_by(`Average Annual Student Debt - National`,`Adjusted Average Annual Student Debt - Composite`,Year_Ending) %>% summarize() %>% mutate(uni_rank='national average') %>% mutate(`Adjusted Average Annual Student Debt`=`Adjusted Average Annual Student Debt - Composite`) %>% dplyr::mutate(`Average Annual Student Debt (by Selectivity)` = `Average Annual Student Debt - National`) %>% merge(cf) %>% select(Year_Ending,`Average Annual Student Debt (by Selectivity)`, uni_rank, `Average Annual Student Debt - National`, CPIAUCSL, `Adjusted Average Annual Student Debt`,`Adjusted Average Annual Student Debt - Composite`)
sc_time_df <- sc_time_df %>% rbind(sc_df) %>% mutate(uni_rank = factor(uni_rank, levels=c('national average','not selective', 'less selective', 'selective', 'more selective', 'elite/highly selective'))) %>% 
  mutate(`Group Level` = ifelse(uni_rank == 'national average', 'National','Selectivity'))
#Name of Static Graph 
plot_line <- sc_time_df %>% 
  ggplot(.,aes(x=Year_Ending,y=`Adjusted Average Annual Student Debt`, color=uni_rank)) + 
  geom_line(aes(linetype=`Group Level`)) + 
  scale_color_manual(values=c('grey',"#87BA8F","#008D70","#008AC6","#6271B3","#2F4858"))+
  scale_linetype_manual(values=c("dotted", "solid"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background= element_rect(fill="white")) +
  scale_x_continuous(breaks = round(seq(min(Schools$Year_Ending), max(Schools$Year_Ending), by = 2),1)) +
  labs(x='', y='Inflation-Adjusted Median Loan Amount per Student\n(thousands)', 
       title='Student Debt Has Been Rising Over The Years',
       color='',fill='',group='',linetype='')
#Name of Plotly Graph
plot_line_plotly <- ggplotly(plot_line)
### Figure 5: <interactive_chloropleth>
# <put code here>

# Tweets
## Grace: Tweets and stuff
### Figure 1: <Interest in #StudentLoanForgiveness over Time>

Trend <- ggplot(SearchTrend,aes(x=Month, y=Total_Searches,group=1)) +
  geom_line(color = "#68A982") +
  scale_color_manual(values="#F2C65F")+
  xlab("") +
  ylab("Total Searches") +
  labs(title="Interest in #StudentLoanForgiveness over Time",caption = "Source: Google Trend Search") +
  theme(text=element_text(size=12,  family="serif"),
        panel.background= element_rect(fill="white"),
        legend.position = "bottom",
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5))

### Figure 2: <Tweets Keywords>
# Create a wordcloud with wesanderson palette
## load preprocessed data
Twitter_wd <- wordcloud2(tweets_tidy_wc,
                         color = wes_palette(name="Royal2"),
                         fontFamily = "serif")

### Figure 3: <#CancelStudentDebt Tweets in the US - Location of Selective Institutions>

#Get list of top words by state
State_top_words <- tweets_tidy %>%
  select(term, count, state) %>%
  group_by(state, term) %>%
  summarise(total=sum(count)) %>%
  arrange(desc(total)) %>%
  filter(! term %in% c("student","loan","debt","cancelstudentdebt","amp")) %>%
  group_by(state) %>%
  slice_max(order_by = total, n=15) %>%
  select(-total) %>%
  summarise(Terms = list(term))

# Get Bing lexicon
bing <- get_sentiments("bing")

# Join text to lexicon
Tweets_bing <- inner_join(tweets_tidy, bing, by = c("term" = "word")) %>%
   # Count by sentiment, index, document
  count(sentiment,index,document, text) %>%
   # Spread sentiments
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive-negative)

# Join sentiment score with original twitter data fame
Tweets_sentiment <- Tweets_state %>%
  inner_join(Tweets_bing, by = c("id" = "index")) %>%
  select(-c(text.y,document, size=favorite_count))

# Student Loan Forgiveness & Selective Institutions Map
# count by state
tw_state <- Tweets_sentiment %>%
  mutate(state = ifelse(state=="new york:long island","new york",
                        ifelse(state=="new york:main","new york",
                        ifelse(state=="new york:main","new york",
                        ifelse(state=="new york:manhattan","new york",
                        ifelse(state=="north carolina:main","north carolina",
                        ifelse(state=="massachusetts:main","massachusetts",
                        ifelse(state=="michigan:north","michigan",
                        ifelse(state=="michigan:south","michigan",
                        ifelse(state=="virginia:main", "virginia",
                        ifelse(state=="washington:main","washington",state))))))))))) %>%
  group_by(state) %>%
  mutate(N=n(), T.Sentiment=sum(sentiment), WordCounts = sum(display_text_width)) %>%   #get total number of tweets & sum of sentiment score by state
  select(display_text_width, lon, lat, state, T.Sentiment, N, WordCounts) %>%  #lon & lat are vary within each state.Keep first one.
  group_by(state, lon, lat) %>%
  summarise(Avg.Sentiment=T.Sentiment/N, WordCounts=WordCounts) %>% #calculate average sentiment score by state and keep total word counts by state
  ungroup(lon,lat) %>%
  filter(row_number()==1) %>%
  mutate(lon=ifelse(state=="new york",-73.935242, lon)) %>%  #change coordinates for New York State
  mutate(lat=ifelse(state=="new york",40.730610,lat)) %>%
  filter(!is.na(state)) %>%
  left_join(State_top_words, by="state")

# Polygon stuff from shape file
states <- states(cb=T) %>%
  mutate(name = tolower(NAME))

# Use the Tigris function geo_join to bring together the states shapefile and the tw_states dataframe
states_shape_tweets <- geo_join(states, tw_state, "name", "state")

#Get Locations of Selective Schools
SelSchools <-Schools %>%
  mutate(Selectivity = case_when(
    ADM_RATE < 0.1 ~ 'elite',
    ADM_RATE < 0.2 ~ 'highly selective',
    ADM_RATE < 0.3 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>%
  filter(Selectivity %in% c('elite', 'highly selective','selective'), !is.na(LONGITUDE)) %>%
  distinct(INSTNM, .keep_all=TRUE)

#Twitter Sentiment Map & Institutions'Location Map

# Creating a color palette based on the number range in the total column
ppal <- colorNumeric("RdYlGn", domain=states_shape_tweets$Avg.Sentiment)

# Creating a color palette based on Selectivity of schools
ppal2 <- colorFactor(palette = c("purple", "forestgreen", "deepskyblue3"),
               levels = c("elite", "highly selective", "selective"))

icon.pop <- pulseIcons(color = ifelse(SelSchools$Selectivity == "elite", "#E64141",
                                      ifelse(SelSchools$Selectivity =="highly selective", "#F9C874", "#97D6FF")))

# Setting up the pop up text
popup_sb <- paste0("State: ", as.character(states_shape_tweets$NAME),"<br/>",
                  "Average Sentiment Score: ", as.character(states_shape_tweets$Avg.Sentiment),"<br/>",
                  "Total Word Count: ",as.character(states_shape_tweets$WordCounts),"<br/>",
                  "Top Words: ",as.character(states_shape_tweets$Terms),"<br/>")

popup_sb2 <- paste0("University: ", as.character(SelSchools$INSTNM),"<br/>",
                  "City: ", as.character(SelSchools$CITY),"<br/>",
                  "Admission Rate: ",as.character(SelSchools$ADM_RATE))

#Setting up color for awesome markers
getColor <- function(SelSchools) {
  sapply(SelSchools$ADM_RATE, function(ADM_RATE) {
  if(ADM_RATE <= 0.1) {
    "purple"
  } else if(ADM_RATE <= 0.2) {
    "green"
  } else {
    "blue"
  } })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(SelSchools)
)


# Map title
map_title2 <- tags$p(tags$style('p {color: black; font-size: 14px; family: serif}'),
                    tags$b('#CancelStudentDebt Tweets in the US - Location of Selective Institutions'))

#Build Tweets Map
TweetsMap <- leaflet() %>%
  setView(-98.483330, 38.712046, zoom = 4) %>%
  addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
    attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>', group = "Dark") %>%
#Overlay Group
  addAwesomeMarkers(data = SelSchools,
                    lng = ~LONGITUDE, lat = ~ LATITUDE,
                    icon= icons,
                    popup = popup_sb2,
                    group = 'Selective Institutions') %>%
  leaflet::addLegend(position = 'bottomright',
            title = 'Selectivity',
            pal = ppal2,
            values = SelSchools$Selectivity,
            opacity = 0.7) %>%
  addCircles(data= states_shape_tweets,
             lng = ~lon, lat = ~lat,
             weight = 2,
             radius = states_shape_tweets$WordCounts*2.8,
             popup=~popup_sb,
             color=~ppal(states_shape_tweets$Avg.Sentiment),
             stroke = TRUE,
             group = 'Twitter Sentiments',
             fillOpacity = 1) %>%
  leaflet::addLegend(position = "bottomleft",
            title = "Average Sentiment Score",
            pal = ppal,
            values = states_shape_tweets$Avg.Sentiment,
            opacity = 0.7) %>%
  addPolylines(data = states_shape_tweets, color = "lightgrey", opacity = 0.2, weight = 2) %>%
  addLayersControl(
          overlayGroups =c("Twitter Sentiments", "Selective Institutions"),
          options = layersControlOptions(collapsed=FALSE),
          position = "bottomright") %>%
  #Add Map Title
  addControl(map_title2,
             position = 'topright') %>%
  addResetMapButton()

### UI ###
ui <- shinydashboard::dashboardPage(
  
  shinydashboard::dashboardHeader(title = "Student Debt"),
  
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      menuItem("About", tabName = "about"),
      menuItem("Student Loans", tabName = "loans"),
      menuItem("What do people think?", tabName = "tweets"),
      menuItem("Where do people think it?", tabName = "tweetmap")
    )
  ),
  
  shinydashboard::dashboardBody(
    tabItems(
      tabItem(tabName = "about", class = "active",
              fluidPage(
                tags$div(
                  tags$h1("Project Motivation"),
                  column(12,p("In March 2021, President Biden announced a federal student loan cancellation program of over $2 billion dollars. 
                  For much of the American public, this announcement reignited interest and discourse around the state of student debt 
                  in America. Even as some cry that this loan forgiveness program was an excessive and unnecessary government expense, 
                  more and more people are pushing for universal student loan forgiveness. Based on the relevance of this topic, our team wished to  
                  better understand this topic from both a historical and a socioeconomic perspective, with the hope that this context could help
                  to shed light on the contemporary discourse around  student loan forgiveness policy.")),
                    
                  column(12,p("This project explores how student debt and repayment interacts with socioeconomic conditions and university prestige. As this was a fairly broad topic,
                  we did a decent amount of data exploration before zooming in on our research focus. The project involves three parts. First, we focused 
                  on specific aspects of the student loan issue including university selectivity, family income, repayment rate over time, and looked at  the regional 
                  differences in average debt at the university level to understand why student debt has become a more and more serious problem in the US. 
                  We noted that while the overall amount of individuals repaying their loans was increasing, this increase was outpaced by the increase in 
                  average student debt amount undertaken by college students. Additionally, we noted that repayment rate and median debt amount were not by any means 
                  evenly distributed across the nation - individuals in less selective schools appear to be incurring slightly higher amounts of student debt on average, 
                  and individuals from lower income brackets were disparately unable to make progress on their loans within the first year of repayment.")),
                    
                  column(12,p("After looking at the historical data, we also sought to understand the current discussion on student loan forgiveness in light of the Biden 
                  administration's recent announcement of a $1 billion cancellation in student loans. Specifically, we looked at the tweets keywords and 
                  examined the overlap between states most concerned about student debt and where the selective schools are located in the US, and were somewhat unsurprised 
                  to find the existence of such an overlap in these areas (despite the fact that more students from more selective schools incur
                  lower levels of student debt on average.")),
                  
                  tags$h1("Sources"),
                  "For our project, we looked at data from the following sources:
                  (1) Scorecard, (2) Urban Institute Education Data, (3) Twitter API, and (4) Google Trends",
                  tags$h1("Team Members"),
                  "Arielle Herman, Connie Xu, Grace Li, Ariel Luo"
                )
              )),
      tabItem(tabName = "loans", class = "active",
              fluidPage(
                tags$div(
                  tags$h1("Repayment Rates", align = "center")),
                fluidRow(align = "left",
                         column(8, wellPanel(plotOutput("repay_rate_ave"))),
                         column(4, wellPanel(p("This first figure demonstrates the proportion of students across all University
                               fiscal cohorts who successfully make progress on repaying their loans in their
                               first year of repayment.  It highlights a declining average trend across all three income
                               brackets: smaller proportions of successive fiscal cohorts decline their loan
                               balances over time.  While the average fiscal cohort may be having more difficulty in
                               repaying their loans, we can look to the distribution of repayment rates across
                               universities in order to determine whether or not income brackets have been impacted
                               differently over time.")), style='padding:5px;')),
                fluidRow(align = "left",
                         column(4, wellPanel(p("This density plot demonstrates the distribution of repayment proportions across
                   universities for the first year of repayment for recent fiscal cohorts.  Each unviersity's fiscal 
                   cohort is then further subdivided by income bracket (such that there can be as many as three 
                   datapoints per university).  This reaffirms the findings in the previous plot, but also indicates
                   that the lowest income bracket has experienced the largest decrease in proportions of students who
                   make progress on their loans."))),
                         column(8, wellPanel(plotOutput("repay_rate_dist")))),
                tags$div(
                  tags$h1("University Selectivity and Student Debt", align = "center"),
                  tags$h2("University Selectivity", align = "center")),
                fluidRow(align = "left",
                         column(7, wellPanel(plotOutput("treemap"))),
                         column(5, wellPanel(p("We will now look at the relationships between student loans and the 'selectiveness'
                 of the universities in our dataset. Selectiveness (admissions rate) is often a proxy for the prestige
                 \"ranking\" of a school."),
                 p("This treemap shows how we categorized our admissions rate for 'university
                 selectivity' grouping (from the College Scorecard file) into 'selectiveness' categories. 
                 As shown, the vast majority of schools in our dataset were categorized as 'not
                 selective' or 'less selective'. Most of our analyses highlight the smaller categories (particularly
                 selective to highly selective/elite), because these categories have more within-group homogeneity and
                are more interesting to our focal questions.")))),
                #column(4, wellPanel(highcart2Output("treemap")))), # finish output function
                fluidRow(align = "left",
                         column(4, wellPanel(p("As schools become less selective, the median loan amount appears to be 
                 increasing (on average). We also see that the variance goes up in terms of the median student loan
                 amount (although this is partially due to the much larger number of schools in the less selective groupings)."),
                 p("This is somewhat interesting because more exclusive/selective schools are on average viewed as more
                 'competitive' and 'prestigious' (e.g., Harvard, Stanford, etc.). Such
                 schools include more financial resources (as more prestigious schools produce alma maters with higher
                 socioeconomic outcomes) and thus potentially can help more with financial aid and scholarships; however, 
                   these schools also disparately admit students from higher socioeconomic backgrounds (because higher-SES 
                   students have access to better secondary school education, and further have better access to
                   experiences conducive to acceptance in more competititve universities)."))),
                         column(8, wellPanel(plotOutput("admissions_scatter")))),
                tags$div(tags$h2("Student Debt Over Time", align = "center")),
                fluidRow(align = "left",
                         column(12, wellPanel(plotlyOutput("plotly")))),
                fluidRow(column(12, wellPanel(p("  Between 2010 and 2018, the median loan amount per student increased by 2000-4000+
                 USD (inflation-adjusted) - nationally, the median student loan amount increased by 19%. When one
                 disaggregates universities by selectivity, we see the same *overall* trend over time (i.e., there is a fair
                 amount of increase during the period in terms of average student loans that students leave with upon
                 entering repayment); however, some selectivity groups increased more dramatically than others (e.g., 'More
                 Selective' or schools with 20-30% acceptance rates appeared to increase greatly in terms of student debt
                 principal between 2015 and 2017, but this amount declined a bit again in 2018) and different selectivity
                 groups decreased in median student debt over time (less selective; selective schools seemed to decrease in
                 median debt following 2014)."),  
                p("To look at how median student debt has changed either by only national average or to look at specific
                'selectivity' buckets, one can simply click to remove the remaining lines in the plot.")))),
                fluidRow(tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
                         column(4, wellPanel(sliderInput(inputId = "year",
                                                label = "",
                                                value = 2019, min = 2010, max = 2019, sep = '')))),
                fluidRow(column(12, wellPanel(leafletOutput("studentdebtmap")))),
                fluidRow(column(12, wellPanel(p("Not all schools have similar amounts of median student debt burden (i.e., upon the
        beginning of the repayment period). This map summarizes the median debt burden by the state that the school is
        located in. As shown in this plot, some states (including schools in Pennsylvania, Minnesota, Illinois, etc.) tend to
        have the highest average median debt burden between the schools within. These states do not necessarily house the
        most prestigious schools. One of the things we can see when disaggregating this way is that locationally there is a
        lot of diversity of the student loan burden (on average) given to students by schools when disaggregated at the state
        level (ranging from 4K to over 20K USD in 2019)."),
        p("Note that we have also included the shiny interactivity component in which one can select the year on the slider to
        see the state-level debt median disaggregation across the years; this is not the primary function but instead is a
        functionality we added to allow individuals to 'explore the data' a bit, and to show once more the increases in 
        student debt over time (now on a state level).")))))),
      tabItem(tabName = "tweets", class = "active",
              fluidPage(
                fluidRow(align = "left",
                         column(6, wellPanel(plotOutput("trend"))),
                         column(6, wellPanel(wordcloud2Output("wordcloud")))),
                fluidRow(align = "left",
                         column(6, wellPanel(p("The google trend website gives us data about the time period and the count of
                                 total number of searches on Google. Plotting the total number of searches overtime
                                 helps us to see changes in people's interest in student loan forgiveness. The pattern
                                 clearly shows that people's interest in this topic has spiked in 2021. We hypothesized
                                 that this is a result of the Biden administration's recent announcement of a
                                 reinterpretation of a federal student loan cancellation program which will result in
                                 $1 billion in student loan forgiveness. Ever since this announcement, more people have
                                 been pushing for a more progressive policy and asking President Biden to cancel student
                                 debt through executive action. There is still a lot of uncertainties around whether a
                                 universal student loan forgiveness will be initiated via an executive order or via
                                 legislation." ))),
                         column(6, wellPanel(p("The wordcloud map shows key words that appeared in the tweets data. The size of the
                                 word represents how frequently it appears in tweets. Some of the most noticeable ones
                                 include people, president, loans, job, college, pay, biden, education,
                                 studentloanforgiveness, etc. This keywords pattern confirms our initial hypothesis that
                                 the recent discussion on canceling student debt on twitter is around the biden
                                 administration as a result of the $1B student loan cancellation announced in
                                 Mid-March.")))))),
      tabItem(tabName = "tweetmap", class = "active",
              fluidPage(
                fluidRow(column(12, wellPanel(leafletOutput("tweetsmap")))),
                fluidRow(align = "left",
                         column(12, wellPanel(p("Lastly, we want to see if there's  an overlap between the states where student loan
                         forgiveness is more discussed and the locations of the selective schools. We added two layers on the map
                         to showcase patterns. The first layer tells us which states are most concerned about student loan
                         forgiveness. The larger the circle, the higher the number of total tweets word count. The gradient
                         color scale shows the sentiment score of each state, where red means a more negative average tweet sentiment and green means a more positive tweet
                         sentiment on average. In addition, the popup window shows the top tweet keywords in each state after we
                         got rid of the most frequent keywords such as student, loan, debt, Cancelstudentdebt, and amp. People tweet the most about student loan forgiveness in
                         California. Other areas where there are more discussions on student loan forgiveness are states
                         along the coastlines. Midwestern states are least concerned about student loan forgiveness.
                     
                         The second layer shows where the selective institutions are. Elite universities are in purple, highly
                            selective universities are in green, and the blue markers are selective universities. Overall, these
                            selective universities concentrate along the coastlines. On the west coast, most of the
                            selective universities are in California. The selective universities spread out more evenly on the east
                            coast. Overall, we can see an overlap by looking at the two layers together. States with a higher
                            concentration of selective schools are also where student loan forgiveness is the most discussed."))))))
    ) # tabItems
  ) # dashboard Body
) # ui?

server <- function(input, output) {
  # arielle's plots ~~~~~~~~~~~~~~~~~~~~~
  output$repay_rate_ave <- renderPlot({repay_rate_ave})
  output$repay_rate_dist <- renderPlot({repay_rate_dist})
  
  # grace's plots ~~~~~~~~~~~~~~~~~~~~~~~
  output$trend <- renderPlot({Trend})
  output$wordcloud <- wordcloud2::renderWordcloud2({Twitter_wd})
  output$tweetsmap <- leaflet::renderLeaflet({TweetsMap})
  
  # Connie's plots ~~~~~~~~~~~~~~~~~~~~~~~
  #output$table <- renderDT({table_1})
  output$treemap <- renderPlot(treemap)
  output$admissions_scatter <- renderPlot({admissions_scatter})
  output$plotly <- renderPlotly({plot_line_plotly})
  
  ## leaflet ~~~~~~~~~
  ### connie map prep code data wrangle
    sc_time_selective <- reactive({Schools %>% subset(Year_Ending == input$year) %>%
        group_by(STABBR) %>% mutate(`Average Student Loans`=sum(DEBT_MDN_STUDENT,na.rm=TRUE)/sum(UGDS,na.rm=TRUE)) %>%
        group_by(STABBR,`Average Student Loans`) %>% summarize()})
    states <- states(cb = TRUE)
    states_year <- reactive({states %>% inner_join(sc_time_selective(), by=c(STUSPS='STABBR'))}) 
  
  ### render leaflet output
  output$studentdebtmap <- renderLeaflet({
    pal = colorFactor('Greens', domain = states_year()$`Average Student Loans`)
    pop_pop <- paste("State:",states_year()$NAME,"<br/>",
                     "Average Student Loans","<br/>", "of Schools Located in State:",
                     paste('$',round(states_year()$`Average Student Loans`)))
    
    sc_time_selective_title <- tags$p(tags$style('p{color:gray; font-size: 14px; family: serif}'),
                                      tags$b(paste('Average Debt By State',input$year)))
    leaflet(states_year()) %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(fillColor = ~pal(states_year()$`Average Student Loans`),
                  color = "white",
                  weight = 0.5,
                  fillOpacity = 0.7,  
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.7,
                    bringToFront = TRUE,
                  ),popup=pop_pop) %>%
      leaflet::addLegend(position = "bottomleft",
                         colors =c("#EDF8E9","#BAE4B3","#74C476","#31A354","#006D2C"),
                         labels = c(paste('$',round(min(states_year()$`Average Student Loans`)))," "," "," ",
                                    paste('$',round(max(states_year()$`Average Student Loans`)))),
                         title = "Average Student Loans (Per Student)") %>%
      leaflet::setView(-98.5795, 39.8282, zoom=3) %>% addControl(sc_time_selective_title, position='topright')
  })
  
  ### observe leaflet output  
  observe({leafletProxy("studentdebtmap", data = states_year())
    })
  
}
shinyApp(server = server, ui = ui)
#}) # profvis
 
