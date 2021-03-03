library(shiny)
library(tidyverse)
library(lubridate)

#Load in data set, transform date variable 
data1 = read_csv("/Users/davidschultheiss/Downloads/GPS_Well_Month_deidentified.csv") %>%
    mutate(week_num = week(Date)) %>% 
    select(-Date)

#Player Wellness Data 
player_well = data1 %>%
    select(week_num, Athlete, Sleep_Quality:Desire_Motivation_to_Train) %>% 
    pivot_longer(-c(week_num, Athlete), names_to = 'Metric', values_to = 'Values') %>% 
    group_by(week_num, Metric, Athlete) %>%
    summarise_at(vars(Values), list(mean = ~ mean(., na.rm = T),
                                    lower = ~ quantile(.,probs = 0.025), 
                                    upper = ~ quantile(.,probs = 0.975)))

#Player GPS Data
player_gps = data1 %>%
    select(week_num, Athlete, PlayerLoad_per_Min.) %>% 
    filter(complete.cases(.)) %>%
    filter(PlayerLoad_per_Min. != '#DIV/0!') %>% 
    mutate_at(vars(PlayerLoad_per_Min.), ~ as.numeric(.)) %>% 
    pivot_longer(-c(week_num, Athlete), names_to = 'Metric', values_to = 'Values') %>% 
    group_by(week_num, Metric, Athlete) %>%
    summarise_at(vars(Values), list(mean = ~ mean(., na.rm = T),
                                    lower = ~ quantile(.,probs = 0.025), 
                                    upper = ~ quantile(.,probs = 0.975)))

#Position Wellness Data
position_well = data1 %>%
    select(week_num, Position, Sleep_Quality:Desire_Motivation_to_Train) %>% 
    pivot_longer(-c(week_num, Position), names_to = 'Metric', values_to = 'Values') %>% 
    group_by(week_num, Metric, Position) %>%
    summarise_at(vars(Values), list(mean = ~ mean(., na.rm = T),
                                    lower = ~ quantile(.,probs = 0.025), 
                                    upper = ~ quantile(.,probs = 0.975)))

#Position GPS Data
position_gps = data1 %>%
    select(week_num, Position, PlayerLoad_per_Min.) %>% 
    filter(complete.cases(.)) %>%
    filter(PlayerLoad_per_Min. != '#DIV/0!') %>% 
    mutate_at(vars(PlayerLoad_per_Min.), ~ as.numeric(.)) %>% 
    pivot_longer(-c(week_num, Position), names_to = 'Metric', values_to = 'Values') %>% 
    group_by(week_num, Metric, Position) %>%
    summarise_at(vars(Values), list(mean = ~ mean(., na.rm = T),
                                    lower = ~ quantile(.,probs = 0.025), 
                                    upper = ~ quantile(.,probs = 0.975)))

#Team Wellness Data
team_well = data1 %>%
    select(week_num, Sleep_Quality:Desire_Motivation_to_Train) %>% 
    pivot_longer(-week_num, names_to = 'Metric', values_to = 'Values') %>% 
    group_by(week_num, Metric) %>%
    summarise_at(vars(Values), list(mean = ~ mean(., na.rm = T),
                                    lower = ~ quantile(.,probs = 0.025), 
                                    upper = ~ quantile(.,probs = 0.975)))

#Team GPS Data
team_gps = data1 %>%
    select(week_num, PlayerLoad_per_Min.) %>% 
    filter(complete.cases(.)) %>%
    filter(PlayerLoad_per_Min. != '#DIV/0!') %>% 
    mutate_at(vars(PlayerLoad_per_Min.), ~ as.numeric(.)) %>% 
    pivot_longer(-week_num, names_to = 'Metric', values_to = 'Values') %>% 
    group_by(week_num, Metric) %>%
    summarise_at(vars(Values), list(mean = ~ mean(., na.rm = T),
                                    lower = ~ quantile(.,probs = 0.025), 
                                    upper = ~ quantile(.,probs = 0.975)))




###############################UI
ui <- fluidPage(
    tabsetPanel(
        ####Panel for Player Data 
        tabPanel('Player Data', fluid = T, 
                 sidebarLayout(
                     sidebarPanel(selectInput('letter', 'Player ID:', LETTERS[c(1:9, 12:13, 17:20, 24)])),
                     mainPanel(fluidRow(
                         column(12, plotOutput('wellPlot1')),
                         column(12, plotOutput('gpsPlot1'))
                     ))
                 )),
        ####Panel for Position Data
        tabPanel('Position Data', fluid = T, 
                 sidebarLayout(
                     sidebarPanel(selectInput('pos_input', 'Position:',
                                              c('Centerback' = 'Centerback',
                                                'Fullback' = 'Fullback', 
                                                'GK' = 'GK', 
                                                'Central Midfielder' = 'Central Midfielder',
                                                'Forward' = 'Forward',
                                                'Winger' = 'Winger'))),
                     mainPanel(fluidRow(
                         column(12, plotOutput('wellPlot2')),
                         column(12, plotOutput('gpsPlot2'))
                     ))
                 )),
        ####Panel for Team Data
        tabPanel('Team Data', fluid = T, 
                 mainPanel(fluidRow(
                     column(12, plotOutput('teamPlot1')),
                     column(12, plotOutput('teamPlot2'))
                 )))
    )
    
)
   



###############################Server
server <- function(input, output) {

    ####Player Wellness
    playerwell = reactive({
        playerwell = player_well %>% 
            filter(Athlete == input$letter) %>%
            select(-Athlete) 
    })
    output$wellPlot1 = renderPlot({
        ggplot(playerwell(), aes(x= week_num, y= mean, fill= week_num, width=.5)) +
            geom_bar(stat= 'identity', position= position_dodge(), width= .75) + 
            geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= position_dodge(0.5)) + 
            facet_wrap(~Metric, labeller = labeller(Metric = c(
                'Desire_Motivation_to_Train' = 'Desire/Motivation to Train',
                'Outside_Stress' = 'Outside Stress', 'Sleep_Quality' = 'Sleep Quality',
                'General_Muscle_Soreness' = 'General Muscle Soreness', 'Mood' = 'Mood',
                'Fatigue' = 'Fatigue'))) + 
            xlab('Week Number') +
            ylab('Ranking (1-5)') +
            ggtitle('Player Wellness Data') + 
            theme(plot.title = element_text(hjust = 0.5))
    })
    
    ####Player GPS 
    playergps = reactive({
        playergps = player_gps %>% 
            filter(Athlete == input$letter) %>%
            select(-Athlete) 
    })
    output$gpsPlot1 = renderPlot({
        ggplot(playergps(), aes(x= week_num, y= mean, fill= week_num, width=.5)) +
            geom_bar(stat= 'identity', position= position_dodge(), width= .75) + 
            geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= position_dodge(0.5)) + 
            xlab('Week Number') +
            ylab('Player Load Per Min.') +
            ggtitle('Player GPS Data') + 
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylim(0,10)
    })
    
    ###Position Wellness 
    positionwell = reactive({
        positionwell = position_well %>% 
            filter(Position == input$pos_input) %>%
            select(-Position)
    })
    output$wellPlot2 = renderPlot({
        ggplot(positionwell(), aes(x= week_num, y= mean, fill= week_num, width=.5)) +
            geom_bar(stat= 'identity', position= position_dodge(), width= .75) + 
            geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= position_dodge(0.5)) + 
            facet_wrap(~Metric, labeller = labeller(Metric = c(
                'Desire_Motivation_to_Train' = 'Desire/Motivation to Train',
                'Outside_Stress' = 'Outside Stress', 'Sleep_Quality' = 'Sleep Quality',
                'General_Muscle_Soreness' = 'General Muscle Soreness', 'Mood' = 'Mood',
                'Fatigue' = 'Fatigue'))) + 
            xlab('Week Number') +
            ylab('Avg. Ranking (1-5)') +
            ggtitle('Position Wellness Data') + 
            theme(plot.title = element_text(hjust = 0.5))
    })
    ####Position GPS 
    positiongps = reactive({
        positiongps = position_gps %>% 
            filter(Position == input$pos_input) %>%
            select(-Position)
    })
    output$gpsPlot2 = renderPlot({
        ggplot(positiongps(), aes(x= week_num, y= mean, fill= week_num, width=.5)) +
            geom_bar(stat= 'identity', position= position_dodge(), width= .75) + 
            geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= position_dodge(0.5)) + 
            xlab('Week Number') +
            ylab('Avg. Player Load Per Min.') +
            ggtitle('Position GPS Data') + 
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylim(0,10)
    })
    ###Team Wellness
    teamwell = reactive({
        teamwell = team_well 
    })
    output$teamPlot1 = renderPlot({
        ggplot(teamwell(), aes(x= week_num, y= mean, fill= week_num, width=.5)) +
            geom_bar(stat= 'identity', position= position_dodge(), width= .75) + 
            geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= position_dodge(0.5)) + 
            facet_wrap(~Metric, labeller = labeller(Metric = c(
                'Desire_Motivation_to_Train' = 'Desire/Motivation to Train',
                'Outside_Stress' = 'Outside Stress', 'Sleep_Quality' = 'Sleep Quality',
                'General_Muscle_Soreness' = 'General Muscle Soreness', 'Mood' = 'Mood',
                'Fatigue' = 'Fatigue'))) + 
            xlab('Week Number') +
            ylab('Avg. Ranking (1-5)') +
            ggtitle('Team Wellness Data') + 
            theme(plot.title = element_text(hjust = 0.5))
    })
    ####Team GPS
    teamgps = reactive({
        teamgps = team_gps
    })
    output$teamPlot2 = renderPlot({
        ggplot(teamgps(), aes(x= week_num, y= mean, fill= week_num, width=.5)) +
            geom_bar(stat= 'identity', position= position_dodge(), width= .75) +
            geom_errorbar(aes(ymin= lower, ymax= upper), width= .2, position= position_dodge(0.5)) + 
            xlab('Week Number') +
            ylab('Avg. Player Load Per Min.') +
            ggtitle('Team GPS Data') + 
            theme(plot.title = element_text(hjust = 0.5)) + 
            ylim(0,10)
    })
}



# Run the application 
shinyApp(ui = ui, server = server)
