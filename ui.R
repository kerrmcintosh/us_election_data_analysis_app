

# UI section 

ui <- fluidPage(    tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
),
theme = shinytheme("simplex"),
navbarPage(
  title = div(img(src="usflag.jpg", id = "flag"), "The 2020 US Election, a State Level Analysis")),


tabsetPanel(
  # Tab 1
  tabPanel("Election Overview", div(class = "separator"),
           
           fluidRow(
             column(9,
                    tags$h4(textOutput("title_map")),
                    plotOutput("us_map"),
                    tags$div(class = "separator30"),
                    column(1),
                    column(10,
                    tags$div(class ="table_shade",
                    tags$div(class ="select_button",
                             selectInput("state_tab1",
                                          tags$h5(textOutput("which_state")),
                                          choices = c("Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii",
                                                          "Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri", "Montana",
                                                          "Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina",
                                                          "South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"))),
                    div(class = "separator"),
                    DT::dataTableOutput("state_key_inds")))

             ),
             column(3,
                    div(class = "separator"),
                    tags$div(class ="radio_button",
                             selectInput("year",
                                          tags$h5(textOutput("which_year")),
                                          choices = c(2016, 2020),
                                          selected = 2020)),
                    div(class = "separator"),
                    tags$h6(textOutput("vote_share")),
                    plotOutput("share", height = 110),
                    div(class = "separator"),
                    tags$h6(textOutput("elec_vote")),
                    textOutput("vote_270"),                             
                    plotOutput("vote", height = 110),
                    div(class = "separator"),
                    tags$h6(textOutput("state_share")),
                    plotOutput("state_sh", height = 110),
                    tags$div(class = "separator"),
                    tags$div(class="side_chart",
                      tags$div(class ="radio_button",
                             selectInput("indicator_bars",
                                         tags$h5(textOutput("compare")),
                                         choices = c("Per Capita Income 2020 ($)",
                                                     "Income Change Over Year (%)","Have Health Insurance (%)","Unemployment Rate Sept 2020 (%)",
                                                     "Proportion of Population Aged 65+ (%)","Proportion of Population with High School Diploma or Higher (%)","Proportion of Population with Bachelors or Higher (%)","Net International Migration (%)",
                                                     "Covid Cases as Proportion of Population (%)", "Covid Deaths as Proportion of Cases (%)", "Proportion of Adults with atleast One Gun in Household (%)"))),
                      tags$div(class = "separator"),
                      tags$h6(textOutput("key_stat_name")),
                      plotOutput("state_split", height = 260)),
                    tags$div(class = "separator"),
                    tags$div(img(src="elephant_donkey.png", class = "symbol"))
             ))),
       
           
           # End Tab 1
           
  
  
  # Tab2
  tabPanel("State Demographics", div(class = "separator"),
           fluidRow(
             column(8,
                    tags$h4(textOutput("title_tab2")),
                    plotOutput("sex_plot"),
                    div(class = "separator"),
                    plotOutput("age_plot"),
                    div(class = "separator"),
                    plotOutput("age2_plot"),
                    div(class = "separator"),
                    plotOutput("hispanic_plot"),
                    div(class = "separator"),
                    plotOutput("black_plot"),
                    div(class = "separator"),
                    plotOutput("college_plot"),
                    div(class = "separator")
             ),
             column(4,
                    div(class = "separator"),
                    tags$div(class ="radio_button_conor",
                             radioButtons("sex",
                                          tags$h5(textOutput("sex_text")),
                                          choices = c("Yes", "No"),
                                          selected = "Yes")),
                    plotOutput("tab_2_map", height = 280),
                    div(class = "separator"),
                    tags$div(class ="radio_button_conor",
                             radioButtons("age",
                                          tags$h5(textOutput("age_text")),
                                          choices = c("Lower than average", "Higher than average"),
                                          selected = "Lower than average")),
                    plotOutput("tab_2_map2", height = 280),
                    div(class = "separator"),
                    tags$div(class ="radio_button_conor",
                             radioButtons("age2",
                                          tags$h5(textOutput("age_text2")),
                                          choices = c("Lower than average", "Higher than average"),
                                          selected = "Lower than average")),
                    plotOutput("tab_2_map3", height = 280),
                    div(class = "separator"),
                    tags$div(class ="radio_button_conor",
                             radioButtons("hispanic",
                                          tags$h5(textOutput("hisp_text")),
                                          choices = c("Yes", "No"),
                                          selected = "Yes")),
                    plotOutput("tab_2_map4", height = 280),
                    div(class = "separator"),
                    tags$div(class ="radio_button_conor",
                             radioButtons("black",
                                          tags$h5(textOutput("black_text")),
                                          choices = c("Yes", "No"),
                                          selected = "Yes")),
                    plotOutput("tab_2_map5", height = 280),
                    div(class = "separator"),
                    tags$div(class ="radio_button_conor",
                             radioButtons("college_1",
                                          tags$h5(textOutput("college_text")),
                                          choices = c("Yes", "No"),
                                          selected = "Yes")),
                    plotOutput("tab_2_map6", height = 280)
             ))),
#End Tab 2
           
  # Tab3
  tabPanel("Influence of Covid", div(class = "separator"),


           fluidRow(
           column(6,
                              tags$div(class = "radio_button",
                             radioButtons("covid_radio",
                                         tags$h5("Choose Covid Stat"),
                                         choices = c("Covid Cases as Proportion of Population (%)", "Covid Deaths as Proportion of Cases (%)"))),
                  div(class = "separator"),
                  div(class = "separator"),
                  tags$div(class = "stat_box",
                           tags$h5(textOutput("covid_us")),
                                    tags$h3(textOutput("covid_stat")))

                    ),
           column(6,
                  tags$h5(textOutput("covid_title")),
                  plotOutput("case_death", height =250)
                  )),
           div(class = "separator"),
           fluidRow(
             column(9,
                    plotOutput("total_cases"),
                    plotOutput("total_deaths")
             ),
             column(3,
                  tags$div(class = "radio_button",
                             selectInput("select_num_votes", 
                                tags$h5(textOutput("select_votes_text")),
                                choices = list("3-6", "7-10", "11-14", "15-19", "20+"), 
                                selected = "3-6")
                             
                      ))
             ),
           div(class = "separator"),
           div(class = "separator"),          
           tags$div(class ="table_shade",
                    fluidRow(
             column(2),
             column(8,
                    plotOutput("lm_cases_vs_deaths")
             ),
             column(2))))
# 
#           # End Tab 3
#            
#   
#   
#   
),

div(class = "separator"),
tags$footer(class = "footer_text", paste0("Produced by David Wright, Conor Power & Kerr McIntosh"),
            tags$div(class = "separator"))

)