library(shiny)
library(magrittr)
library(data.table)
#library(highcharter)
library(dplyr)
library(rintrojs)
library(shinyBS)

#http://www.gisremotesensing.com/2015/11/passing-r-variables-dynamically-to.html

load("input_var.Rdata")

MakeMyInput <- function(id,label,choices = c('Female', 'Male', 'Other')) {
  
  Thesechoices <-choices %>%(function(x){
    paste0('<div value="',x,'">', x, '</div>')
  }) %>% paste(collapse = '')
  
  ThisId <- paste0('<div id="', id, '" class="CustomInput">')
  ThisLabel <- paste0('<label>', label, '</label>')
  
  paste(
    '<div class="CustomInputWrap">',
    ThisLabel,
    ThisId,
    Thesechoices,
    '</div></div>',
    collapse = '') %>% 
    HTML
  
}

MakeMyNumeric <- function(id,label,value = 0) {
  ThisLabel <- paste0('<label for="', id, '" >', label, '</label>')
  ThisValue <- paste0('<input id="', id, '" type="number" class="CustomInputWrap form-control shiny-bound-input" value="', value,'"/>')
  
  paste(
    '<div class="CustomInputWrap">',
    '<div class="form-group shiny-input-container">',
    ThisLabel,
    ThisValue,
    '</div>','</div>',
    collapse = '') %>% 
    HTML
  
}

makeBlock <- function(blockname,explanation,col = c("#cc5460","#66a961","#4390eb","#ffe408")){
  paste0("
         <div class='left_panel left_panel4 ' style='background-color:",col,";'>
         <div class='valueHolder'> </div>
         <div class='Tlab'> ",blockname," </div>
         <span class='infoGroup'> ", explanation,"</span>
         </div>
         ")
}








makeColSliders <- function(id = "expT",
                           labels = c("Ef1","Ef2","Ec","Save"),
                           explanation = expl[mvar %in% c("Ef1","Ef2","Ec","Save"), long %>% paste0(., collapse=", "),by = mvar]$V1,
                           defaultExp="Distribution of expenditure, to change drag the vertical black line",
                           LablePref="",
                           LablePos="%",
                           multiplier=100,
                           colours=c("#cc5460","#66a961","#4390eb","#ffe408"))
{
  paste0("<div id='Explain-for-",id,"' class='ColorSliderLabel'>",defaultExp,"</div>
          <div id='Def-Explain-for-",id,"' style='visibility:hidden; '>",defaultExp,"</div>
         <div id='",id,"' class='bar_slider' prefix ='",LablePref,"' posfix='",LablePos,"' multiplier='",multiplier,"'>
         ",
         makeBlock(labels,explanation,colours ) %>% 
           paste(collapse = "<div class='mysep-wrap'><div class='mysep'></div></div>")
         ,"
         
         </div>
         </div>") %>% HTML  
}
makeColSliders()


shinyUI(
  fluidPage(
    introjsUI(),
    tags$head(
              tags$title('VoL'),
              HTML(
                '<meta name="viewport" content="width=device-width, initial-scale=1">'
              ),
              HTML('<link rel="stylesheet" type="text/css" href="jquery-ui.css">
                  <link rel="stylesheet" type="text/css" href="style.css">'),
              includeScript("www/jquery-2.1.3.txt"),
              includeScript("www/jquery-ui.txt"),
              includeScript("www/thisjs.txt")
      ),
    
    actionButton("intorBtn",icon('info')),
    tags$div(style=' display: flex;justify-content: center;align-items: center;',
             HTML("Provided information will be saved!") %>% tags$div(class="leftIn", id="country_list",.)
    ),
    tags$div(style=' display: flex;justify-content: center;align-items: center;',
             HTML("Country") %>% tags$div(class="leftIn", id="country_list",.),
             selectizeInput("ctry", "", as.list(c("",ctry[,2])), options = list(placeholder = 'Select a country...',tabindex="-1", style="display: none;", value="", selected="selected"),
                            multiple = FALSE) %>% 
                                      tags$div(class="rightIn",.)
    ),
  #   
  #   tags$div(style='display: flex;
  # justify-content: center;
  #            align-items: center;',
  #            HTML('<div id="ctry1" class="ui-widget leftIn">
  # <label for="ctry1">County: </label>
  #                 <input id="ctry1">
  #                 </div>')
  #   ),
  #   
    tags$div(style=' display: flex; justify-content: center; align-items: center;',
        MakeMyInput('Gender','Gender',c('Female', 'Male', 'Other')) %>% tags$div(class="leftIn",.),
        MakeMyInput('children','Children',c(' Yes', ' No ')) %>% tags$div(class="rightIn",.)
    ),
    tags$div(style=' display: flex; justify-content: center; align-items: center;',
       MakeMyInput('urban','Place of residence',c(' Urban ', ' Non-urban '))  %>% tags$div(class="leftIn",.),
       MakeMyInput('nrwork','Workers in household',c('  1  ', ' 2+ ')) %>% tags$div(class="rightIn",.)
    ),
    tags$div(class='MidNumeric',
      numericInput("age","Age",value = 18) %>% tags$div(class="leftIn",.),
      numericInput("income", "Monthly net wage, $", "",value = 0) %>% tags$div(class="rightIn",.)
    ),
    tags$div(style=' display: flex; justify-content: center; align-items: center;',
      MakeMyInput('exp_prc','Expenditure in:',c('%', '$')) %>% tags$div(class="leftIn",.),
      MakeMyInput('act_prc','Time-Use in:',c('%', 'h')) %>% tags$div(class="rightIn",.)
    ),  
  #colorful SLiders done with makeColSliders function
makeColSliders(   id = "expT",
              labels = c("Ef1","Ef2","Ec","Save"),
              explanation = expl[mvar %in% c("Ef1","Ef2","Ec","Save"), long %>% paste0(., collapse=", "),by = mvar]$V1,
              defaultExp="Distribution of expenditure, to change drag the vertical black line",
              LablePref="",
              LablePos="%",
              multiplier="100")
              ,
makeColSliders(   id = "actT",
                  labels = c("Tw","Tf1","Tf2","Tc"),
                  explanation = actl[mvar %in% c("Tw","Tf1","Tf2","Tc"), long %>% paste0(., collapse=", "),by = mvar]$V1,
                  defaultExp="Distribution of Time, to change drag the vertical black line",
                  LablePref="",
                  LablePos="%",
                  multiplier="100"),

    br(),
    actionButton("calcVoL", "Calculate your VoL"),
    # a shiny element to display unformatted text
    #textOutput("wage"),
bsModal("modalExample", "", "calcVoL", tableOutput('results')
        ),
list()    
#tableOutput('results')
    )
    )
