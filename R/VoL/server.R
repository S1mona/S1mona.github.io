  #https://codepen.io/MyXoToD/pen/IDnfk
library(shiny)
library(magrittr)
library(data.table)
#library(highcharter)
library(dplyr)

load("input_var.Rdata")

shinyServer(function(input, output, session) {
  # intro js
  
  observeEvent(input$intorBtn, {
    intro <- data.frame(element = c(".selectize-control", "#Gender","#children","#urban","#nrwork","#age","#income","#exp_prc","#act_prc","#cont_exp",
                                    "#cont_act","#calcVoL"),
                        intro = c("Select country of residence",
                                  "Select gender",
                                  "Select number of children",
                                  "Where do you live?",
                                  "Number of workers in your household",
                                  "How old are you?",
                                  "Monthly netto wage",
                                  "Percentage or number?",
                                  "Percentage or number?",
                                  "Structure of expediture",
                                  "Structure of time disribution",
                                  "Get results!"
                                  ))
    introjs(session, options = list(steps= intro))
  })

  rez <- data.frame(long="a", val=0)
  dem <- data.frame(long="a", val=0)
  ffname <- isolate(paste0(
    "output/ans_",
    Sys.time() %>% as.numeric %>% as.character %>%
      gsub("\\.", "", .)
  ))
  #x <- "%"
  calc_vol <- function(x){
    #browser()
    # crate demog matrix
    demo <- x[type=="demo",]
    demo[, value:= trimws(value)]
    act <- . %>% unlist%>%as.numeric
    #browser()
    
    demo_val <- data.table(segm=c("Global",
                      c("ResUrb", "PeGenF", "PeAge", "HhChildrYN", "HhWorkers", "PeAllIncW")),
               part=c(3,ifelse(demo[var=="urban", value]=="Urban", 1, 2), 
                      ifelse(demo[var=="gender", value]=="Male", 1, 2),
                      ifelse(demo[var=="age", value] < 46, 1, 2),
                      ifelse(demo[var=="children", value]=="No", 1, 2),
                      ifelse(demo[var=="nrwork", value]=="1", 1, 2),
                      ifelse(((demo[var=="income", value]%>%act)/4)<(432 *0.82), 2, 1)
                      ))
    demo_val <- data.table(segm=demo_val[, segm], var=c("Global","urban","gender", "age", "children", "nrwork", "income")) %>% 
      merge(demo, all=TRUE)%>%merge(demo_val, by="segm", all=TRUE)
    demo_val <- demo_val[!value%in%c("NULL", ""),]
    demo_val[, c("type", "unit", "time", "var", "value"):=NULL]
    #demo <- demo[!value%in%c("NULL", ""),]
    demo_val_op <- copy(demo_val)
    demo_val_op[, part:=replace(part, part==2, 11)]
    demo_val_op[, part:=replace(part, part==1, 2)]
    demo_val_op[, part:=replace(part, part==11, 1)]
    #browser()
    dtmp <- merge(demo_val, vsegl1, by=c("segm", "part"))%>%merge(., tmp)
    dtmp_op <- merge(demo_val_op, vsegl1, by=c("segm", "part"))%>%merge(., tmp)
    
    #phi <- dtmp[segm=="Global", est]
    phi <- dtmp[, est]
    phi_op <- dtmp_op[, est]
      if(unique(x[type=="act",unit])=="h"){
        Tw <- x[type=="act"&var=="Tw", value]%>%act
        Tc <- x[type=="act"&var=="Tc", value]%>%act
      }else{
        Tw <- (x[type=="act"&var=="Tw", value]%>%act)*168/100
        Tc <- (x[type=="act"&var=="Tc", value]%>%act)*168/100
      }
      
      #Tw <- x[type=="act"&var=="Tw", value]%>%act
      inc <- (x[type=="demo"&var=="income", value]%>%act)/4
      w <- (inc)/(Tw)
      #Tc <- x[type=="act"&var=="Tc", value]%>%act
      if(unique(x[type=="exp",unit])=="$"){
        Ec <- (x[type=="exp"&var=="Ec", value]%>%act)/4
      }else{
        Ec <- (x[type=="exp"&var=="Ec", value]%>%act)*inc/100
      }
      rez <- data.table(dtmp[,.(segmn,  parn)], VoL=(1/phi*(w*Tw-Ec)/(7*24- Tw-Tc))%>%round(., digits=2))
      rez <- rbind(rez[1,], rez)
      rez[1, ] <- list("Your wage $/h", "",  w%>%round(., digits=2))
      setnames(rez, c("Model", "Segment", "VoL"))
      rez_op <- data.table(dtmp_op[,.(segmn,  parn)], VoL=(1/phi_op*(w*Tw-Ec)/(7*24- Tw-Tc))%>%round(., digits=2))
      rez_op <- rbind(rez_op[1,], rez_op)
      rez_op[1, ] <- list("Wage", "",  w%>%round(., digits=2))
      setnames(rez_op, c("Model", "Segment", "VoL"))
      rez_op[, VoL:=as.character(VoL)]
      setnames(rez_op, "VoL", "VoL $/h")
      setnames(rez, "VoL", "VoL $/h")
      rez_op[1:2, ] <- list("", "", "")
      list(cbind(rez, rez_op[, -1]))
  }

  save_res <- function() {
    tt <- Sys.time() 
    rez[, time:=tt]
    #print(ffname)
      save(
        rez,
        file =  paste0(
          ffname,
          "_",
         tt %>% as.numeric %>% as.character %>%
            gsub("\\.", "", .),
          ".Rdata"
        )
      )
  }
  
      res <- reactive({
        input$jsValue
      })
      
      ress <- reactive({
        list(input$Gender, input$age, input$children, input$nrwork, input$income, input$urban, input$ctry)
      })
      
      
      observe({
        if(length(res())>1){
          rez <<- res()
          rez <<- lapply(rez, function(x)strsplit(x, " ")%>%unlist%>%matrix(., nrow=1))%>%do.call("rbind", .) %>%
            data.table()
          setnames(rez, c("type", "var", "value"))
          rez[, unit:=gsub("\\d|\\.", "", value)]
          rez[, value:=as.numeric(gsub("\\$|h|\\%", "", value))]
          rez[, type:=gsub("_", "", type)]
          #browser()
          dem <<- data.table(type="demo", var=c("gender", "age", "children", "nrwork", "income","urban", "country"), value=ress(), unit="")
          rez <<- rbind(rez, dem)
          save_res()
          output$results <<-  renderTable({
            if(nrow(rez)>1){
             calc_vol(rez)[[1]]
            }
          })
        }
      })
      session$allowReconnect(TRUE)
      session$onSessionEnded(function(x){
        save_res()
      })
      
})
    