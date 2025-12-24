#function 
funpyrwrapper.x <- function (res1,
                             ivar = "none", #cases where res is supplied, mention ivar is directly supplied
                             iindi = "pop", #res is shortened and iindi is supplied to control the title and period
                             # iregs, #put more than one country together
                             # iaggre.regs=T,
                             # iregaggr.name = "Total",
                             iarea = NULL,
                             iages = NULL,
                             iTime,
                             iiscen, iscale = 1, facet.scale = "fixed",
                             ititle.text = NULL, icol = "xxx"
) 
{
  res1 <<- res1
  iTime <<- iTime
  # iregs <<- iregs #select regions [more than one bhayo bhane jodnu parchha]
  # iaggre.regs <<- iaggre.regs #T or F
  # iregaggr.name <<- iregaggr.name #if T what is the name
  iages <<- iages #selecting particular ages
  iiscen <<- iiscen #
  ivar <<- ivar #pop deaths births imm emi
  iindi <<- iindi # indicator supplied
  icol <<- icol 
  iscale <<- iscale
  facet.scale <<- facet.scale
  ititle.text <<- ititle.text
  iscale.nm = ifelse(iscale == 1000, "Millions", ifelse(iscale ==1e+06, "Billions", "Thousands"))
  
  #Data Selection
  
  df1 <- res1[Time%in%iTime&scen%in%iiscen
  ][agest>=100,agest:=100 #limit to 100+ need to aggregate
  ]
  
  if(ivar=="none"){# variable as value
     df1 <- df1[agest <= 100 & agest >= ifelse(iindi=="deaths",-5,0)]
      
    
     #stock vs flows
     if (iindi != "pop")  df1 <- df1[,Time:= paste(Time, Time + 1, sep = "-")]
   print(head(df1))
   
   } else {
      df1 <- df1[, setnames(.SD, c(ivar),c("value"))#select variable
                  ][,.(scen,Time,sex,agest,value)
                   ][agest <= 100 & agest >= ifelse(ivar=="deaths",-5,0)]
      
      #stock vs flows
      if (ivar!="pop")  df1 <- df1[,Time:= paste(Time, Time + 1, sep = "-")]
    
    }
  
  # if(iaggre.regs) df1[,region:=iregaggr.name] #need to aggregate
  
  
  #color choose ...
  iage <- df1[,unique(agest)]
  
  # ivar = "pop"
  # iindi = "yy"
  # (ivar =="pop" | iindi == "pop")
   
  #aggregate in any case 
  df1 <- df1[, by = .(Time,sex, agest, scen), .(value = sum(value))]
  
  #final state-space (edu,sex,age,Time)
  # edu.nm = unique(df1$edu)
  sex.names = unique(df1$sex)
  female_nm = grep("f", sex.names, value = T)
  male_nm = grep("^m", sex.names, value = T)
  nTime = unique(df1$Time)
  
  
  #Graph Title
  # if (length(iTime) == 1){
  #   if(iregs == 1){
  #     if(iiscens == 1){
  #       
  #       if(iTime == 2020) ititle = icnt else ititle = paste(icnt, iiscen)    
  #       
  #       if (!is.null(ititle.text)) {
  #         iscale.nm.tot = iscale.nm#ifelse(iscale == 1000, "Millions", ifelse(iscale == 1e+06, "Trillions", "Millions"))
  #         if (ititle.text == "size") 
  #           ititle <- paste0(ititle, ": Population ", round(df1[,sum(value)]/iscale, 0), " ", iscale.nm.tot)
  #       }
  #       
  #       
  #       
  #     } else { #more than 1 scens
  #       ititle = paste(icnt)
  #     }
  #     
  #   } else {
  #     
  #   }
  #   
  #   
  #   
  # } else {
  #   
  # } 
  # 
  # 
  # 
  # 
  # 
  # 
  # 
  # if (length(iiscen) > 1) {
  #   if (length(iregs) > 1) {
  #     ititle = paste0(iregaggr.name,": Population - Test Scenarios")
  #   }    else {
  #     ititle = paste(icnt, "Population SSPs")
  #   }
  # }
  
  #Graph Legend
  # legend.title = "Education"
  ititle = iarea
  
  gg1 <- df1 %>% arrange(agest) %>%
    ggplot(mapping = aes(x = agest,y = ifelse(sex == female_nm, value, -value)/iscale, fill=sex)) +
    geom_bar(stat = "identity", position = "stack") + 
    coord_flip() +
    facet_grid(rows = vars(Time), cols = vars(scen), 
                              scales = facet.scale) +
    ggtitle(ititle) + 
    labs(x = "Age group",y = paste("Population in", paste("'", iscale.nm, sep = ""),sep = " "), fill="Sex") + 
    # scale_fill_manual(name = legend.title, breaks = edu.nm, 
    #                   labels = legend.labels, values = ipal, guide = guide_legend(reverse = TRUE)) + 
    scale_y_continuous(labels = abs) + geom_hline(yintercept = 0, 
                                                  color = "black") + theme_bw()+
    scale_fill_manual(values = c("f" = "#f8766d", "m" = "#00bfc4"),
                      labels=c("f"="Female", "m"= "Male"))+theme(plot.title = element_text(face = "bold"))
  
  ymax = max(ggplot_build(gg1)$layout$panel_params[[1]]$x$minor_breaks)
  
  if (length(nTime) >= 3) {
    iht = 3
    iwd = 8.5
    textsize = 4
  }else {
    iht = 5.5
    iwd = 8.5
    textsize = 4
  }
  
  gg1 <- gg1 + geom_text(y = c(-ymax * 0.75), x = c(100),size = textsize, hjust = "inward", parse = TRUE, check_overlap = TRUE,label = c("M")) + 
    geom_text(y = c(ymax * 0.75), x = c(100),size = textsize, hjust = "inward", parse = TRUE, check_overlap = TRUE,label = c("F")) +
    geom_text(y=c(0),x=c(1), size=6, hjust="inward", check_overlap = T, label=c("Source: PSR HUB"), 
              color="#418fde", fontface="bold", family="Calibri Bold",alpha=0.7)+
    theme(panel.spacing.x = unit(1.5,"lines"))
  
  return(gg1)
}
