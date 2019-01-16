# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

waterfall_graph<-function(user_data){
  data_waterfall<-user_data
  Metric<-unique(as.character(data_waterfall$Metric))
  data_waterfall$spend_diff<- round(data_waterfall$opt_spend-data_waterfall$Spend)
  data_waterfall$inc_spend<-ifelse(data_waterfall$spend_diff>0,data_waterfall$spend_diff,"0")
  data_waterfall$dec_spend<-ifelse(data_waterfall$spend_diff<=0,-1*data_waterfall$spend_diff,"0")

  #Table structure for waterfall plot

  #add a new empty row
  temprow <- c(rep.int(0,length(data_waterfall)))
  temprow<-transpose(data.frame(temprow))
  colnames(temprow)<-colnames(data_waterfall)
  data_waterfall$Metric<-as.character(data_waterfall$Metric)
  data_waterfall<-rbind(temprow,data_waterfall)
  data_waterfall<-rbind(data_waterfall,temprow)

  data_waterfall$Metric[1]<-"Hist.Spend"
  data_waterfall$Metric[nrow(data_waterfall)]<-"Opt.Spend"

  data_waterfall$Metric<-factor(data_waterfall$Metric,
                                levels=c("Hist.Spend",Metric,"Opt.Spend"))
  # make a new column as start and end and make the same in row

  #start budget (adjusting the bar length)
  start<-sum(data_waterfall$Spend)
  data_waterfall$start<- c(start/20,rep(0,(nrow(data_waterfall)-1)))
  data_waterfall$end<- rep(0,nrow(data_waterfall))


  data_waterfall$inc_spend<-as.numeric(data_waterfall$inc_spend)
  data_waterfall$dec_spend<-as.numeric(data_waterfall$dec_spend)
  data_waterfall$start<- as.numeric(data_waterfall$start)

  data_waterfall$base<-0
  for(i in 2:nrow(data_waterfall)){
    data_waterfall$base[i]<-data_waterfall$base[i-1]+data_waterfall$inc_spend[i-1]+
      data_waterfall$start[i-1]-data_waterfall$dec_spend[i]
  }

  #putting the value of end cost in the end column for plotting it independently
  data_waterfall$end[nrow(data_waterfall)]<-data_waterfall$base[nrow(data_waterfall)]
  data_waterfall$base[nrow(data_waterfall)]<-0


  ## text to be displayed in the graph ##

  txt<-sapply(data_waterfall$spend_diff,function(tx) {
    if(substr(as.character(tx),1,1)!="-"){
      div <- findInterval(as.numeric(gsub("\\,", "", tx)),
                          c(1, 1e3, 1e6, 1e9, 1e12) )
      a<-paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 1),
               c("","K","M","B","T")[div] )
      return(a)
    }
    else
    {

      txneg<-(-1)*tx
      div <- findInterval(as.numeric(gsub("\\,", "", txneg)),
                          c(1, 1e3, 1e6, 1e9, 1e12) )
      a<-paste("-",round( as.numeric(gsub("\\,","",txneg))/10^(3*(div-1)), 1),
               c("","K","M","B","T")[div] )
      return(a)
    }

  }
  )
  ## to put spend as text on start and end bar values
  txt[1]<- sum(data_waterfall$Spend)
  txt[length(txt)]<-sum(data_waterfall$opt_spend)

  ## shaping the axes
  f <- list(
    family = "Arial Black",
    weight="bold",
    size = 15,
    color = "#444649"
  )
  x <- list(
    title = "",
    titlefont = f
  )
  y <- list(
    title = "Spend ($)",
    titlefont = f,
    showticklabels = FALSE
  )

  # dynamically fitting the x axis label for large names
  max_length <- max(str_length(Metric))*6

  ##plot for waterfall
  p <- plot_ly(data_waterfall,source = "port_spend_waterfall", x = ~Metric, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'),
               hoverinfo="none") %>%
    add_trace(y = ~inc_spend, marker = list(color = '#3D9970',
                                            # color='hsl(80,240,57)',
                                            line = list(color = '#3D9970',
                                                        width = 2)),name ="Profit",hoverinfo="none")  %>%
    add_trace(y = ~dec_spend, marker = list(color = '#DD4B39',
                                            # color='hsl(41,240,106)',
                                            line = list(color = '#DD4B39',
                                                        width = 2)),name ="Loss",hoverinfo="none") %>%
    add_trace(y = ~start, marker = list(color = '#A6A6A6',
                                        line = list(color = '#A6A6A6',
                                                    width = 2)),name="Inital Spend",hoverinfo="none") %>%
    add_trace(y = ~end, marker = list(color = 'steelblue',
                                      line = list(color = 'steelblue',
                                                  width = 2)),name="Optimized Spend",hoverinfo="none") %>%

    layout(
      autosize = T, #height = 480,#width=600,#xaxis = list(title = "", color ="", tickangle = 90),
      title = '<b>Changes in Budget and Allocation<b>',
      xaxis = x, yaxis = y,
      barmode = 'stack',
      #   paper_bgcolor = 'rgba(245, 246, 249, 1)',
      #   plot_bgcolor = 'rgba(245, 246, 249, 1)',
      hoverlabel = list(font=list(family='calibri',size=20,color='white'),bgcolor='rgb(44,59,65)',bgcolor='hsl(131,46,51)',bordercolor='rgb(44,59,65)'),
      showlegend = FALSE,
      margin=list(b=max_length)
    )%>%
    add_annotations(text = txt,
                    x = data_waterfall$Metric,
                    y = data_waterfall$base+data_waterfall$inc_spend+data_waterfall$dec_spend+data_waterfall$start+data_waterfall$end+((0.6/100)*start),
                    xref = "x",
                    yref = "y",
                    font = list(family = 'Arial',
                                size = 14,
                                weight= 'bold',
                                color = '#4C4C4C'),
                    showarrow = FALSE)

}
