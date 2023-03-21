Visualization in R
[YS Practice]
 
#______________________________________________________________#
#                         Line Plot                         ----
    line_plot = df %>%
      mutate(dates = as.Date(paste(df$Date,"-01",sep=""))) %>%
      ggplot(aes(x=dates, y=Interest, group = 1)) +
      geom_line(size = 2, alpha = 0.75, color = "lightpink3") +
      geom_point(color = "lightpink2", alpha = 0.9)+
      theme_solarized(light = FALSE) +
      labs(title = "Rocket League trend",
           y = "Interest") +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            axis.title.x = element_blank(),
            title = element_text(color = "#EEEEEE", size = 20),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1") +
      scale_x_date(date_labels = "%B %Y")
      
#----

![image](https://user-images.githubusercontent.com/58554631/226737767-7d8c2758-28dc-43c8-b271-95fcad21a5db.png)
    
  
#______________________________________________________________#
#                       Waterfall Plot                      ----
    wf = df %>%
      mutate(dates = as.Date(paste(df$Date,"-01",sep=""))) 

    ggplot_waterfall(wf, 'dates', 'Interest') +
    geom_line(size = 2, alpha = 0.75, color = "lightpink3") +
    geom_point(color = "lightpink2", alpha = 0.9) +
    theme_solarized(light = FALSE) +
    labs(title = "Rocket League trend",
         y = "Interest") +
    scale_fill_continuous(low = 'green', high = 'red') + 
    theme(text = element_text(family = "fav", 
                              colour = "#EEEEEE",
                              face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          title = element_text(color = "#EEEEEE", size = 20),
          panel.background = element_rect(fill = NA),
          plot.background = element_rect(fill = "#111111"),
          plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1") 
#----
  
      
![image](https://user-images.githubusercontent.com/58554631/226738189-9165170a-da77-421e-95c8-8e4e472bda90.png)
#______________________________________________________________#
#                          Lag Plot                         ----
    lag1_plot = df %>%
      mutate(lag1 = lag(Interest, 1)) %>%
      ggplot(aes(x=lag1, y=Interest)) +
      geom_point(color = "lightpink2", alpha = 0.5, size=3)+
      theme_solarized(light = FALSE) +
      labs(title = "Lag 1") +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            title = element_text(color = "#EEEEEE", size = 20),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1") 

    lag2_plot = df %>%
      mutate(lag2 = lag(Interest, 2)) %>%
      ggplot(aes(x=lag2, y=Interest)) +
      geom_point(color = "lightpink2", alpha = 0.5, size=3)+
      theme_solarized(light = FALSE) +
      labs(title = "Lag 2") +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            title = element_text(color = "#EEEEEE", size = 20),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1") 

    lag3_plot = df %>%
      mutate(lag3 = lag(Interest, 3)) %>%
      ggplot(aes(x=lag3, y=Interest)) +
      geom_point(color = "lightpink2", alpha = 0.5, size=3)+
      theme_solarized(light = FALSE) +
      labs(title = "Lag 3") +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            title = element_text(color = "#EEEEEE", size = 20),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1") 

    lag1_plot | lag2_plot | lag3_plot
#----
  
![image](https://user-images.githubusercontent.com/58554631/226738827-8bc00434-6cab-4614-93d8-b0afcfe81417.png)

  
#______________________________________________________________#
#                        Autocor Plot                       ----
    ac <- df%>%
      select(Interest)%>%
      acf(lag.max = 15, plot = FALSE)

    ac.plot <- data.frame(lag = ac$lag, coef = ac$acf) %>%
      ggplot(aes(x = lag, y = coef)) +
      geom_segment(aes(xend=lag, yend=0), color = 'lightpink4')+
      geom_point(size=5, color="lightpink2") +
      geom_hline(aes(yintercept=0),color = 'lightpink4') +
      theme_solarized(light = FALSE) +
      labs(title = "Autocorrelation",y = 'Coefficent', x='LAG') +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            title = element_text(color = "#EEEEEE",),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1") 

    pac <- df%>%
      select(Interest)%>%
      acf(lag.max = 15, plot = FALSE, type = 'partial')

    pac.plot <- data.frame(lag = pac$lag, coef = pac$acf) %>%
      ggplot(aes(x = lag, y = coef)) +
      geom_segment(aes(xend=lag, yend=0), color = 'lightpink4')+
      geom_point(size=5, color="lightpink2") +
      geom_hline(aes(yintercept=0),color = 'lightpink4') +
      theme_solarized(light = FALSE) +
      labs(title = "Partial Autocorrelation",y = 'Coefficent', x='LAG') +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            title = element_text(color = "#EEEEEE",),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            plot.title = element_text(hjust = 0.5)) +
      scale_color_brewer(palette = "Pastel1")

    ac.plot | pac.plot
#---- 

![image](https://user-images.githubusercontent.com/58554631/226738876-f6e059a5-1ea6-4032-9671-53b4420bf690.png)

  
#______________________________________________________________#
#                       Forecast Plot                       ----
  
    line_plot + stat_smooth(color = 'lightblue', alpha = 0.4)|line_plot + 
      geom_ma(ma_fun = EMA, color = 'lightblue', n = 5, size = 2) 

    library(forecast)
    myforecast <- forecast(auto.arima(unlist(c(df$Interest))), 
                           level=c(95), 
                           h=60)
    plot(myforecast)

#---- 

![image](https://user-images.githubusercontent.com/58554631/226738918-85ee802e-429b-443d-ad95-a876cb768437.png)

![image](https://user-images.githubusercontent.com/58554631/226738984-ffd84940-2bdb-453e-94bc-badb53b948f5.png)


#______________________________________________________________#
#                          Chord                            ----

#Preprocessing
#______________________________________________________________#
    
    data = read.csv("migration.csv")
    #group by "from" "current_state" - sum(number of people)

    chord = data %>%
      select(To = 'current_state', 
             From = 'from',
             Qnt = 'number_of_people' ) %>%
      filter(To != From)%>% top_n(200)


    cleanD = chord %>%
      group_by(To,From) %>%
      summarise(TotalQnt = sum(Qnt))%>%
      arrange(desc(TotalQnt))

    write.csv(cleanD,"", row.names = FALSE)


    uniqTo = cleanD %>% distinct(To)
    uniqTo = uniqTo %>% select(col = To)
    uniqFrom = cleanD %>% group_by(From)%>%select(From)%>%distinct(From)
    uniqFrom = uniqFrom %>% select(col = From)


    nodes = rbind(uniqTo, uniqFrom)
    nodes = nodes %>% distinct(col)
#______________________________________________________________#
  
    graph.df <- tbl_graph(nodes = nodes,
                          edges = cleanD,
                          directed = T,
                          node_key = 'col')

    chorde_plot = ggraph(graph.df, layout = "linear", circular=T) +
      geom_edge_arc(aes(width = TotalQnt,
                        color = TotalQnt),
                    alpha=0.7) +
      geom_node_label(aes(label=col,
                          color=col),
                      nudge_y = 0.01) +
      scale_size_binned(range=c(1.5,6)) +
      theme_graph(border = TRUE)


    ggraph(graph.df, layout = "linear", circular=F) +
      geom_edge_arc(aes(width=TotalQnt,
                        color=TotalQnt), 
                    alpha=0.7) +
      geom_node_label(aes(label = col, color = col)) +
      scale_size_binned(range=c(1.5,6)) + 
      theme_graph(border = TRUE)
  
#----
 ![image](https://user-images.githubusercontent.com/58554631/226739273-c0d9e0fb-b3e7-487e-bdb5-0c14f512b4cc.png)
 
#______________________________________________________________#
#                         Sankey                            ---- 
  
#Preprocessing
#______________________________________________________________#
    
    unique_val = unique(c(nodes$col))
    unique_node = data.frame(col = factor(cleanD$To,
                                          levels = unique_val))

    sankey_data<-
      data.frame(From = as.numeric(factor(cleanD$To,
                                          levels = unique_val)),
                 To = as.numeric(factor(cleanD$From,
                                        levels = unique_val)),
                 TotalQnt = as.numeric(cleanD$TotalQnt))

#______________________________________________________________#
    
    sankeyNetwork(Links = sankey_data, Nodes = unique_node,
                  Source = "From",
                  Target = "To", 
                  Value = "TotalQnt",
                  NodeID = "col", 
                  fontSize = 12,
                  nodeWidth = 30)
#----
  ![image](https://user-images.githubusercontent.com/58554631/226739359-9ab17bc8-a993-4720-a409-66b962351014.png)

  
#______________________________________________________________#
#                         HeatMap                           ---- 
  
#Preprocessing
#______________________________________________________________#
    
    hmp = data %>%
      select(To = 'current_state', 
             From = 'from',
             Qnt = 'number_of_people')

    cleanHMP = hmp %>%
      group_by(To,From) %>%
      summarise(Qnt = sum(Qnt))%>%
      add_tally()%>%
      mutate(Qnt = log(Qnt))%>%
      filter(n>1)
  
#______________________________________________________________# 

    cleanHMP %>%
      ggplot(aes(To, From)) +
      geom_tile(aes(fill = Qnt))   +   
      theme_solarized(light = FALSE) +
      labs(title = "Heatmap") +
      theme(text = element_text(family = "fav", 
                                colour = "#EEEEEE",
                                face = "bold"),
            title = element_text(color = "#EEEEEE", size = 20),
            panel.background = element_rect(fill = NA),
            plot.background = element_rect(fill = "#111111"),
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle=60, hjust=1)) +
      scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))+
      coord_fixed()
      
#----
 
 ![image](https://user-images.githubusercontent.com/58554631/226739410-f7aa28c4-1106-4681-9c24-fe62daaa9ff3.png)
  
  
#______________________________________________________________#
#                     Arc + Net Diagr                      ---- 
  
#Preprocessing ----
#______________________________________________________________#
    
    link = "https://scholar.google.com.ua/citations?user=mRdgo0gAAAAJ"
    page = read_html(link)

    researches <- page %>%
      html_nodes(".gsc_a_at") %>% html_text()

    coauthors_row <- page %>%
      html_nodes(".gsc_a_at+ .gs_gray") %>% html_text()

    coauthors_unlisted <- as.data.frame(x = unlist(strsplit(coauthors_row,", ")))

    authors <- distinct(coauthors_unlisted)
    cleaned_auth <- authors %>% 
      select(Surname=  'unlist(strsplit(coauthors_row, ", "))' ) %>%
      filter(Surname != "...")

    str_contains(coauthors_row[1],cleaned_auth$Surname[1])

    AdjacencyMat <- matrix(0,nrow = nrow(cleaned_auth), ncol =  nrow(cleaned_auth))

    rownames(AdjacencyMat) <-cleaned_auth$Surname
    colnames(AdjacencyMat) <-cleaned_auth$Surname

    for(x in cleaned_auth$Surname){
      for(y in coauthors_row ){
        if (str_contains(y,x)){
          for(xx in cleaned_auth$Surname){
            if (str_contains(y,xx)){
              AdjacencyMat[x,xx] =AdjacencyMat[x,xx]+ 1 
            }
          }
        }
      }
    }

    for(x in cleaned_auth$Surname){
      AdjacencyMat[x,x] = 0
    }

    edge_from<-c()
    edge_to<-c()
    edge_value<-c()

    for (i in 1:length(cleaned_auth$Surname)){
      for (j in 1:length(cleaned_auth$Surname)){
        if (AdjacencyMat[cleaned_auth$Surname[i],cleaned_auth$Surname[j]]!=0)
        {
          edge_from <- append(edge_from,cleaned_auth$Surname[i])
          edge_to <- append(edge_to,cleaned_auth$Surname[j])
          edge_value <- append(edge_value,AdjacencyMat[cleaned_auth$Surname[i],cleaned_auth$Surname[j]])
        }
      }
    }
#----
  
    nodes.df <- data.frame(Surname = cleaned_auth$Surname)
    edges.df <- data.frame(from = edge_from,
                           to = edge_to,
                           value =  edge_value)

    graph.df <- tbl_graph(nodes=nodes.df,
                          edges=edges.df,
                          directed = F,
                          node_key = "name")

    ggraph(graph.df,
           layout = "linear",
           circular=T) +
      geom_edge_arc(aes(width=value,
                        color=value), alpha=0.7) +
      geom_node_label(aes(label=Surname,
                          color=Surname),
                      show.legend = F)+
      scale_size_binned(range=c(1.5,6)) +
      theme_graph(border = TRUE)

    ggraph(graph.df,
           layout = "linear",
           circular=F) +
      geom_edge_arc(aes(width=value,
                        color=value),
                    alpha=0.7,
                    show.legend = F) +
      geom_node_label(aes(label=Surname, color=Surname),
                      show.legend = F, size= 3)+ 
      scale_size_binned(range=c(1.5,6)) +
      theme_graph(border = TRUE)
  
#----
  ![image](https://user-images.githubusercontent.com/58554631/226739307-255d4b4b-90bc-4449-8dd1-bf59324791b5.png)

  
#______________________________________________________________#
#                       Dendrogram                          ---- 

    edge_from<-c()
    edge_to<-c()
    for (x in researches){
      edge_from <-append(edge_from,"Origin")
      edge_to <- append(edge_to,x)
    }

    for (i in 1:length(coauthors_row)){
      for (j in 1:length(cleaned_auth$Surname)){
        if (str_contains(coauthors_row[i],cleaned_auth$Surname[j])){
          edge_from <-append(edge_from,researches[i])
          edge_to <- append(edge_to,cleaned_auth$Surname[j])
        }
      }
    }
    edges<- data.frame(from = edge_from,to = edge_to)

    mygraph <- graph_from_data_frame(edges)

    ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
      geom_edge_diagonal() +
      geom_node_point() +
      theme_void()

    name <- unique(c(as.character(edges$from), as.character(edges$to)))
    ggraph(mygraph, layout = 'dendrogram',circular = T) + 
      geom_edge_diagonal() +
      geom_node_text(aes( label=name, filter=leaf))

#----

![image](https://user-images.githubusercontent.com/58554631/226739558-0567e369-d9bb-4f31-b474-ee3015c13eef.png)
