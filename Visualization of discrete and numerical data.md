    library(ggplot2)
    library(dplyr)
    library(vcd)
    library(GGally)
    library(MASS)
    library(RColorBrewer)
    library(treemapify)
    library(ggplotify)

    options(scipen=999)
    theme_set(theme_bw())

    path <- "CARS_1.csv"
      df <- read.csv(path, header = TRUE) 

  
  
  ## Lollipop diagram :)
  
    df%>%
      distinct(body_type, 
               rating)%>%
      ggplot(aes(x=body_type,
                 y=rating))+
      geom_point(size=3, 
                 colour ="#098080", 
                 size = 5)+
      geom_segment(aes(x=body_type,
                       xend=body_type,
                       y=0,
                       yend=rating),
                   colour ="#098080")+
      theme(plot.title = element_text(size = 20, 
                                 face = "bold",
                                 hjust = 0.5),
            panel.grid = element_blank())+
      labs(title = "Lollipop",
           x = "Type",
           y = "Rating")+ coord_flip()
         
![image](https://user-images.githubusercontent.com/58554631/226740940-5c95bdc3-9235-4516-a9ab-452d3e60810f.png)

    donut = df%>%
      count(fuel_type)%>%
      mutate(
        frc = n/sum(n),
        ymax = cumsum(frc),
        ymin = c(0, head(ymax, n=-1)),
        lpos = (ymax + ymin)/2,
        labelV = paste0(fuel_type, "\n Value: ", n)
      )

    donut %>%
      ggplot(aes(ymax=ymax,
                       ymin=ymin, 
                       xmax=4, 
                       xmin=3,
                       fill=fuel_type)) +
      geom_rect() +
      geom_label( x=3.5,
                  aes(y=lpos,
                        label=labelV),
                  size=3) +
      scale_fill_brewer(palette=4) +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      theme_void() +
      theme(legend.position = "none")
      
![image](https://user-images.githubusercontent.com/58554631/226741038-d822fd28-ce88-40d3-9308-17e4e5779a83.png)

## Treemap

    df%>%
      ggplot(aes(area = reviews_count, #
                 fill = body_type,
                 label = paste(car_name,
                               "\n",
                               prettyNum(reviews_count,
                                         big.mark=","))))+
      geom_treemap()+
      geom_treemap_text(colour="white",
                        place = "center",
                        fontface = "bold.italic")+
      theme(legend.title=element_text(size=13, 
                                      face = "bold"),
            title = element_text(size = 20, 
                                 face = "bold",
                                 hjust = 0.5),
            panel.grid = element_blank())+
      labs(fill = "Type")+
      scale_fill_brewer(palette = "BrBG")

![image](https://user-images.githubusercontent.com/58554631/226741107-57222925-0614-4afe-be97-f1c428959164.png)

## Parallel Plot
    df %>%
      slice_sample(n=100) %>%
      ggparcoord(
        groupColumn = "body_type", columns=c(10:12,2),
        splineFactor = 20, showPoints = T, alphaLines = 0.8,
        scaleSummary = "median",
      ) +
      scale_color_brewer(palette = "Paired") +
      facet_wrap(~transmission_type)+
      theme(legend.title=element_text(size=13, 
                                       face = "bold",
                                       color = "darkslategray"),
            title = element_text(size = 20, face = "bold"),
            legend.position = "bottom",
            panel.grid = element_blank())+
      labs(title = "Parallel Plot",
           x = "",
           y="",
           color = "Type")
           
![image](https://user-images.githubusercontent.com/58554631/226741270-a0354423-3c77-4a66-a453-3ed6608e48db.png)

## Scatter Plot w line
    df%>%
      ggplot(aes(x = starting_price, y = ending_price))+ #ending_price
      geom_point(aes(col=body_type, size = rating),
                 alpha= 0.5)+
      geom_smooth(method="loess", 
                  se=F, 
                  color = "firebrick",
                  size = 1.3)+
      theme(plot.title = element_text(size = 15, face = "bold"),
            title = element_text(size = 12,face="bold.italic"),
            panel.grid = element_blank())+
      labs(title = "ScatterPlot",
         y="Starting Price", 
         x="Ending Price",
         size="Rating",col="Type")

![image](https://user-images.githubusercontent.com/58554631/226741336-08de1063-5a9b-4a6d-bf42-53c1ea28e31d.png)
