#Alex Albright (thelittledataset.com & @AllbriteAllday)
#Code for visuals in "This Post is Brought to You by the National Science Foundation"

#Load libraries
library(ggplot2);library(ggthemes);library(plyr);library(reshape);library(grid);library(scales);library(RColorBrewer);library(gridExtra)

#Define theme for the visuals - thanks Max Woolf (@minimaxir) for tips from his blog
 my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="bottom", legend.title = element_blank()) +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=7,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=16, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=6,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=6,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=9,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=9,color=color.axis.title, vjust=1.8, face="italic")) +

  #Format title and facet_wrap title
  theme(strip.text = element_text(size=6.5), plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = 1, hjust=0.5))+
    
  # Plot margins
  theme(plot.margin = unit(c(.2, .2, .2, .2), "cm"))
}

#Part I: Make treepmap!
#I wish I could make these using ggplot...but for now I am sticking with the treemap package
library(treemap)

#Load data  
data <- read.csv('treemap.csv', check.names=F)
pal<- c("#00441b", "#045a8d", "#878787", "#bf812d", "#9e9ac8", "#f03b20", "#000000", "#02818a")
pal1<- c("#78c679", "#3690c0", "#878787", "#bf812d", "#807dba", "#fc4e2a", "#fee0b6", "#02818a")

treemap(data, 
        index=c("Science Degree"), 
        vSize="2012", 
        vColor = "Subdomain", 
        type="categorical", 
        title="Treemap of Science PhD Degrees Obtained in 2012 \n Data via NSF & Visual via Alex Albright (thelittledataset.com)", 
        title.legend="Parent Science Category", 
        palette=pal1,
        #algorithm = "pivotSize",
        algorithm = "squarified", 
        sortID="2012",
        fontsize.title = 14,
        fontfamily.title = "serif",
        fontfamily.labels = "serif", 
        fontfamily.legend = "serif",
        fontcolor.labels = "black",
        border.col = "white",
        fontsize.labels = 10,
        fontsize.legend = 10,
        lowerbound.cex.labels = 0.6,
        force.print.labels = T,
        overlap.labels = 1,
        position.legend="bottom",
        #aspRatio= 1.6
        aspRatio= 1.8)

#Part II: Make the visual with raw numbers!
sci_more <- read.csv('sciences3.csv', check.names=F)

sci_more1 <- reshape(sci_more, 
                     varying = c("2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012"), 
                     v.names = "count",
                     timevar = "year", 
                     times = c("2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012"), 
                     direction = "long")

sci_more1 <- sci_more1[order(sci_more1$id),]
sci_moreall<-sci_more1[!(sci_more1$Group=="U.S. citizen and permanent resident" | sci_more1$Group=="All"),]

pal1<- c("#ffffd9", "#c7e9b4", "#41b6c4", "#225ea8", "#081d58", "#f03b20", "#000000")

p<-ggplot(data=sci_moreall, aes(x=year, y=count, group=Group, fill=Group, color=Group)) + 
  scale_fill_manual(values = pal1) + 
  scale_color_manual(values = pal1) +
  my_theme()+
  facet_wrap(~Type, ncol=6, scales="free")+
  labs(title="", x="Note: The ethnic and racial groups provided are subsets of the 'US citizen and permanent resident' category", y="")+
  ggtitle(expression(atop(bold("Demographic Breakdown of The Science PhD's"), 
                          atop(italic("NSF Data on Degrees Earned, 2002-2012"), 
                               atop(italic("Created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))
#Stacked area graph
p+geom_area(aes(fill=Group), position='stack')+scale_x_discrete(breaks=c("2002","2007","2012"))

#Part III: Make the visual with percents!
sci_more <- read.csv('sciences3.csv', check.names=F)

sci_more1 <- reshape(sci_more, 
                     varying = c("2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012"), 
                     v.names = "count",
                     timevar = "year", 
                     times = c("2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012"), 
                     direction = "long")

sci_more1 <- sci_more1[order(sci_more1$id),]
sci_moreall<-sci_more1[!(sci_more1$Group=="U.S. citizen and permanent resident" | sci_more1$Group=="All"),]

agg<-aggregate(count ~ Type + year, data = sci_moreall, sum)
sci_moreall<-merge(sci_moreall, agg, by=c("Type", "year"))

sci_moreall$perc<-sci_moreall$count.x/sci_moreall$count.y
sci_moreall <- sci_moreall[order(sci_moreall$id),]

pal1<- c("#ffffd9", "#c7e9b4", "#41b6c4", "#225ea8", "#081d58", "#f03b20", "#000000")

p<-ggplot(data=sci_moreall, aes(x=year, y=perc, group=Group, fill=Group, color=Group)) + 
  scale_fill_manual(values = pal1) + 
  scale_color_manual(values = pal1) +
  my_theme()+
  facet_wrap(~Type, ncol=6)+
  labs(title="", x="Note: The ethnic and racial groups provided are subsets of the 'US citizen and permanent resident' category", y="")+
  scale_y_continuous(labels = percent_format())+
  ggtitle(expression(atop(bold("Demographic Breakdown of The Science PhD's"), 
                          atop(italic("NSF Data on Degrees Earned, 2002-2012"), 
                               atop(italic("Created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))
#Stacked area graph
p+geom_area(aes(fill=Group), position='stack')+ scale_x_discrete(breaks=c("2002","2007","2012"))
#Nightingale graphs
p+geom_bar(stat="identity", aes(fill=Group), position='stack')+ coord_polar()+ facet_wrap(~Type, ncol=6)
#Line graphs
p+geom_line(size=1)+facet_wrap(~Type, ncol=6) + scale_x_discrete(breaks=c("2002","2007","2012"))
