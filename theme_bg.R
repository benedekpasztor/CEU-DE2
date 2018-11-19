theme_bg <- function() {
  # The function sets the basic design of the graphs (axis, grids, titles etc.)
  
  # Import library
  library(RColorBrewer)
  
  # Generate color palette

  palette <- c("white", "grey90", "black", "blue") # global
  color.background = palette[1]
  color.grid.major = palette[2]
  color.grid.minor = palette[2]
  color.axis.text = palette[3]
  color.axis.title = palette[3]
  color.title = palette[3]
  
  palette_brewer <- brewer.pal("Blues", n=9)
  color.fill <- palette[4]
  color.line <- palette[3]
  
  # Chart elements
  
  theme_bw() +
    
    # Chart region
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=palette[3])) +
    
    # Grids
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.5)) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.5)) +
    # theme(axis.ticks=element_blank()) +
    
    # Legend
    
    theme(legend.position="right") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=10,color=color.axis.title)) +
    
    # Title & axis labels
    
    theme(plot.title=element_text(color=color.title, size=14, vjust=1.25, hjust=0.5, face = "bold")) +
    theme(axis.text.x=element_text(size=12,color=color.axis.text, face = "bold")) +
    theme(axis.text.y=element_text(size=12,color=color.axis.text, face = "bold")) +
    theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=0, face = "bold")) +
    theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.25, face = "bold")) +
    
    # Margins
    
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
}
