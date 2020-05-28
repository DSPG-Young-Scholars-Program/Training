theme_SDAD<-function (base_size=12, base_family="sans", style=c("default","darkunica"), bgcolor=NULL)
{
  if (!is.null(bgcolor)) {
    warning("`bgcolor` is deprecated. Use `style` instead.")
    style<-bgcolor
  }
  style<-match.arg(style)
  bgcolor<-switch(style, default="#FFFFFF", darkunica="#2A2A2B")
  ret<-theme(rect=element_rect(fill=bgcolor, linetype=0, colour = NA),
             text=element_text(size=base_size, family=base_family),
             title=element_text(hjust=0.5),
             axis.title.x=element_text(hjust=0.5),
             axis.title.y=element_text(hjust=0.5),
             panel.grid.major.y=element_line(colour="#D8D8D8"),
             panel.grid.minor.y=element_blank(),
             panel.grid.major.x=element_line(colour="#D8D8D8"),
             panel.grid.minor.x=element_blank(),
             panel.border=element_blank(),
             panel.background=element_blank(),
             legend.position="bottom",
             legend.key=element_rect(fill="#FFFFFF"))
  if (style=="darkunica") {
    ret<-(ret + theme(rect=element_rect(fill=bgcolor),
                      text=element_text(colour="#A0A0A3"), 
                      title=element_text(colour="#FFFFFF"),
                      axis.title.x=element_text(colour="#A0A0A3"),
                      axis.title.y=element_text(colour="#A0A0A3"),
                      panel.grid.major.y=element_line(colour="#707073"),
                      legend.title=element_text(colour="#A0A0A3")))
  }
  ret
}



