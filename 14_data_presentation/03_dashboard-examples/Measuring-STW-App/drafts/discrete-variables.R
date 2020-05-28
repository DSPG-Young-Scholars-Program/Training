library(ggplot2)

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

Field<-rep(c("Life Sciences","Physical and Earth Sciences","Mathematics and Computer Sciences",
             "Psychology and Social Sciences","Engineering","Education","Humanities and Arts",
             "Other Non-S&E Fields"), rep(5,8))
Percent<-c(6.8,0.3,11.4,6.5,3.2,5.8,0.2,9.6,2.5,3.0,4.5,0,15.8,2.8,2.9,8.4,0.4,7.7,7.7,3.0,
           6.6,0.2,16.9,3.9,2.6,8.2,0.5,5.5,14.7,2.1,7.2,0.4,4.2,3.4,2.7,7.0,0.2,9.7,12.8,2.7)
Race<-rep(c("Hispanic or Latino","Amerian Indian or Alaska Native","Asian",
            "Black or African American","More than One Race"),8)
SED4<-data.frame(Field,Race,Percent)

#Multi-set Bar Chart
ggplot(SED4, aes(fill=Race, y=Percent, x=Field)) +
  geom_bar(position="dodge", colour="black", stat="identity") +
  coord_flip() + xlab("") +
  guides(fill=guide_legend(ncol=2)) +
  #scale_fill_manual(values=theme_SDAD[c(5,1,2,3,6)], name="Race and Ethnicity") +
  theme_SDAD() +
  labs(title="Doctorates Awarded to Minority U.S.and Permanent Residents by Race,Ethnicity and Broad Field of Study: 2017")


