ref2014.gpa <- function(df) {
    with(df, (4*FourStar+3*ThreeStar+2*TwoStar+OneStar)/100)
}

ref2014.inst <- ref2014 %>%
    group_by(UKPRN,Profile) %>%
        summarise(FourStar=weighted.mean(FourStar, StaffFte, na.rm=TRUE),
                  ThreeStar=weighted.mean(ThreeStar, StaffFte, na.rm=TRUE),
                  TwoStar=weighted.mean(TwoStar, StaffFte, na.rm=TRUE),
                  OneStar=weighted.mean(OneStar, StaffFte, na.rm=TRUE),
                  Unclassified=weighted.mean(Unclassified, StaffFte, na.rm=TRUE),
                  Institution=first(Institution),
                  nUOA=n(),
                  StaffFte=sum(StaffFte, na.rm=TRUE),
                  EligibleFte=sum(EligibleFte, na.rm=TRUE)) %>%
                      ungroup

ref2014.inst$GPA <- ref2014.gpa(ref2014.inst)
ref2014.inst$GPA.Intensity <- ref2014.inst$GPA * ref2014.inst$StaffFte / ref2014.inst$EligibleFte

ref2014.inst.overall <- ref2014 %>%
    filter(is.na(Profile) | Profile == "Overall") %>%
        group_by(UKPRN) %>%
            summarise(FourStar=weighted.mean(FourStar, StaffFte, na.rm=TRUE),
                      ThreeStar=weighted.mean(ThreeStar, StaffFte, na.rm=TRUE),
                      TwoStar=weighted.mean(TwoStar, StaffFte, na.rm=TRUE),
                      OneStar=weighted.mean(OneStar, StaffFte, na.rm=TRUE),
                      Unclassified=weighted.mean(Unclassified, StaffFte, na.rm=TRUE),
                      Institution=first(Institution[!is.na(Institution)]),
                      nUOA=n(),
                      StaffFte=sum(StaffFte, na.rm=TRUE),
                      EligibleFte=sum(EligibleFte, na.rm=TRUE)) %>%
                          filter(StaffFte < EligibleFte, StaffFte > 0)

ref2014.inst.overall$GPA <- ref2014.gpa(ref2014.inst.overall)
ref2014.inst.overall$GPA.Intensity <- ref2014.inst.overall$GPA * ref2014.inst.overall$StaffFte/ref2014.inst.overall$EligibleFte

ref2014.inst.overall$FourStar.Intensity <- ref2014.inst.overall$FourStar * ref2014.inst.overall$StaffFte/ref2014.inst.overall$EligibleFte

ref2014.inst.overall <- ref2014.inst.overall %>%
    arrange(-GPA) %>% mutate(GPA.Rank=1:nrow(ref2014.inst.overall)) %>%
        arrange(-GPA.Intensity) %>% mutate(GPA.Intensity.Rank=1:nrow(ref2014.inst.overall)) %>%
            arrange(-FourStar) %>% mutate(FourStar.Rank=1:nrow(ref2014.inst.overall)) %>%
                arrange(-FourStar.Intensity) %>% mutate(FourStar.Intensity.Rank=1:nrow(ref2014.inst.overall))

ref2014.inst.overall <- ref2014.inst.overall %>%
    arrange(-(3*FourStar+ThreeStar)) %>% mutate(QR=3*FourStar+ThreeStar, QR.Rank=1:nrow(ref2014.inst.overall)) %>%
        arrange(-(3*FourStar+ThreeStar)*StaffFte/EligibleFte) %>% mutate(QR.Intensity=(3*FourStar+ThreeStar)*StaffFte/EligibleFte, QR.Intensity.Rank=1:nrow(ref2014.inst.overall))

library(ggplot2)
library(scales)
months <- 1
l11<-paste(ref2014.inst.overall$Institution, ref2014.inst.overall$QR.Rank, sep=" ")
l13<-paste(ref2014.inst.overall$QR.Intensity.Rank, ref2014.inst.overall$Institution, sep=" ")
p<-ggplot(ref2014.inst.overall) + geom_segment(aes(x=0,xend=months,y=152-QR.Rank,yend=152-QR.Intensity.Rank, alpha=0.5+0.5*abs(QR.Rank-QR.Intensity.Rank)/152),size=.5)
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())
p<-p + theme(legend.position="none")
p<-p + xlab("") + ylab("Rank")
p<-p + theme(axis.title.y=element_text(vjust=3))
p<-p + xlim((0-1),(months+1))
p<-p + ylim(0,160)
p<-p + geom_text(label=l13, y=152-ref2014.inst.overall$QR.Intensity.Rank, x=months+0.025,hjust=0,size=3.5)
p<-p + geom_text(label=l11, y=152-ref2014.inst.overall$QR.Rank, x=-0.025,hjust=1,size=3.5)
p<-p + geom_text(label="QR (3:1) Rank", x=0,     y=154,hjust=1,size=5)
p<-p + geom_text(label="QR (3:1) Intensity Rank", x=months,y=154,hjust=0,size=5)
p<-p + geom_text(label="Overall", x=0.5, y=158, size=4)
p

library(ggplot2)
library(scales)
months <- 1
l11<-sprintf("%s %.0f", ref2014.inst.overall$Institution, ref2014.inst.overall$QR)
l13<-sprintf("%.0f %s", ref2014.inst.overall$QR.Intensity, ref2014.inst.overall$Institution)
p<-ggplot(ref2014.inst.overall) + geom_segment(aes(x=0,xend=months,y=QR,yend=QR.Intensity, alpha=0.5+0.5*abs(QR-QR.Intensity)/(QR+QR.Intensity)),size=.5)
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())
p<-p + theme(legend.position="none")
p<-p + xlab("") + ylab("QR")
p<-p + theme(axis.title.y=element_text(vjust=3))
p<-p + xlim((0-1),(months+1))
p<-p + ylim(0,214)
p<-p + geom_text(label=l13, y=ref2014.inst.overall$QR.Intensity, x=months+0.025,hjust=0,size=3.5)
p<-p + geom_text(label=l11, y=ref2014.inst.overall$QR, x=-0.025,hjust=1,size=3.5)
p<-p + geom_text(label="QR (3:1)", x=0,     y=210,hjust=1,size=5)
p<-p + geom_text(label="QR (3:1) Intensity", x=months,y=210,hjust=0,size=5)
p<-p + geom_text(label="Overall", x=0.5, y=212, size=4)
p


ref2014.inst.outputs <- ref2014 %>%
    filter(is.na(Profile) | Profile == "Outputs") %>%
        group_by(UKPRN) %>%
            summarise(FourStar=weighted.mean(FourStar, StaffFte, na.rm=TRUE),
                      ThreeStar=weighted.mean(ThreeStar, StaffFte, na.rm=TRUE),
                      TwoStar=weighted.mean(TwoStar, StaffFte, na.rm=TRUE),
                      OneStar=weighted.mean(OneStar, StaffFte, na.rm=TRUE),
                      Unclassified=weighted.mean(Unclassified, StaffFte, na.rm=TRUE),
                      Institution=first(Institution[!is.na(Institution)]),
                      nUOA=n(),
                      StaffFte=sum(StaffFte, na.rm=TRUE),
                      EligibleFte=sum(EligibleFte, na.rm=TRUE)) %>%
                          filter(StaffFte < EligibleFte, StaffFte > 0)

ref2014.inst.outputs$GPA <- ref2014.gpa(ref2014.inst.outputs)
ref2014.inst.outputs$GPA.Intensity <- ref2014.inst.outputs$GPA * ref2014.inst.outputs$StaffFte/ref2014.inst.outputs$EligibleFte

ref2014.inst.outputs$FourStar.Intensity <- ref2014.inst.outputs$FourStar * ref2014.inst.outputs$StaffFte/ref2014.inst.outputs$EligibleFte

ref2014.inst.outputs <- ref2014.inst.outputs %>%
    arrange(-GPA) %>% mutate(GPA.Rank=1:nrow(ref2014.inst.outputs)) %>%
        arrange(-GPA.Intensity) %>% mutate(GPA.Intensity.Rank=1:nrow(ref2014.inst.outputs)) %>%
            arrange(-FourStar) %>% mutate(FourStar.Rank=1:nrow(ref2014.inst.outputs)) %>%
                arrange(-FourStar.Intensity) %>% mutate(FourStar.Intensity.Rank=1:nrow(ref2014.inst.outputs))

ref2014.inst.outputs <- ref2014.inst.outputs %>%
    arrange(-(3*FourStar+ThreeStar)) %>% mutate(QR.Rank=1:nrow(ref2014.inst.outputs)) %>%
        arrange(-(3*FourStar+ThreeStar)*StaffFte/EligibleFte) %>% mutate(QR.Intensity.Rank=1:nrow(ref2014.inst.outputs))

library(ggplot2)
library(scales)
months <- 1
l11<-paste(ref2014.inst.outputs$Institution, ref2014.inst.outputs$FourStar.Rank, sep=" ")
l13<-paste(ref2014.inst.outputs$FourStar.Intensity.Rank, ref2014.inst.outputs$Institution, sep=" ")
p<-ggplot(ref2014.inst.outputs) + geom_segment(aes(x=0,xend=months,y=152-FourStar.Rank,yend=152-FourStar.Intensity.Rank, alpha=0.5+0.5*abs(FourStar.Rank-FourStar.Intensity.Rank)/152),size=.5)
p<-p + theme(panel.background = element_blank())
p<-p + theme(panel.grid=element_blank())
p<-p + theme(axis.ticks=element_blank())
p<-p + theme(axis.text=element_blank())
p<-p + theme(panel.border=element_blank())
p<-p + theme(legend.position="none")
p<-p + xlab("") + ylab("Rank")
p<-p + theme(axis.title.y=element_text(vjust=3))
p<-p + xlim((0-1),(months+1))
p<-p + ylim(0,160)
p<-p + geom_text(label=l13, y=152-ref2014.inst.outputs$FourStar.Intensity.Rank, x=months+0.025,hjust=0,size=3.5)
p<-p + geom_text(label=l11, y=152-ref2014.inst.outputs$FourStar.Rank, x=-0.025,hjust=1,size=3.5)
p<-p + geom_text(label="4* Rank", x=0,     y=154,hjust=1,size=5)
p<-p + geom_text(label="4* Intensity Rank", x=months,y=154,hjust=0,size=5)
p<-p + geom_text(label="Outputs", x=0.5, y=158, size=4)
p


ref2014 <- with(ref2014, cbind(ref2014, GPA=4*FourStar+3*ThreeStar+2*TwoStar+1*OneStar))
