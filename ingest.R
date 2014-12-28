library(methods)

ref2014.colClasses <- list()
ref2014.factor.levels <- list()

ref2014 <- read.csv("csv/REF2014_Results.csv", header=TRUE, skip=4, colClasses="character", na.strings=" ")[-(1:3),]

ref2014$UOA <- as.numeric(ref2014$UOA)

for (i in 1:length(colnames(ref2014))) {
    ref2014.colClasses[[colnames(ref2014)[i]]] <-
        c("factor", "character", "numeric", "factor", "factor", "character", "factor", "character",
          "character", "factor", "numeric", rep("numeric",5))[i]
}

hesa.colnames <- c("INSTID", "UKPRN", "Region", "Institution",
                   "UOA", "UnitOfAssessment", "msubId", "EligibleFte")
hesa.colClasses <- c("factor", "factor", "factor", "character",
                     "factor", "character", "factor", "numeric")
ref2014.colClasses[["INSTID"]] <- "factor"
ref2014.colClasses[["Region"]] <- "factor"
ref2014.colClasses[["EligibleFte"]] <- "numeric"



hesa2014.raw <- droplevels(
    read.csv("csv/290183_REF_Contextual_table_1314.csv", na.strings=c("Z", ".."), skip=8,
             header=FALSE, col.names=hesa.colnames, colClasses="character")[-(2559:2563),])
hesa2014.qmul <- droplevels(
    read.csv("csv/290183_0139_ReplacementFTEs.csv", na.strings="Z", skip=9,
             header=FALSE, col.names=hesa.colnames, colClasses="character")[-(12:15),])
hesa2014.cardiff <- droplevels(
    read.csv("csv/290183_0179_ReplacementFTEs.csv", na.strings="Z", skip=9,
             header=FALSE, col.names=hesa.colnames, colClasses="character")[-(28:31),])

hesa2014.fixup <- function(df) {
    df <- df[,c("INSTID", "UOA", "msubId", "EligibleFte")]
    for(col in c("INSTID", "UOA", "msubId")) {
        df[,col] <- as.character(df[,col])
    }
    tmp <- merge(hesa2014.raw, df, by=c("INSTID", "UOA", "msubId"), all.x=TRUE)
    tmp$EligibleFte <- ifelse(!is.na(tmp$EligibleFte.y), tmp$EligibleFte.y, tmp$EligibleFte.x)
    tmp[,colnames(hesa2014.raw)]
}

hesa2014 <- hesa2014.fixup(rbind(hesa2014.qmul, hesa2014.cardiff))

stopifnot(sum(is.na(hesa2014$EligibleFte)) == 0)

### UCL / Institute of Education
hesa2014[hesa2014$UKPRN == "10007784" & hesa2014$UOA == "25", "msubId"] <- "B"
hesa2014[hesa2014$UKPRN == "10007766" & hesa2014$UOA == "25", "msubId"] <- "A"
hesa2014[hesa2014$UKPRN == "10007766" & hesa2014$UOA == "25", "UKPRN"] <- "10007784"

hesa2014$UOA <- as.numeric(hesa2014$UOA)

for (c in colnames(ref2014)) {
    ref2014[[c]] <-
        if (ref2014.colClasses[[c]] == "factor") {
            ref2014.factor.levels[[c]] <- unique(c(ref2014[[c]], hesa2014[[c]]))
            factor(ref2014[[c]], levels=ref2014.factor.levels[[c]])
        } else {
            as(ref2014[[c]], ref2014.colClasses[[c]])
        }
}

for (i in 1:length(hesa.colnames)) {
    hesa2014[[hesa.colnames[i]]] <-
        if (hesa.colClasses[i] == "factor") {
            ls <- ref2014.factor.levels[[hesa.colnames[i]]]
            if(!is.null(ls)) {
                factor(hesa2014[[hesa.colnames[i]]], levels=ls)
            } else {
                factor(hesa2014[[hesa.colnames[i]]])
            }
        } else {
            as(hesa2014[[hesa.colnames[i]]], hesa.colClasses[i])
        }
}

dir.create("R/data")
save(hesa2014, file="R/data/hesa2014.rda")
save(ref2014, file="R/data/ref2014.rda")
