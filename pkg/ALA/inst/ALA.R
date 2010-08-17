### ALA.R --- Preparing data for ALA package
## Author: Sebastian P. Luque
## Created: Fri Aug 13 22:35:06 2010 (UTC)
## Last-Updated: Tue Aug 17 16:52:34 2010 (UTC)
##           By: Sebastian P. Luque
## copyright (c) 2010 Sebastian P. Luque
###
### Commentary: 
##
## Initial code provided by Doug Bates.
##
## The code below assumes my local convention:
##
## ALA
##  ALA
##  data
##
## where the ALA subdirectory holds the package source, and data is a
## subdirectory where we store and pre-process the data sets.
##
## I used a short Shell script to extract and write the data from the
## source files (write_data.sh), which calls an awk script (read_data.awk),
## since I got a warning that "^[[:space:]]*$" in Doug's script was not
## correct for my current locale, so felt more confident using these
## scripts.
### -----------------------------------------------------------------------
### Code:

library(reshape)

###_ + Download and extract the data

baseURL <- "http://biosun1.harvard.edu/~fitzmaur/ala/"

fnms <- c("lead", "tlc", "smoking", "fev1", "headache", "bpd", "tvsfp", "ntp",
          "melanoma", "obesity", "muscatine", "epilepsy", "ecg", "amenorrhea",
          "exercise", "dental", "chd", "leprosy", "cholesterol", "rat", "respir",
          "tumor", "cd4", "toenail", "skin", "fat", "ccs")
## Download to local directory
download <- function(nm) {
    download.file(url=paste(baseURL, nm, ".txt", sep=""),
                  destfile=paste("../../data/", nm, ".txt", sep=""))
}
for (nm in fnms) download(nm)
## Extract and write the data section of the files
system(paste("./write_data.sh", "../../data/*.txt"))

for (f in fnms) {
    assign(f, read.table(paste("../../data/", f, sep=""), na.strings='.'),
           env=.GlobalEnv)
}

print(ls.str())


###_ + Prepare data sets

###_  : amenorrhea

names(amenorrhea) <- c("id", "dose", "day", "amenorrhea")
amenorrhea <- within(amenorrhea, {
    id <- factor(id)
    dose <- factor(dose, labels=c("low", "high"))
    day <- seq(90, 360, by=90)
    amenorrhea <- factor(amenorrhea, labels=c("no", "yes"))
})

str(amenorrhea)
summary(amenorrhea)
nrow(unique(subset(amenorrhea, select=c(id, dose))))

promptData(amenorrhea, "../man/amenorrhea.Rd")
save(amenorrhea, file="../data/amenorrhea.rda")


###_  : bpd

names(bpd) <- c("bpd", "birth.weight", "gestational.age", "toxemia")
bpd <- data.frame(id=factor(seq(nrow(bpd))), bpd)
bpd <- within(bpd, {
    toxemia <- factor(toxemia, labels=c("no", "yes"))
})

str(bpd)
summary(bpd)

## No need for tests here, since they're all numeric data.
promptData(bpd, "../man/bpd.Rd")
save(bpd, file="../data/bpd.rda")


###_  : ccs

names(ccs) <- c("id", "problems", "parent.status", "parent", "teacher")
ccs <- within(ccs, {
    id <- factor(id)
    problems <- factor(problems, labels=c("good", "bad"))
    parent.status <- factor(parent.status, labels=c("otherwise", "single"))
    parent <- factor(parent, labels=c("no", "yes"))
    teacher <- factor(teacher, labels=c("no", "yes"))
})
ccs.m <- melt(ccs, measure.vars=4:5, variable_name="respondent")
ccs <- ccs.m
names(ccs)[5] <- "behaviour"

str(ccs)
summary(ccs)
nrow(unique(subset(ccs, select=c(id, problems))))
nrow(unique(subset(ccs, select=c(id, problems, parent.status))))

promptData(ccs, "../man/ccs.Rd")
save(ccs, file="../data/ccs.rda")


###_  : cd4

names(cd4) <- c("id", "treatment", "age", "gender", "week", "logCD4")
cd4 <- within(cd4, {
    id <- factor(id)
    treatment <- factor(treatment,
                        labels=c("zX400d", "zA225z", "zA400d", "zA400dA400n"))
    gender <- factor(gender, labels=c("F", "M"))
})

str(cd4)
summary(cd4)
nrow(unique(subset(cd4, select=c(id, treatment))))
nrow(unique(subset(cd4, select=c(id, treatment, gender))))

promptData(cd4, "../man/cd4.Rd")
save(cd4, file="../data/cd4.rda")


###_  : chd

## Not sure what "person years follow-up" is
names(chd) <- c("smoking", "blood.pressure", "behaviour", "nCHD",
                "years.followup")
chd <- within(chd, {
    smoking <- factor(smoking)
    blood.pressure <- factor(blood.pressure,
                             labels=c("< 140", ">= 140"))
    behaviour <- factor(behaviour, labels=c("B", "A"))
})

str(chd)
summary(chd)
nrow(unique(subset(chd, select=c(smoking, blood.pressure))))
nrow(unique(subset(chd, select=c(smoking, blood.pressure, years.followup))))

promptData(chd, "../man/chd.Rd")
save(chd, file="../data/chd.rda")


###_  : cholesterol

## Number variables are "month"
names(cholesterol) <- c("treatment", "id", "0", "6", "12", "20", "24")
cholesterol <- within(cholesterol, {
    treatment <- factor(treatment, labels=c("chenodiol", "placebo"))
    id <- factor(id)
})
cholesterol.m <- melt(cholesterol, measure.vars=3:7,
                      variable_name="month")
cholesterol <- with(cholesterol.m,
                    data.frame(id, treatment,
                               month=as.numeric(as.character(month)), value))
cholesterol <- with(cholesterol,
                    cholesterol[order(id, treatment, month), ])
names(cholesterol)[4] <- "cholesterol"
cholesterol

str(cholesterol)
summary(cholesterol)
nrow(unique(subset(cholesterol, select=c(id))))
nrow(unique(subset(cholesterol, select=c(id, treatment))))

promptData(cholesterol, "../man/cholesterol.Rd")
save(cholesterol, file="../data/cholesterol.rda")


###_  : dental -- SAME AS Orthodont in nlme

## Skip this and make a note in man/ALA-package.Rd
## Variable numbers are age
names(dental) <- c("id", "gender", "8", "10", "12", "14")


###_  : ecg

## Number variables are period containing the ECG response
names(ecg) <- c("sequence", "1", "2", "n")
ecg <- ecg[rep(seq(nrow(ecg)), ecg$n), 1:3]
ecg <- within(ecg, {
    id <- seq(nrow(ecg))
    sequence <- factor(sequence, labels=c("P->A", "A->P"))
})
ecg.m <- melt(ecg, measure.vars=2:3, variable_name="period")
ecg <- with(ecg.m, data.frame(id, sequence, period, value))
## ecg <- within(ecg, {period <- as.numeric(as.character(period))})
ecg <- with(ecg, ecg[order(id, period, sequence), ])
names(ecg)[4] <- "ecg"
ecg <- within(ecg, {
    id <- factor(id)
    ecg <- factor(ecg, labels=c("normal", "abnormal"))
})
ecg

str(ecg)
summary(ecg)
nrow(unique(subset(ecg, select=c(id))))
nrow(unique(subset(ecg, select=c(id, sequence))))
nrow(unique(subset(ecg, select=c(id, sequence, period))))

promptData(ecg, "../man/ecg.Rd")
save(ecg, file="../data/ecg.rda")


###_  : epilepsy

## Number variables are times (at 2-week intervals)
names(epilepsy) <- c("id", "treatment", "age", "0", "2", "4", "6", "8")
epilepsy <- within(epilepsy, {
    id <- factor(id)
    treatment <- factor(treatment, labels=c("placebo", "progabide"))
})
epilepsy.m <- melt(epilepsy, measure.vars=4:8, variable_name="week")
epilepsy <- epilepsy.m
epilepsy <- within(epilepsy, {week <- as.numeric(as.character(week))})
epilepsy <- with(epilepsy, epilepsy[order(id, treatment, week), ])
names(epilepsy)[5] <- "nSeizures"
epilepsy

str(epilepsy)
summary(epilepsy)
nrow(unique(subset(epilepsy, select=c(id))))
nrow(unique(subset(epilepsy, select=c(id, treatment))))
nrow(unique(subset(epilepsy, select=c(id, treatment, age))))

promptData(epilepsy, "../man/epilepsy.Rd")
save(epilepsy, file="../data/epilepsy.rda")


###_  : exercise

## Number variables represent days
names(exercise) <- c("id", "treatment", "0", "2", "4", "6",
                     "8", "10", "12")
exercise <- within(exercise, {
    id <- factor(id)
    treatment <- factor(treatment, labels=c("repetitions", "weights"))
})
exercise.m <- melt(exercise, measure.vars=3:9, variable_name="day")
exercise <- exercise.m
exercise <- within(exercise, {day <- as.numeric(as.character(day))})
exercise <- with(exercise, exercise[order(id, treatment, day), ])
names(exercise)[4] <- "strength"
exercise

str(exercise)
summary(exercise)
nrow(unique(subset(exercise, select=c(id))))
nrow(unique(subset(exercise, select=c(id, treatment))))

promptData(exercise, "../man/exercise.Rd")
save(exercise, file="../data/exercise.rda")


###_  : fat

names(fat) <- c("id", "age", "age.menarche", "time.menarche", "percent.fat")
fat <- within(fat, {
    id <- factor(id)
})

str(fat)
summary(fat)
nrow(unique(subset(fat, select=c(id))))
nrow(unique(subset(fat, select=c(id, age.menarche))))

promptData(fat, "../man/fat.Rd")
save(fat, file="../data/fat.rda")


###_  : fev1

names(fev1) <- c("id", "height", "age", "height0", "age0", "logFEV1")
fev1 <- within(fev1, {
    id <- factor(id)
})
## Check whether initial time variables are needed
by(fev1, fev1$id, function(x) {
    c(x[1, "age"] - x[1, "age0"], x[1, "height"] - x[1, "height0"])
})            # no, initial variables not needed, but leave for convenience
fev1 <- with(fev1,
             data.frame(id, age, height, age0, height0, logFEV1))

str(fev1)
summary(fev1)
nrow(unique(subset(fev1, select=c(id))))
nrow(unique(subset(fev1, select=c(id, age0))))

promptData(fev1, "../man/fev1.Rd")
save(fev1, file="../data/fev1.rda")


###_  : headache

## The 3rd column should be removed, since it's a duplicate of the 6th
headache <- subset(headache, select=c(1, 2, 4:7))
names(headache) <- c("id", "center", "treatment.seq", "period", "treatment",
                     "relief")
headache <- within(headache, {
    id <- factor(id)
    center <- factor(center, labels=paste("C", unique(center), sep=""))
    treatment.seq <- factor(treatment.seq,
                            labels=c("AB", "BA", "AP", "PA", "BP", "PB"))
    period <- factor(period)
    treatment <- factor(treatment)
})

str(headache)
summary(headache)
nrow(unique(subset(headache, select=c(id))))
nrow(unique(subset(headache, select=c(id, period))))
nrow(unique(subset(headache, select=c(id, period, treatment))))

promptData(headache, "../man/headache.Rd")
save(headache, file="../data/headache.rda")


###_  : lead

## Number variables represent weeks
names(lead) <- c("id", "0", "1", "4", "6")
lead <- within(lead, {
    id <- factor(id)
})
lead.m <- melt(lead, measure.vars=2:5, variable_name="week")
lead <- lead.m
lead <- within(lead, {week <- as.numeric(as.character(week))})
lead <- with(lead, lead[order(id, week), ])
names(lead)[3] <- "lead"
lead

str(lead)
summary(lead)
nrow(unique(subset(lead, select=c(id))))

promptData(lead, "../man/lead.Rd")
save(lead, file="../data/lead.rda")


###_  : leprosy

names(leprosy) <- c("drug", "pre", "post")
## We need to add subject ID
leprosy <- with(leprosy,
                data.frame(id=factor(seq(30)), drug, pre, post))
## Reshape
leprosy.m <- melt(leprosy, measure.vars=3:4, variable_name="period")
leprosy <- leprosy.m
leprosy <- with(leprosy, leprosy[order(id, period), ])
names(leprosy)[4] <- "nBacilli"
leprosy

str(leprosy)
summary(leprosy)
nrow(unique(subset(leprosy, select=c(id))))
nrow(unique(subset(leprosy, select=c(id, drug))))
nrow(unique(subset(leprosy, select=c(id, drug, period))))

promptData(leprosy, "../man/leprosy.Rd")
save(leprosy, file="../data/leprosy.rda")


###_  : melanoma -- SAME as Mmmec in mlmRev

## We skip this and add a note in man/ALA-package.Rd
names(melanoma) <- c("region", "county", "observed.deaths", "expected.deaths",
                     "exposure")


###_  : muscatine

names(muscatine) <- c("id", "gender", "age0", "age", "year", "obesity")
muscatine <- within(muscatine, {
    id <- factor(id)
    gender <- factor(gender, labels=c("M", "F"))
    year <- seq(1977, 1981, 2)[year]
    obesity <- factor(obesity, labels=c("non-obese", "obese"))
})
## Check whether initial age is needed
by(muscatine, muscatine$id, function(x) {
    x[1, "age"] - x[1, "age0"]
})            # no, initial age not needed, but leave for convenience

str(muscatine)
summary(muscatine)
nrow(unique(subset(muscatine, select=c(id))))
nrow(unique(subset(muscatine, select=c(id, gender))))

promptData(muscatine, "../man/muscatine.Rd")
save(muscatine, file="../data/muscatine.rda")


###_  : ntp

names(ntp) <- c("id", "dose", "weight", "malformed")
## Not sure whether dose should be a factor
ntp <- within(ntp, {
    id <- factor(id)
    malformed <- factor(malformed, labels=c("no", "yes"))
})

str(ntp)
summary(ntp)

promptData(ntp, "../man/ntp.Rd")
save(ntp, file="../data/ntp.rda")


###_  : obesity -- Subset of muscatine, so not worth including in ALA

## First 3 number variables represent years, and the rest cohorts
names(obesity) <- c("gender", "1977", "1979", "1981",
                    "1", "2", "3", "4", "5")


###_  : rat

## Number variables represent weeks and contain the weight
names(rat) <- c("id", "treatment", "0", "1", "2", "3", "4")
rat <- within(rat, {
    id <- factor(id)
    treatment <- factor(treatment,
                        labels=c("control", "thiouracil", "thyroxin"))
})
## Reshape
rat.m <- melt(rat, measure.vars=3:7, variable_name="week")
rat <- rat.m
rat <- within(rat, {week <- as.numeric(as.character(week))})
rat <- with(rat, rat[order(id, treatment, week), ])
names(rat)[4] <- "weight"
rat

str(rat)
summary(rat)
nrow(unique(subset(rat, select=c(id))))
nrow(unique(subset(rat, select=c(id, treatment))))

promptData(rat, "../man/rat.Rd")
save(rat, file="../data/rat.rda")


###_  : respir

## Number variables represent visit containing respiratory status
names(respir) <- c("center", "id", "treatment", "gender", "age", "0",
                   "1", "2", "3", "4")
respir <- within(respir, {
    center <- factor(paste("C", center, sep=""))
    id <- factor(id)
    treatment <- factor(treatment, labels=c("active", "placebo"))
    gender <- factor(gender)
})
## Reshape
respir.m <- melt(respir, measure.vars=6:10, variable_name="visit")
respir <- respir.m
respir <- within(respir, {visit <- as.numeric(as.character(visit))})
respir <- with(respir, respir[order(id, treatment, center, gender), ])
names(respir)[7] <- "status"
respir <- with(respir,                  # reorder columns
               data.frame(id, gender, center, treatment, age, visit, status))
respir <- within(respir, {
    status <- factor(status, labels=c("poor", "good"))
})
respir

## The description says there are 111 patients, yet the data show 56
## IDs... something is messed up here
str(respir)
summary(respir)
nrow(unique(subset(respir, select=c(id))))
nrow(unique(subset(respir, select=c(id, gender))))
nrow(unique(subset(respir, select=c(id, gender, treatment))))

promptData(respir, "../man/respir.Rd")
save(respir, file="../data/respir.rda")


###_  : skin

names(skin) <- c("id", "center", "age", "skin", "gender", "nExposure",
                 "nCancers", "treatment", "year")
skin <- within(skin, {
    id <- factor(id)
    center <- factor(paste("C", center, sep=""))
    skin <- factor(skin, labels=c("otherwise", "burns"))
    gender <- factor(gender, labels=c("F", "M"))
    treatment <- factor(treatment, labels=c("placebo", "B-carotene"))
})

str(skin)
summary(skin)
nrow(unique(subset(skin, select=c(id))))
nrow(unique(subset(skin, select=c(id, gender))))
nrow(unique(subset(skin, select=c(id, gender, treatment))))
nrow(unique(subset(skin, select=c(id, gender, treatment, center))))

promptData(skin, "../man/skin.Rd")
save(skin, file="../data/skin.rda")


###_  : smoking

names(smoking) <- c("id", "smoker", "year", "FEV1")
smoking <- within(smoking, {
    id <- factor(id)
    smoker <- factor(smoker, labels=c("former", "current"))
})

str(smoking)
summary(smoking)
nrow(unique(subset(smoking, select=c(id))))
nrow(unique(subset(smoking, select=c(id, smoker))))

promptData(smoking, "../man/smoking.Rd")
save(smoking, file="../data/smoking.rda")


###_  : tlc

## Number variables represent week containing lead level
names(tlc) <- c("id", "treatment", "0", "1", "4", "6")
tlc <- within(tlc, {
    id <- factor(id)
    treatment <- factor(treatment, labels=c("succimer", "placebo"))
})
## Reshape
tlc.m <- melt(tlc, measure.vars=3:6, variable_name="week")
tlc <- tlc.m
tlc <- within(tlc, {week <- as.numeric(as.character(week))})
tlc <- with(tlc, tlc[order(id, treatment), ])
names(tlc)[4] <- "lead"
tlc

str(tlc)
summary(tlc)
nrow(unique(subset(tlc, select=c(id))))
nrow(unique(subset(tlc, select=c(id, treatment))))

promptData(tlc, "../man/tlc.Rd")
save(tlc, file="../data/tlc.rda")


###_  : toenail

names(toenail) <- c("id", "onycholysis", "treatment", "month", "week")
## We need to have a look
summary(toenail)
toenail <- within(toenail, {
    id <- factor(id)
    treatment <- factor(treatment, labels=LETTERS[1:2])
    week <- c(0, 4, 8, 12, 24, 36, 48)[week]
})

str(toenail)
summary(toenail)
nrow(unique(subset(toenail, select=c(id))))
nrow(unique(subset(toenail, select=c(id, treatment))))

promptData(toenail, "../man/toenail.Rd")
save(toenail, file="../data/toenail.rda")


###_  : tumor

names(tumor) <- c("id", "treatment", "recurrence")
tumor <- within(tumor, {
    id <- factor(id)
    treatment <- factor(treatment, labels=c("placebo", "thiotepa"))
    recurrence <- factor(recurrence, labels=c("no", "yes"))
})

str(tumor)
summary(tumor)
nrow(unique(subset(tumor, select=c(id))))
nrow(unique(subset(tumor, select=c(id, treatment))))

promptData(tumor, "../man/tumor.Rd")
save(tumor, file="../data/tumor.rda")


###_  : tvsfp

## The pre and post variables indicate score period, containing the THKS score
names(tvsfp) <- c("school", "class", "school.based", "tv.based",
                  "pre", "post")
## We need to have a look
summary(tvsfp)
## Need to add a student ID column
tvsfp <- within(tvsfp, {
    id <- factor(seq(nrow(tvsfp)))
    school <- factor(school)
    class <- factor(class)
    school.based <- factor(school.based, labels=c("no", "yes"))
    tv.based <- factor(tv.based, labels=c("no", "yes"))
})
tvsfp <- with(tvsfp,
              data.frame(id, school, class, school.based, tv.based, pre, post))
## Reshape
tvsfp.m <- melt(tvsfp, measure.vars=6:7, variable_name="stage")
tvsfp <- tvsfp.m
tvsfp <- with(tvsfp, tvsfp[order(id, school, class, stage), ])
names(tvsfp)[7] <- "THKS"
tvsfp

## Have I understood the data structure correctly?
str(tvsfp)
summary(tvsfp)
nrow(unique(subset(tvsfp, select=c(id))))
nrow(unique(subset(tvsfp, select=c(id, school))))
nrow(unique(subset(tvsfp, select=c(id, school, class))))

promptData(tvsfp, "../man/tvsfp.Rd")
save(tvsfp, file="../data/tvsfp.rda")


###_ + Prepare examples

library(ALA)

###_  . Chapter 8

fev1.e <- exp(fev1$logFEV1)
set.seed(1234); ids <- sample(levels(fev1$id), 50)

if (require(lattice)) {
    bwplot(~ logFEV1, data=fev1)        # reveals the outlier mentioned in text
    subset(fev1, logFEV1 < -0.5)
    ## Fig. 8.4 (roughly)
    xyplot(log(fev1.e/height) ~ age, data=fev1, groups=id, type="b",
           subset=id %in% ids, cex=0.5, col=1,
           xlab="Age (years)", ylab="Log(FEV1/Height)",
           ylim=c(-0.3, 1.2))
}

if (require(lme4)) {
    ## Model in p. 213
    (fm1 <- lmer(logFEV1 ~ age + log(height) + age0 + log(height0) + (age | id),
                 data=fev1, subset=logFEV1 > -0.5))
    ## Table 8.3
    VarCorr(fm1)$id * 100

    ## Model in p. 216
    (fm2 <- update(fm1, . ~ . - (age | id) + (log(height) | id)))
}


###_ + Emacs local variables
## Local variables:
## allout-layout: (0 : + 0)
## End:
## 
### ALA.R ends here
