# Generating a csv of random infection cases in a period of two months
# For uploading to DHIS2 Event Tracker according to specified format
# https://docs.dhis2.org/master/en/developer/html/dhis2_developer_manual_full.html#webapi_events_csv_import_export


# Dependencies
library(truncnorm)
library(dplyr)
library(reshape2)
library(XML)


n <- 200

# constant dimensions
const <- list(status            = 'ACTIVE',
              program           = 'iMQ9InaUU5m', #Kenema 2014 Outbreak
              enrollment        = '',
              programStage      = 'Wn7LAv17lfS',
              orgUnit           = 'kJq2mPyFEHo', #kenema
              AmO6YwrO46f       = "jUb8gELQApl", #district = Kailahun
              dueDate           = '2017-07-12T19:11:19.801',
              latitude          = 0,
              longitude         = 0,
              storedBy          = 'admin',
              providedElsewhere = '')


#TODO -> replace static UIDs with dynamic call to /api/system/id.json?limit=n
uid <- c("tHF7qDZVeoT", "e4b8nZEVFIS", "r214KeAilsh", "PUPI2ZGMKCU", "JcmJa3BPJPB", "WO08wxnFxlo", "jBJ4dVT4Vpn", "KpV4jXnOi3X", "wYpVB2qvtif", "qyEGtKuURxp", "v8zo0HACxdc", "HObXfaI9bKP",
         "a4Tjvw2D2IO", "wNN6FioIyLH", "PA2dv8AuQjY", "JLaBV19XZWH", "sTooUkg05gm", "M8pidK3JvLI", "l79SXeCwvOk", "yBEM2gX2d6H", "YkWQwYfZqPO", "whsXsdFTdya", "RO8oANHxSQA", "jqD6guWMVfo",
         "bEj4uYXrG47", "QzFYf8s6zNm", "ex6Rb2TWv9O", "XzKfqF0IcAg", "AVJGQwXDw6a", "aW37Qdu3Dcs", "o0RetQF035r", "sTD4iAjuqJg", "KSSciLFlDcL", "TgMpvv9emZN", "dvYlBJqyqpo", "gmGalusQaUK",
         "bBFauEOEl2n", "zSNTCN2R0eV", "QmpRDqpm6Ov", "FaI8mCzx3Re", "fw0JyU1acej", "Ig88doreF9i", "W0TQGR3bB9S", "oXtUYcoq4ZX", "cU7Z1pw1tsL", "HRafkzdnTP2", "SkryQFhlCYk", "SPN0cId0ntZ",
         "VU8C0b9HXeC", "PMKYIPQJFU6", "fVAek9wGXy6", "ZSGSw2Ho8uq", "E6nCasUgYe6", "YDZntnX9SK0", "eoT0kfAxjGQ", "R7sRihn6FU7", "smzNYC57J8X", "s6VAX2JWQR3", "XfxprZ5fG49", "lOtstMFTs16",
         "OSy9yXMenU1", "tt0YkEZHAep", "GxWR2SkIyNc", "X2Vw0dmOYXD", "j14AeWKkZYx", "OqJlIAyj640", "Vc3GIDOFycj", "QB0QRxyObxT", "ucD5UsmHSHu", "TkkwIsv1fmb", "tX03umHsNnG", "F7xqGXrFjZe",
         "FX1BiOKu5w4", "RSa0eMV8AQc", "zMSNbbztgM9", "Ue8jabUvOSK", "ie9kvjvFzuN", "FAQ2kXKuOzx", "f5c8ooiLl6H", "jM2HAVdLvGs", "jsV3ruvcSpr", "Nhy4CJkj0WE", "KLivieskyYG", "ZVK49FaApKc",
         "B0pOBq7XXhW", "UYYT1uOTxr9", "gaJc0A8GsaG", "YK4CkcEQvlK", "oSCIbQnW9A1", "VuJTkKL5BOR", "BebOk8Vg2Oi", "XgnQGl9uPVv", "GtKoyyzN03g", "gy1CnRm3fNl", "h6rCZ6hkUSR", "GPeuZsIzxAk",
         "UI23HXLcEG9", "cRGvGeEvet5", "tZArO0TrNKm", "crtr9iEP4CG", "ehvpcfGI5Ye", "jP2gjFzAIOA", "joi8IDTF49f", "Blo65YK3wdS", "Ne99YFdKTTs", "QAlGIIcU0xx", "yDFjGGotFLs", "OTY2FB4BFeB",
         "doBnC39WTLj", "vlEen05Lc1H", "r6SvYYOajCo", "VZ9d26E5z4V", "JWbr8HHkdOu", "Zkrz3tBtOey", "qRnud97OSw1", "WCfmMIKhcso", "N7N4OzNfZfy", "PJ6SB7med9m", "t7irV8PsQFC", "iiJNoK82b5e",
         "H7RSzseBELg", "iMxtJ2sQ4ye", "zFSUrsLeSDj", "JCWg6PC9DBD", "jeZVl7cvjOA", "gnnlGOz2fgT", "oMD38fC02YE", "N96XBdtfGmw", "izQdTV9KXuk", "OeVEixMNdji", "m6mOSp6HHGZ", "PESGlCWtYOV",
         "wLsIVHwBKJy", "N7rUzttkdb0", "Trk9N3OG7Bp", "WbKcZKRg9Wt", "DW69VT8ZNst", "IqybskXtbaV", "gEIr49eF5no", "fH2P38OCp88", "nh1P32Bg2ll", "HNHVzyu8u1X", "YXs8F1TE5o9", "cWdDokNlhD5",
         "Nlh8RcsPKCF", "jCLIrQwptr6", "BYS04pjQhPw", "CK4IW4qKHEB", "yLry0B6JOWr", "mnt0y891d5O", "iW7xvTrmzFm", "iBMRuuB1BIR", "OQ9ppbm2Crb", "nE7B4iW7q1H", "tDT7MMMImKu", "Wc8KrGtietl",
         "sHRIcHZy15k", "bLtlwGf4Y3I", "OfV4vDQZiMn", "tS6SFiOW9R0", "QH9tDcalQee", "SRBNgplb1um", "FPmHSWFesZ6", "ZMma2r4828Q", "fRfE3g8cwPW", "IbRtZdHYL4e", "hkcXAHa6Yl6", "DKqaBVDb93d",
         "XAb5RAw7OB9", "s1RLmGvPKBi", "L8SSz4JWRLt", "Gb1zc8bNj6i", "oD9EUSzYWUj", "KEh4btqYT3H", "x4ORtMCNBtF", "fFWe6MRtS67", "oAO3nxzMFq3", "TssMGFPGKQF", "HydV84q4HHf", "u8Q6HRY8RUY",
         "OD8f5JcDxks", "vQ6DoDziQCO", "SStA7SLRNUG", "Q4uiIHk2wo1", "IbNmcNLJ3Ml", "IuaRcYrbzJY", "r9EwrGmlpZ3", "kfBGWlfRXy5", "CmeKALHgudv", "g4QwPi0Ttga", "uYsCgTncRoc", "VAoGVfKvfd2",
         "Y8EbvQrEeCi", "WfKBmKzkx1n", "E5NX0YMakGy", "yBNg23LOP2C", "BPvX62h3Gif", "pec8WuT7R8L", "QK1cwoqBeLG", "TqRuyFWDIwn")


# EBOV PCR load values
pcr <- round(rtruncnorm(n=n, a=1, b=8.3, mean=3.5, sd=3.82), digits = 8)
pcr.t <- data.frame(event = uid, QloZSfFjMlw = pcr)

# GID numbers == pzsJKs59JsY
sequence <- seq(0000, 1000, by = 1)
lead.sequence <- formatC(sequence, width = 4, format = "d", flag = "0")
lead.sequence.c <- sprintf("T-%s", lead.sequence)
gid <- sample(lead.sequence.c, n, F)
gid.t <- data.frame(event = uid, pzsJKs59JsY = gid)


# assign GenBank IDs
seqID <- c('KR105295.1', 'KR105294.1', 'KR105266.1', 'KR105263.1', 'KR105318.1', 'KR105317.1', 'KR105316.1', 'KR105312.1',
           'KR105311.1', 'KR105310.1', 'KR105308.1', 'KR105306.1', ' KR105300.1', 'KR105293.1', 'KR105285.1', 'KR105284.1')
sequence <- sample(seqID, n, T)
sequence.t <- data.frame(event = uid, Wr0eNRscuNo = sequence)

# assign gender
gender <- sample(c("Female", "Male"), n, T)
gender.t <- data.frame(event = uid, oZg33kd9taw = gender)

# assign age
age <- floor(rtruncnorm(n=n, a=16, b=70, mean=33, sd=20))
age.t <- data.frame(event = uid, qrur9Dvnyt5 = age)

# assign outcome
outcome <- sample(c("Died", "Discharged"), n, T)
outcome.t <- data.frame(event = uid, wriXJkDtFE7 = outcome)

# chiefdom == NczRMzhFDdO
chief.options <- c("j0Mtr3xTMjM", "hjpHnHZIniP", "j0Mtr3xTMjM", "byp7w6Xd9Df", "JsxnA2IywRo")
chiefdom <- sample(chief.options, n, T)
chiefdom.t <- data.frame(event = uid, NczRMzhFDdO = chiefdom)


# Generating the two days datasets
set.seed(43)
days1 <- round(rnorm(100, mean = 25, sd = 2))
days2 <- round(rnorm(100, mean = 0, sd = 4))

# Turning negative days into days of previous month
toPrevMonth <- function(vect) {
  toMay <- c()
  toJune <- c()
  for (i in 1:length(vect)) {
    if (vect[i] <= 0) {
      toMay <- append(toMay, vect[i] + 31)
    } else {
      toJune <- append(toJune, vect[i])
    }
  }
  return(list("toMay" = toMay, "toJune" = toJune))
}

listOfVectors <- toPrevMonth(days2)


# Clean days variable asignment
D1a <- days1
D1b <- listOfVectors$toMay
D2 <- listOfVectors$toJune


# Create dates for May and June
dates1a <- sprintf("2014-05-%dT00:00:00.000", D1a)
dates1b <- sprintf("2014-05-%dT00:00:00.000", D1b)
dates2 <- sprintf("2014-06-%dT00:00:00.000", D2)

# create random locations
#for town 1
lat1 <- seq(8.81, 9.34, by = 0.03)
lng1 <- seq(-12.48, -11.8, by = 0.03)

# for town 2
lat2 <- seq(7.6, 8.2, by = 0.03)
lng2 <- seq(-11.5, -10.6, by = 0.03)


# sampling to create coordinates
c1 <- list('lat' = sample(lat1, length(dates1a), T),
           'lng' = sample(lng1, length(dates1a), T))


c2a <- list('lat' = sample(lat2, length(dates1b), T),
            'lng' = sample(lng2, length(dates1b), T))

c2b <- list('lat' = sample(lat2, length(dates2), T),
            'lng' = sample(lng2, length(dates2), T))


# dataframes
town1 <- data.frame(eventDate = dates1a, lat = c1$lat, lng = c1$lng)
town2a <- data.frame(eventDate = dates1b, lat = c2a$lat, lng = c2a$lng)
town2b <- data.frame(eventDate = dates2, lat = c2b$lat, lng = c2b$lng)
allcases <- rbind(town1, town2a, town2b)
allcases <- cbind(event = uid, allcases, sequence, gender, age, outcome, chiefdom, gid)

allcases$location <- paste("[", allcases$lng, ",", allcases$lat, "]", sep = "")
allcases$load <- pcr

allcases$eventDate <- as.character(allcases$eventDate)
allcases$gender <- as.character(allcases$gender)
allcases$age <- as.character(allcases$age)
allcases$outcome <- as.character(allcases$outcome)
allcases$sequence <- as.character(allcases$sequence)
allcases$chiefdom <- as.character(allcases$chiefdom)
allcases$gid <- as.character(allcases$gid)

sub <- allcases[c(1:5),]

location.t <- data.frame(event = uid, F3ogKBuviRA = paste(allcases$lng, ',', allcases$lat, sep = ''))

# meltCol <- function(vector){
#   return(melt(vector, id.vars = 'event'))
# }
# 
# pcr.m <- meltCol(pcr.t)
# gid.m <- meltCol(gid.t)
# sequence.m <- meltCol(sequence.t)
# gender.m <- meltCol(gender.t)
# age.m <- meltCol(age.t)
# outcome.m <- meltCol(outcome.t)
# chiefdom.m <- meltCol(chiefdom.t)
# location.m <- meltCol(location.t)
# district.m <- meltCol(data.frame(event = uid, AmO6YwrO46f = const$AmO6YwrO46f))
# 
# dates <- data.frame(event = uid, eventDate = allcases$eventDate)
# 
# 
# rowVars <- rbind(location.m, gender.m, age.m, outcome.m, pcr.m, sequence.m, chiefdom.m, district.m, gid.m)
# rowVars.o <- arrange(rowVars, event)
# 
# varsTable <- dates %>% left_join(rowVars.o, by = "event")
# 
# finalTable <- data.frame(event = varsTable$event,
#                          status = const$status,
#                          program = const$program,
#                          programStage = const$programStage,
#                          enrollment = const$enrollment,
#                          orgUnit = const$orgUnit,
#                          eventDate = as.character(varsTable$eventDate),
#                          dueDate = const$dueDate,
#                          latitude = const$latitude,
#                          longitude = const$longitude,
#                          dataElement = varsTable$variable,
#                          value = varsTable$value,
#                          storedBy = const$storedBy,
#                          providedElsewhere = const$providedElsewhere
# )

# write.csv(finalTable, file = "programKenemaDhis.csv", row.names = F, quote = F, fileEncoding = 'UTF-8')

# write.csv(finalTable[1:5,], file = "programKenemaTest.csv", row.names = F, quote = F, fileEncoding = 'UTF-8')


xml <- xmlTree()
  xml$addTag("events", close=F)
    for (i in 1:nrow(allcases)) {
      xml$addTag("event", close=F, attrs = c(program = const$program, orgUnit = const$orgUnit, eventDate = allcases$eventDate[i], status = const$status, storedBy = const$storedBy))
      xml$addTag("coordinate", close=T, attrs = c(latitude = const$latitude, longitude = const$longitude))
      xml$addTag("dataValues", close=F)
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "F3ogKBuviRA", value = allcases$location[i])) #location
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "oZg33kd9taw", value = allcases$gender[i])) #gender
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "qrur9Dvnyt5", value = allcases$age[i])) #age
      # xml$addTag("dataValue", close=T, attrs = c(dataElement = "wriXJkDtFE7", value = allcases$outcome[i])) #outcome
      # xml$addTag("dataValue", close=T, attrs = c(dataElement = "QloZSfFjMlw", value = allcases$load[i])) #pcr load
      # xml$addTag("dataValue", close=T, attrs = c(dataElement = "Wr0eNRscuNo", value = allcases$sequence[i])) #genebank
      # xml$addTag("dataValue", close=T, attrs = c(dataElement = "NczRMzhFDdO", value = allcases$chiefdom[i])) #chiefdom
      # xml$addTag("dataValue", close=T, attrs = c(dataElement = "AmO6YwrO46f", value = const$AmO6YwrO46f))    #district
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "pzsJKs59JsY", value = allcases$gid[i]))    #gid
      xml$closeTag()
      xml$closeTag()
    }

cat(saveXML(xml))


saveXML(xml, file = "Kenema2014FinalDummy.xml", indent=TRUE)
