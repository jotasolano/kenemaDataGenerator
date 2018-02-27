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
              program           = 'vtHPxcCgu25', #Kenema 2014 Outbreak
              enrollment        = '',
              programStage      = 'Wn7LAv17lfS',
              orgUnit           = 'kJq2mPyFEHo', #kenema
              AmO6YwrO46f       = "MqD4UQm3eBd", #district = Kailahun
              dueDate           = '2017-07-12T19:11:19.801',
              latitude          = 0,
              longitude         = 0,
              storedBy          = 'admin',
              providedElsewhere = '')


#TODO -> replace static UIDs with dynamic call to /api/system/id.json?limit=n
uid <- c("ybtwbJiVFFS", "VraR2pvuOVx", "VdNlBYghxfa", "VPlrjr86UMD", "wRxv38X25TF", "Lb9lVT8g2il", "VObQ3awUSqV", "QioLx6Sgepw", "JRY6JASZwAJ", "NFfInbGcks0", "PmBOYLnzbiQ",
         "ydPdqRLa6yI", "SFXoX9Vq2zp", "Vq1oz4j9X3M", "WkMxhoyh9Yj", "bjxaX1pp7uR", "AaJaUzHda3r", "gPbXA5Ibj02", "v0V5D21Smz0", "KsGzqvkioHX", "b4vwgZbvgra", "nTJsy207yA3", 
         "SAkOrVHFeYI", "UHaicwoodAU", "qrVRoic8JcD", "lwvaCS94cMG", "TqXZq9HVrIo", "AZOLUTSXF3i", "gvSXUrWyZK8", "n6HvDhkARcg", "NvOtvicX6WV", "rLj6Avi63f8", "KRAvsNr7BFA",
         "ElgCY5GDe3M", "MgaWMci84pd", "qT6dWKjP31T", "DEBgZxABCQK", "EdVJUgLhvjT", "YeixtyVm9rC", "MJUzrPmcum2", "WgWy0iPfxof", "qSubZcydjG6", "Tu4AWpexnyr", "kvth5Zfcbat",
         "jXOP9mPn509", "OfZvHXbxJzN", "P6mlkbkflYD", "uUgLAIaY7IC", "CNFJOHKky7e", "XDnWoonAsat", "IGoi6OOMdyG", "m5I6CnDPDWY", "AmRUG6LkoyX", "szhwjzhwwOh", "c0fEMukuM8V",
         "VwmKD1aSZK0", "fy27DoVInZw", "Ct87kJLsoSJ", "FBEHHU6Rj4a", "s7iOoHo6xjt", "nnHBUxSjMYe", "GHuVNqlLtpb", "aprBHKt72ei", "uC3GJRqyyFz", "BTyuMyQHUfi", "lHdaiqfxWVg",
         "wQLlf2ElGdl", "IqzaSXtZ0DV", "Xe3JITUfkUT", "ticWlwZzkXv", "f0plZh134Uv", "hIafMTDFEqr", "KLBgqHegFF1", "ZXUXrx12LPZ", "TmYRSG6Eydr", "TsJ9f3v0Bbd", "ujLeS07pjDT",
         "CBIlinRrPJ5", "lxM5FIlyeMc", "G5MMWm53VjP", "ZXVgT5zfKzX", "mojFsckwWha", "CPiUEUnGHrv", "VroKQbsIUxo", "d895Xtjq6WY", "rUZyr4NWKM7", "V0541CdB4mv", "essNShPI0Zb",
         "LNK5e1Y6DDy", "R9v5ZyYE3dq", "NQebwKpltBZ", "HzvXELRqQnf", "xqfaZCG5HOu", "KqvgikvRLj9", "bO21vDDpYLp", "POgl9JsaZvu", "fpU76OxAgG2", "qao6xvUvzon", "wkFwZUqPW95",
         "RzEmVFh7mnw", "xi2Zt3SFssz", "z2JfrBrFmsO", "urGgKuGkLv4", "pMvDs7eHaJm", "OnsVG6yCZH4", "RCBNARvC8GP", "nC5qrpBTnhn", "SoFFp4Z6Lzh", "gSIWW2TNvuq", "WtznNcW9xMW",
         "YJGooGzUt7W", "ehwDWSbjbQw", "U8WbFxuPIV5", "w5arxUoT5jW", "nS37ooxlsac", "pHwUOYWYntM", "eMeEuDJm3QP", "g5XiwqpiKTi", "UEc3EdX3RLb", "pOtA5kAYCIm", "f8iGG82U0sj",
         "z1pi661XCPv", "deKLnLUQplv", "wYhFrosoI1o", "qSfqUyhEFlm", "eYCdQf4Gl7y", "AXmUmeQbqwy", "PR2d1HE4ObJ", "hbS1TOEZ7HE", "mIDB234I7fC", "aca44s1TPDW", "kpZcvucam3x",
         "YyTXerOTmLK", "O48dcve26k0", "Ke8dEcgzxMl", "sTrkqPLIjov", "CbSGLLRDigf", "PzNZq2n4A0v", "wH8J9djTAi4", "jkR9UD59l6O", "sh6lXF69k1h", "QK64JCXvcBK", "heje2G89kEh",
         "E3NHnf1rvVn", "fg1tgibRTX3", "YjOJicwSHAX", "jRw7W3P0Wog", "uzvHlZ8llpe", "heWmMIAmXJj", "aJkIJQJ1RjU", "ggTo1YOQrta", "SsXTUenne6y", "GSUkHNIEySK", "KkmQbphR16I",
         "n9S9oEhnzRX", "LTqfXNhBXqh", "S0vpgOQOR0M", "K8KHtTRRHP8", "bw1wiohJ0D0", "Vivfh8Sqot0", "c8PKkrqw0cU", "BrGDYrZcV7a", "TcGdzrZ6mLG", "EuWSEkFljZJ", "A6cH4qiqBQO",
         "VhraVyumCYh", "jLLAkZu0TWV", "r0Vk1lO2XJR", "cASGZQ0JTKJ", "SZEVuOBrbWj", "Z4e7FQfTmCz", "kdex1AQlfDh", "dyfdUmP9tIL", "EubV6oZn3VN", "NvUWcJroBY1", "TpNG63RIeIW",
         "txsV3Ru9Bu2", "DVx7357j31B", "bEqOGMcVrLq", "OL6mplP6OUt", "UeeuNUZv47M", "QzlptnZ0Ooo", "Nx6MvDDmyIn", "mjzLKv4cZro", "jaS4SMUSMkh", "AC1OUXwmVoI", "ZLs9aA9Qfra",
         "AaXkVoU0rwi", "iMLM6XmhrZD", "CzcNhic3nRm", "oem6NuCVwgv", "CX3tAFzeDLL", "dBz8pUiZmmE", "C52pldE2wcH", "RHjn8X1ut9A", "HJxJcDkGVhW", "FwV6e8XNguT", "lhWCIQBYO4z",
         "Gx7FfTCwtHu", "lwqpx9S6B7G")


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

# chiefdom == g8BwLBXu0ah
chief.options <- c("j0Mtr3xTMjM", "hjpHnHZIniP", "j0Mtr3xTMjM", "byp7w6Xd9Df", "JsxnA2IywRo")
chiefdom <- sample(chief.options, n, T)
chiefdom.t <- data.frame(event = uid, g8BwLBXu0ah = chiefdom)


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
    for (i in 1:2) {
      xml$addTag("event", close=F, attrs = c(program = const$program, orgUnit = const$orgUnit, eventDate = allcases$eventDate[i], status = const$status, storedBy = const$storedBy))
      xml$addTag("coordinate", close=T, attrs = c(latitude = const$latitude, longitude = const$longitude))
      xml$addTag("dataValues", close=F)
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "F3ogKBuviRA", value = allcases$location[i])) #location
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "oZg33kd9taw", value = allcases$gender[i])) #gender
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "qrur9Dvnyt5", value = allcases$age[i])) #age
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "sRPDqR7eyNy", value = allcases$outcome[i])) #outcome
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "waxuDSlxQnv", value = allcases$load[i])) #pcr load
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "oMOpzxWyIbM", value = allcases$sequence[i])) #genebank
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "g8BwLBXu0ah", value = allcases$chiefdom[i])) #chiefdom
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "MqD4UQm3eBd", value = const$AmO6YwrO46f))    #district
      xml$addTag("dataValue", close=T, attrs = c(dataElement = "KeOyiSETpGr", value = allcases$gid[i]))    #gid
      xml$closeTag()
      xml$closeTag()
    }

cat(saveXML(xml))


saveXML(xml, file = "Kenema2014FinalDummy_RecoverOnly2.xml", indent=TRUE)
