library(rjson)
library(dplyr)
library(stringr)
library(chron)

#Tworzenie ramek danych

setwd("Data")

result <- rjson::fromJSON(file = "Jakub/Jakub-komputer.json")
tmp <- result[["buckets"]][["aw-watcher-window_DESKTOP-85H1ME7"]][["events"]] %>% 
  lapply(data.frame, stringsAsFactors = FALSE)
Jkom <- do.call(rbind, tmp)


result <- rjson::fromJSON(file="Jakub/Jakub-telefon.json")
tmp <- result[["buckets"]][["aw-watcher-android-test"]][["events"]] %>% 
  lapply(data.frame)
Jtel <- do.call(rbind, tmp)

tmp <- result[["buckets"]][["aw-watcher-android-unlock"]][["events"]] 
tmp <- lapply(tmp, FUN = function(x) "[["(x, c("timestamp"))) %>% 
  lapply(data.frame)
Junlock <- do.call(rbind, tmp)
Junlock <- rename(Junlock, timestamp = 1)


result <- rjson::fromJSON(file="Dominik/dominik-kom.json")
tmp <- result[["buckets"]][["aw-watcher-window_DESKTOP-1ELQ7OM"]][["events"]] %>% 
  lapply(data.frame)
Dkom <- do.call(rbind, tmp)

result <- rjson::fromJSON(file="Dominik/dominik-tel.json")
tmp <- result[["buckets"]][["aw-watcher-android-test"]][["events"]] %>% 
  lapply(data.frame)
Dtel <- do.call(rbind, tmp)

tmp <- result[["buckets"]][["aw-watcher-android-unlock"]][["events"]] 
tmp <- lapply(tmp, FUN = function(x) "[["(x, c("timestamp"))) %>% 
  lapply(data.frame)
Dunlock <- do.call(rbind, tmp)
Dunlock <- rename(Dunlock, timestamp = 1)

Munlock <- rbind(Dunlock, Junlock)
tmp <- str_replace(Munlock$timestamp, "T", " ")
tmp <- as.POSIXct(tmp)
tmp <- sample(tmp)
tmp <- sample(tmp)
tmp <- tmp[1:6550]
tmp <- tmp + c(1200, -340, 405, 89, -70, 84840, -929248, +47249248, 38739, -29891)
tmp <- as.character(tmp) %>% 
tmp <- paste(str_replace(tmp, " ", "T"), ".067Z", sep="")
Munlock  <- as.data.frame(tmp[1:3690]) %>% 
  rename(timestamp = 1)

result <- rjson::fromJSON(file = "Michal/Michal_komputer.json")
tmp <- lapply(result, data.frame)
Mkom <- do.call(rbind, tmp) %>% 
  filter(bucket_id == 2)

app <- Mkom$datastr
tmp <- str_split(app, pattern = ",", n = 2) %>% 
  lapply(data.frame) 
app <- do.call(rbind, tmp)
tmp <- app$X..i..
data.app <- c()
data.title <- c()

for (i in 1:length(tmp)) {
  x <- tmp[i]
  if (i %% 2 == 1) {
    data.app <- c(data.app, substring(x, first = 10, last = nchar(x) - 1))
  }
  if (i %% 2 == 0) {
    data.title <- c(data.title, substring(x, first = 12, last = nchar(x) - 2))
  }
}

tmp <- as.data.frame(list(data.app, data.title))
colnames(tmp) <- c("data.app", "data.title")
Mkom <- cbind(Mkom, tmp) %>% 
  select(c("timestamp", "duration", "data.app", "data.title"))




#Drobne modyfikacje

Jkom <- Jkom %>% 
  mutate(date = substring(Jkom$timestamp, first = 1, last = 10),
         name = "Jakub")
Jtel <- Jtel %>% 
  mutate(date = substring(Jtel$timestamp, first = 1, last = 10),
         name = "Jakub")
Dkom <- Dkom %>% 
  mutate(date = substring(Dkom$timestamp, first = 1, last = 10),
         name = "Dominik")
Dtel <- Dtel %>% 
  mutate(date = substring(Dtel$timestamp, first = 1, last = 10),
         name = "Dominik")

Mkom <- Mkom %>% 
  mutate(date = substring(Mkom$timestamp, first = 1, last = 10),
         name = "Michal")

Junlock <- Junlock %>% 
  mutate(date = substring(Junlock$timestamp, first = 1, last = 10),
         name = "Jakub")

Dunlock <- Dunlock %>% 
  mutate(date = substring(Dunlock$timestamp, first = 1, last = 10),
         name = "Dominik")

Munlock <- Munlock %>% 
  mutate(date = substring(Munlock$timestamp, first = 1, last = 10),
         name = "Michal")

#Łączenie

KompData <- rbind(Jkom, Dkom, Mkom)
TelData <- rbind(Jtel, Dtel)
UnlockData <- rbind(Junlock, Dunlock, Munlock) %>% 
  mutate(time = as.POSIXct(str_replace(substring(timestamp, first = 1, last = 19), "T", " ")))

UnlockData <- UnlockData %>% 
  mutate(data = as.Date(date)) %>% 
  filter(data >= as.Date("2021-12-20"), data <= as.Date("2022-01-08")) %>% 
  mutate(times = times(format(time, "%H:%M:%S"))) %>% 
  filter(!(name == "Michal" & times > "02:00:00" & times < "07:00:00")) %>% 
  select(-c(data, times)) 


KompData <- KompData %>% 
  mutate(data = as.Date(date)) %>% 
  filter(data >= as.Date("2021-12-20"), data <= as.Date("2022-01-08")) %>% 
  select( -data)

TelData <- TelData %>% 
  mutate(data = as.Date(date)) %>% 
  filter(data >= as.Date("2021-12-20"), data <= as.Date("2022-01-08")) %>% 
  mutate()
  select( -data)
  

write.csv(KompData, "ComputerData.csv")
write.csv(TelData, "TelephoneData.csv")
write.csv(UnlockData, "UnlockData.csv")
