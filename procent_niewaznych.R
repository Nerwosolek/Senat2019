#setwd("Your data folder path here")
library(dplyr)
library(readxl)
total <- tibble()
for (obwod in 1:100) {
  temp <- read_excel(paste("wyniki_gl_na_kand_po_obwodach_senat_okr_",obwod,".xlsx", sep = ""))
  temp <- select(temp, 14, 28:32)
  temp$obwod = obwod
  total <- rbind(total, temp)
}

total_an <- total %>% group_by(obwod) %>% 
  summarise(liczbaKartWydanych = sum(`Liczba wyborców, którym wydano karty do g³osowania`), 
            liczbaGlosowWaznych = sum(`Liczba g³osów wa¿nych oddanych ³¹cznie na wszystkich kandydatów`),
            liczbaX2 = sum(`W tym z powodu postawienia znaku „X” obok nazwiska dwóch lub wiêkszej liczby kandydatów`),
            liczbaX0 = sum(`W tym z powodu niepostawienia znaku „X” obok nazwiska ¿adnego kandydata`),
            liczbaXS = sum(`W tym z powodu postawienia znaku „X” wy³¹cznie obok skreœlonego nazwiska kandydata`))
total_an <- total_an %>% mutate(liczbaNiewaznychKartGlosow = liczbaKartWydanych - liczbaGlosowWaznych)
total_an <- total_an %>% mutate(procentNiewaznychKG = liczbaNiewaznychKartGlosow / liczbaKartWydanych * 100)
sredniProc = mean(total_an$procentNiewaznychKG)
odchylenieProc = sd(total_an$procentNiewaznychKG)
X2Proc = sum(total_an$liczbaX2) / sum(total_an$liczbaNiewaznychKartGlosow) * 100
X0Proc = sum(total_an$liczbaX0) / sum(total_an$liczbaNiewaznychKartGlosow) * 100
XSProc = sum(total_an$liczbaXS) / sum(total_an$liczbaNiewaznychKartGlosow) * 100
procNiewKG = sum(total_an$liczbaNiewaznychKartGlosow) / sum(total_an$liczbaKartWydanych) * 100