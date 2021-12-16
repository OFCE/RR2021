library(scales)
library(OECD)
library(glue)
library(ggrepel)
library(lubridate)
library(tidyverse)

pays <- c("ITA", "GRC", "SVK", "ESP", "POL", "KOR", "PRT", "HUN", "IRL", "AUT", "CAN", "BEL", "NZL", "GBR", "FRA", "DEU", "FIN", "USA", "NLD", "DNK", "SWE", "NOR")

datasets <- OECD::get_datasets() |> filter(str_detect(id, "SNA_TABLE11"))

query_cofog <- ".TLYCG+D1CG+B1_GE.T+010+020+030+040+050+060+070+080+090+100.GS13+S1.C"
query_d1 <- ".P1A+P2A+B1GA+D1A+D11A+B2G_B3GA+K1A+B2N_B3NA+D29_D39A+B1_GE.VTOT+VO+VP+VQ.C"

d1_oecd <- OECD::get_dataset(dataset="SNA_TABLE6A", query_d1) |> 
  transmute(value=as.numeric(ObsValue),
            measure = factor(MEASURE),
            year=as.numeric(Time),
            country = factor(LOCATION),
            branche = factor(ACTIVITY),
            transac = factor(TRANSACT), 
            unit = factor(UNIT)) |> 
  filter(unit!="IDX") |> 
  group_by(country, year) |> 
  summarize(
    rd1 = sum(value[branche%in%c("VO", "VP", "VQ")&transac=="D1A"]),
    d1tot = value[branche=="VTOT"&transac=="D1A"],
    pib = value[branche=="VTOT"&transac=="B1_GE"],
    educ = value[branche=="VP"&transac=="D1A"]/d1tot,
    admin = value[branche=="VO"&transac=="D1A"]/d1tot,
    sante = value[branche=="VQ"&transac=="D1A"]/d1tot,
    d1_nm = rd1/pib, 
    unit = unique(unit)) |> 
  filter(!near(rd1,0))

cofog_oecd <- OECD::get_dataset(dataset="SNA_TABLE11", query_cofog) |> 
  transmute(value=as.numeric(ObsValue),
         year=as.numeric(Time),
         country = factor(LOCATION),
         cofog = factor(ACTIVITY),
         transac = factor(TRANSACT), 
         unit = factor(UNIT)) |> 
  group_by(cofog, year, country) |>
  summarize(d1s13_td =  value[transac=="D1CG"]/value[transac=="TLYCG"],
            d1s13_pib = value[transac=="D1CG"]/value[transac=="B1_GE"],
            d1s13 = value[transac=="D1CG"]) |> 
  ungroup() |>
  arrange(country, cofog, -year) |> 
  mutate(country = fct_reorder(country, d1s13_pib, .fun = first)) 

data <- cofog_oecd |> filter(cofog=="T") |> 
  left_join(d1_oecd, by=c("year", "country")) |> 
  drop_na(d1_nm) |> 
  group_by(country) |> 
  summarize(year_max = max(year),
            d1_pib = d1s13_pib[year==year_max],
            d1_nm = d1_nm[year==year_max],
            d1_d1tot = d1s13[year==year_max]/d1tot[year==year_max], 
            d1_educ = educ[year==year_max],
            d1_sante = sante[year==year_max],
            d1_admin = admin[year==year_max],
            d1_vnm = d1_educ+d1_admin+d1_sante,
            delta_d1tot = d1s13[year==year_max]/d1tot[year==year_max]-d1s13[year==year_max-5]/d1tot[year==year_max-5]) |> 
  ungroup() |>
  mutate(
    country_a = str_c(country,case_when(
      year_max==2020 ~ "",
      year_max==2019 ~ "*",
      year_max <=2018 ~ "**")),
    country2 = fct_reorder(str_c(country, ' (', year_max, ')'), d1_nm),
    country_a = fct_reorder(country_a, d1_vnm)) |> 
  filter(country%in%pays) 
  
  
ggcofog <- ggplot(data |> select(country_a, d1_educ, d1_sante, d1_admin) |> pivot_longer(cols=-country_a), aes(x=country_a)) +
  geom_col(aes(y=value, fill = name), width = 0.5, alpha=0.75)+
  scale_fill_discrete(type = RColorBrewer::brewer.pal(3, "Blues"), labels=c("Administration", "Education", "Santé")) +
  theme_minimal(base_family="Nunito", base_size = unit(8, "pt"))+
  scale_y_continuous(labels = label_percent(1))+
  xlab("")+ylab("")+labs(fill="Part de la masse \ndu secteur dans la masse \nsalariale totale", size=3)+
  geom_segment(data = data,
               mapping=aes(x=country_a, xend=country_a, y=d1_d1tot-delta_d1tot, yend=d1_d1tot), 
               col="orange", arrow=arrow(type="closed", length=unit(2.5,"pt")))+
  geom_point(data = data, mapping=aes(x=country_a, y=d1_d1tot), col="darkorange")+
  geom_point(data = data, mapping=aes(x=country_a, y=d1_d1tot-delta_d1tot), col="orange" |> colorspace::lighten())+
  annotate("text", x="SVK*", y=0.32, color= "orange", hjust = 0, size=2,
           label="Part de la masse salariale des employés publics \ndans la masse salariale totale\n(dernier point connu et 5 ans avant)")+
  annotate("curve", curvature=0, x="SVK*", xend="HUN*", yend=0.25, y=0.32, arrow = arrow(length = unit(2, "mm")), color= "orange")+
  theme(legend.position = "bottom")+
  labs(caption="données pour 2020, *2019, **2018\n Sources : OCDE Comptes Nationaux table 11 et table 6a")

ggsave("fonctionnaires en masse salariale.svg", plot=ggcofog, width = 16, height=14, unit="cm")
           
data.table::fwrite(data |> select(country_a, d1_educ, d1_sante, d1_admin, d1_d1tot, delta_d1tot), "secteur public data.csv")
