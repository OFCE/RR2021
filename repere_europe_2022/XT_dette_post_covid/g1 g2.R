library(eurostat)
# devtools::install_github("expersso/OECD")
library(OECD)
library(sf)
library(tmap)
library(ggpmisc)
library(jtools)
library(cartogram)
library(scales)
library(plotly)
library(raster)
library(stars)
library(tidyverse)
library(lubridate)
library(tsibble)
library(tempdisagg)
library(slider)
library(patchwork)
library(glue)
library(countrycode)
library(ggrepel)
library(ggh4x)

# OECD data -------------------

datasets <- OECD::get_datasets() |> filter(str_detect(id, "^EO")) |> mutate(eo = str_extract(id, "(?<=EO)[:digit:]+") |> as.numeric()) |> filter(eo == max(eo, na.rm=TRUE))
eo_ref <- str_c("EO", str_extract(datasets$id,"[:digit:]{3}"))
end_q <- yq("2021 Q4")
end_y <- 2021

# struct <- OECD::get_data_structure(datasets$id)
# struct$VAR_DESC
# reg_names <- struct$REG_ID 
# struct$LOCATION
# struct$OBS_VALUE
# struct$VARIABLE %>% view

eurozone <- tibble::tribble(
  ~State,     ~Adopted, ~Population, ~Nominal_GNI, ~Relative_GNI, ~GNI_pc,    ~preeuro,          ~Exceptions, ~iso,
  "Austria", "01/01/1999",    8858775L,      456779L,       "3.18%",  51460L, "Schilling",                   NA, "AT",
  "Belgium", "01/01/1999",   11467923L,      551595L,       "4.18%",  48030L,     "Franc",                   NA, "BE",
  "Cyprus", "01/01/2008",     875898L,       24628L,       "0.18%",  27710L,     "Pound", "Northern Cyprus[a]", "CY",
  "Estonia", "01/01/2011",    1324820L,       30856L,       "0.20%",  23260L,     "Kroon",                   NA, "EE",
  "Finland", "01/01/1999",    5517919L,      276085L,       "2.08%",  50010L,    "Markka",                   NA, "FI",
  "France", "01/01/1999",   67028048L,     2846910L,      "22.41%",  42960L,     "Franc",   "New Caledonia[b]", "FR",
  "Germany", "01/01/1999",   83019214L,     4038526L,      "31.79%",  42450L,      "Mark",                   NA, "DE",
  "Greece", "01/01/2001",   10722287L,      211647L,       "1.97%",  19750L,   "Drachma",                   NA, "EL",
  "Ireland", "01/01/1999",    4904226L,      316269L,       "1.69%",  64000L,     "Pound",                   NA, "IE",
  "Italy", "01/01/1999",   60359546L,     2081972L,      "16.91%",  34530L,      "Lira",                   NA, "IT",
  "Latvia", "01/01/2014",    1919968L,       33932L,       "0.24%",  17740L,      "Lats",                   NA, "LV",
  "Lithuania", "01/01/2015",    2794184L,       53162L,       "0.36%",  19080L,     "Litas",                   NA, "LT",
  "Luxembourg", "01/01/1999",     613894L,       45817L,       "0.33%",  73910L,     "Franc",                   NA, "LU",
  "Malta", "01/01/2008",     493559L,       14089L,       "0.07%",  28030L,      "Lira",                   NA, "MT",
  "Netherlands", "01/01/1999",   17282163L,      920333L,       "6.89%",  53100L,   "Guilder",           "Aruba[c]", "NL",
  "Portugal", "01/01/1999",   10276617L,      238204L,       "1.75%",  23200L,    "Escudo",                   NA, "PT",
  "Slovakia", "01/01/2009",    5450421L,      104778L,       "0.76%",  19210L,    "Koruna",                   NA, "SK",
  "Slovenia", "01/01/2007",    2080908L,       54169L,       "0.38%",  25940L,     "Tolar",                   NA, "SI",
  "Spain", "01/01/1999",   46934632L,     1430766L,      "10.75%",  30390L,    "Peseta",                   NA, "ES"
)
ez <- countrycode::countrycode(eurozone |> filter(Population>0.01*sum(Population)) |> arrange(-Population) |> pull(State), "country.name", "iso3c") 
ez_query <- str_c(ez, collapse="+")
query_A <- "EA17+GBR+USA+{ez_query}.GNFLQ+CBGDPR+ITISK+POP+NLGXQ+NLGQ+GDPVTR_ANNPCT+GDP+GDPV+GGFLMQ+GNINTQ.A" |> glue()
query_Q <- "EA17+GBR+USA+{ez_query}.UNR+GNFLQ+PCORE+PCOREH+CBGDPR+ITISK+GDP+GDPV+GDPVD+GGFLMQ.Q" |> glue()

oecd_a <- OECD::get_dataset(dataset=datasets$id, query_A) 
oecd_q <- OECD::get_dataset(dataset=datasets$id, query_Q) 

oecd1 <- oecd_a %>%
  mutate(year=as.numeric(Time),
         ObsValue = as.numeric(ObsValue)) %>%
  select(-UNIT,  -TIME_FORMAT, -FREQUENCY, -REFERENCEPERIOD,-POWERCODE, pays=LOCATION, -Time) %>%
  pivot_wider(names_from=c(VARIABLE), values_from=ObsValue) %>% 
  as_tsibble(key=pays, index=year) %>% 
  group_by_key() %>% 
  mutate(dette = if(all(is.na(GNFLQ))) GGFLMQ else GNFLQ,
         dette = dette-dette[which(year==2007)]) |> 
  mutate(pays = factor(pays, c(ez, "EA17","GBR","USA")))

pop.a <- map(oecd1 %>% as_tibble() %>% select(pays, POP, year) %>% pivot_wider(names_from = pays, values_from = POP) %>% select(-year),
             ~ts(.x, start=min(oecd1$year), end=max(oecd1$year)))
pop.q <- map(pop.a, ~predict(td(.x ~ 1, to="quarterly", method = "denton-cholette", conversion = "average")))
pop.q <- do.call(cbind,pop.q) %>% as_tsibble() %>% rename(POP=value)

oecd2 <- oecd_q %>%
  filter(FREQUENCY=="Q") %>% 
  select(-UNIT, -REFERENCEPERIOD, -TIME_FORMAT, -FREQUENCY) %>%
  rename(trim=Time, pays=LOCATION) %>% 
  mutate(date = lubridate::yq(trim),
         ObsValue =as.numeric(ObsValue)) %>% 
  pivot_wider(names_from=c(VARIABLE), values_from=ObsValue) %>% 
  mutate(PCORE=ifelse(is.na(PCORE), PCOREH, PCORE)) %>% 
  group_by(pays) %>% 
  arrange(pays,date) %>% 
  mutate(core_inflation = (PCORE/lag(PCORE, 4)-1)) %>% 
  select(-PCOREH) %>% 
  left_join(pop.q, by=c("pays"="key", "date"="index")) %>% 
  drop_na(date) %>% 
  as_tsibble(key=pays) %>%
  group_by_key() %>% 
  mutate( gdp_pc = GDPV/POP/(GDPV[which(trim=="2007-Q1")]/POP[which(trim=="2007-Q1")]),
          inv = ITISK/GDP/(ITISK[which(trim=="2007-Q1")]/GDP[which(trim=="2007-Q1")])) %>% 
  mutate(ca_ma = slide_dbl(CBGDPR, mean, .before=3),
         inv= slide_dbl(inv, mean, .before=3)) |> 
  mutate(pays = factor(pays, c(ez, "EA17","GBR","USA")))

so <-  "Source OECD {eo_ref}" |> glue()

(g1_repere <- ggplot(oecd1 %>% filter(between(year,2007,end_y)), aes(x=year, y=dette, group=pays))+
    geom_line(data=~filter(.x, !pays%in%c("EA17","GBR","USA")),lwd=0.25, show.legend = FALSE,  color = "grey80")+
    geom_line(data=~filter(.x, pays%in%c("EA17","GBR","USA")),size=0.5, mapping=aes(linetype=pays), col="black", show.legend=FALSE)+
    geom_text_repel(data= ~.x |>
                      mutate(label=if_else(pays%in%c("EA17","GBR","USA") & year==2015, as.character(pays), "")), 
                    aes(label=label), 
                    size=2, 
                    segment.size = 0.1, 
                    fontface = "italic",
                    min.segment.length=0)+
    guides(color="none")+
    labs(title="", caption=str_c(so, " Dette au sens de Maastricht pour EA et GBR"))+
    theme_void(base_family="Nunito", base_size=6)+
    scale_y_continuous(limits=c(0,100), guide="axis_minor")+
    scale_x_continuous(breaks = c(2007, 2010, 2015, 2020), guide="axis_minor", minor_breaks = seq(2007,2021, 1))+
    theme(
      axis.line = element_line(color="black", size=0.2, linetype = 1, lineend = "butt"),
      panel.border = element_rect(color="black", fill=NA, size=0.2, linetype = 1),
      axis.ticks = element_line(color="black", size=0.1),
      axis.ticks.length = -unit(2.5,"pt"),
      axis.text = element_text(size=unit(6,"pt"), margin=margin(t=5, r=5, l=5, b=5), hjust=0),
      plot.caption=element_text(size = unit(6,"pt"), face = "italic")))

ggsave("g1_repere.svg" |> glue(), plot=g1_repere, width=8, height=4.4, units="cm")

(g2_repere <- ggplot(oecd1 %>% filter(between(year,2007,end_y)), aes(x=year, y=-NLGQ, group=pays))+
    geom_line(data=~filter(.x, !pays%in%c("EA17","GBR","USA")), lwd=0.25, show.legend = FALSE,  color = "gray80")+
    geom_line(data=~filter(.x, pays%in%c("EA17","GBR","USA")), aes(linetype=pays), color = "black", lwd = 0.5, show.legend=FALSE) +
    geom_hline(yintercept= 3, linetype = "dashed", size=0.2)+
    geom_hline(yintercept= 0, color="grey", size=0.2)+
    scale_y_continuous(breaks = c(-5, 0, 3, 5, 10, 15), limits = c(-5,15), oob = oob_keep, guide="axis_minor")+
    geom_text_repel(data= ~.x |>
                      mutate(label=if_else(pays%in%c("EA17","GBR","USA") & year==2015, as.character(pays), "")), 
                    aes(label=label), 
                    size=2, 
                    segment.size = 0.1, 
                    fontface = "italic",
                    min.segment.length=0)+
    guides(color="none")+
    labs(title="", caption=so)+
    theme_void(base_family="Nunito", base_size=6)+
    scale_x_continuous(breaks = c(2007, 2010, 2015, 2020), guide="axis_minor", minor_breaks = seq(2007,2021, 1))+
    theme(
      axis.line = element_line(color="black", size=0.2, linetype = 1, lineend = "butt"),
      panel.border = element_rect(color="black", fill=NA, size=0.2, linetype = 1),
      axis.ticks = element_line(color="black", size=0.1),
      axis.ticks.length = -unit(2.5,"pt"),
      axis.text = element_text(size=unit(6,"pt"), margin=margin(t=5, r=5, l=5, b=5), hjust=0),
      plot.caption=element_text(size = unit(6,"pt"), face = "italic")))

ggsave("g2_repere.svg" |> glue(), plot=g2_repere, width=8, height=4.4, units="cm")
