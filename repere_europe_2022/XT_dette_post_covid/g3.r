library(scales)
library(lubridate) 
library(ggpmisc)
library(vroom)
library(eurostat)
library(data.table)
library(glue)
library(ggrepel)
library(lubridate)
library(patchwork)
require(timeSeries)
require(xts)
require(tsbox)
library(broom)

library(plotly)
library(tidyverse)

irl.m.raw <- get_eurostat(id="irt_lt_mcby_m")
debt.q.raw <- get_eurostat(id="gov_10q_ggdebt") |>
  mutate(var_label = label_eurostat(na_item, dic="na_item")) |> 
  dplyr::filter(sector=="S13", unit=="PC_GDP",na_item == "GD") |> 
  mutate(y = year(time), q= quarter(time), m = month(time)+2)
# and labels
geo_labels <- label_eurostat(irl.m.raw |> select(geo) |> distinct(), fix_duplicated = TRUE, code="geo") |> rename(label=geo, geo=geo_code)
var_labels <- label_eurostat(irl.m.raw |> distinct(int_rt), fix_duplicated = TRUE, code="int_rt") 
ggplotly(irl.m.raw |> dplyr::filter(geo=="FR") |> ggplot()+geom_line(aes(x=time, y=values)))

debt.m <- left_join(expand(debt.q.raw, sector, geo, y, m=1:12),
                        debt.q.raw,
                        by=c("sector", "geo","y", "m")) |> 
  select(-na_item, -unit, -var_label,-q) |> 
  group_by(geo) |> 
  arrange(geo,y,m) |> 
  mutate(md = na.spline(values),
         time = ym(str_c(y," ",m))) 

data <-  left_join(irl.m.raw |> select(-int_rt, int=values), debt.m |> select(-sector, -y,-m, -values), by=c("geo", "time"))

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

data_ez <-  data |>
  left_join(eurozone |> dplyr::select(iso, Adopted, Population), by=c("geo"="iso")) |>
  drop_na(Adopted) |> 
  dplyr::filter(time>=dmy(Adopted))

ggplot(data_ez |> dplyr::filter(month(time)==6) |>  drop_na(int, md), aes(x = md, y=int)) +
  geom_point(aes(color=Adopted, size=Population)) +
  geom_smooth(method = "lm") +
  facet_wrap(vars(time))

est <- data_ez |>
  drop_na(int, md) |> 
  group_by(time) |> 
  mutate(dint = int - int[geo=="DE"],
         ddet = md - md[geo=="DE"]) |> 
  dplyr::filter(!geo%in%c("DE")) |> 
  summarize(mm = list(tidy(lm(dint~ddet+0, data=cur_data())))) |> 
  unnest(mm)

est_el <- data_ez |>
  drop_na(int, md) |> 
  group_by(time) |> 
  mutate(dint = int - int[geo=="DE"],
         ddet = md - md[geo=="DE"]) |> 
  dplyr::filter(!geo%in%c("DE", "EL")) |> 
  summarize(mm = list(tidy(lm(dint~ddet+0, data=cur_data())))) |> 
  unnest(mm)

(g3_repere <- ggplot(est |> dplyr::filter(term == "ddet", time > "2000-01-01")) +
    geom_ribbon(data = est_el |> dplyr::filter(term=="ddet", time> "2000-01-01"),
                aes(x=time, ymin = estimate-1*std.error, ymax= estimate+1*std.error), alpha=0.3, fill="gray65") +
    geom_line(aes(x=time, y=estimate), linetype="dotted") + 
    geom_ribbon(aes(x=time, ymin = estimate-1*std.error, ymax= estimate+1*std.error), alpha=0.3, fill="gray85") +
    geom_line(data = est_el |> dplyr::filter(term=="ddet", time> "2000-01-01"), aes(x=time, y=estimate), linetype="solid") + 
    scale_x_date(breaks = ym(str_c(seq(2000, 2020, 5), " 01")), guide="axis_minor", minor_breaks =  ym(str_c(seq(2000, 2021, 1), " 01")), date_labels = "%Y", name="") +
    ylab("Lien entre dette et taux souverain") +
    annotate("text", label="ZE sans la Grèce", size=2, x=ym("2013 01"), y=-0.01, family="Nunito", fontface="italic", hjust=0) +
    annotate("text", label="ZE y.c. la Grèce", size=2, x=ym("2013 06"), y=0.15, family="Nunito", fontface="italic", hjust=0) +
    scale_y_continuous(guide="axis_minor") +
    guides(color="none") +
    theme_void(base_family="Nunito", base_size=6) +
    theme(
      axis.line = element_line(color="black", size=0.2, linetype = 1, lineend = "butt"),
      panel.border = element_rect(color="black", fill=NA, size=0.2, linetype = 1),
      axis.ticks = element_line(color="black", size=0.1),
      axis.ticks.length = -unit(2.5,"pt"),
      axis.text = element_text(size=unit(6,"pt"), margin=margin(t=5, r=5, l=5, b=5), hjust=0),
      plot.caption=element_text(size = unit(6,"pt"), face = "italic")))

ggsave("g3_repere.svg" |> glue(), plot=g3_repere, width=8, height=4.4, units="cm")

