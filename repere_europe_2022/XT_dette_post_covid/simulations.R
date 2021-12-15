# init --------------
# remotes::install_github("jthomasmock/gtExtras")
# devtools::install_github("rstudio/gt")
library(rlang)
library(future)
library(shiny)
library(progressr)
library(odin.dust)
library(dust)
library(odin)
library(microbenchmark)
library(scales)
library(furrr)
library(paletteer)
library(lubridate)
library(glue)
library(gt)
library(furrr)
library(ggrepel)
library(tidyverse)
library(patchwork)
library(countrycode)
library(gtExtras)

br <-  paletteer_d("miscpalettes::brightPastel")
col_4 <- scale_color_manual(values=br[c(6,2,3, 1)], aesthetics = c("color", "fill")) 
col_4_prog <-scale_color_manual(values=colorspace::sequential_hcl(n=4, palette = "Viridis"))

future::plan("multisession", workers=4)
progressr::handlers(progressr::handler_progress(format = ":bar :percent :eta", width = 80))
walk(list.files("dwr/R/", "*.[r|R]"), ~ source("R/{.x}" |> glue(), encoding="UTF-8"))

pays_1 <- c("FRA", "ITA", "DEU", "ESP", "NLD", "BEL", "GRC", "PRT", "AUT", "IRL")
pays_2 <- c( "GRC", "PRT")
source("ze.r")

plan(multisession, workers=4)
with_progress({
  cases <- cross(list(country=ez, loss_t=c(20, 40), og_dep=c(1, 1.2, 1.5), dette=c(0.6, 0.9, 4), ib_cap=c(0.05, 0.005), ec=c(0, NA), r_dette=c(0, 0.025)))
  pb <- progressor(along=cases)
  sim <- future_map_dfr(cases, ~{
    pb()
    init_cty <- .x[[1]]
    init_sy <- 2023
    globals <- set_globals(init_cty, version = "x", ameco_version="11/2021")
    
    suppressMessages(
      globals$model <- odin.dust::odin_dust("odin/dwrstochasticmodel.r", options = odin_options(target="c", verbose = FALSE)),
      classes = c("message", "warning"))
    
    dandp <- dataandparams(country = init_cty, start_year = init_sy, globals = globals, periods = 50, draws = 1000)
    effective_dstar <- min(.x[[4]], dandp$historical_data |> filter(year==2023) |> pull(dettep))
    p <- list_modify(dandp$p_def,
                     start_year = init_sy,
                     start_hist = 2007,
                     end_year = 2100,
                     go_mc  = TRUE,
                     dstar = effective_dstar,
                     r_dette = .x[[7]],
                     loss_t = .x[[2]],
                     loss_ib = 0.2,
                     loss_og = 0.2,
                     loss_df = 0, 
                     loss_d = 25,
                     potdep_star = 1,
                     infdep_star = 1,
                     ecstar = case_when(
                       is.na(.x[[6]]) ~ dandp$historical_data |> filter(year==2023) |> pull(ec),
                       TRUE ~ .x[[6]]),
                     og_dep = .x[[3]],
                     pcpo = 0,
                     draws=1000,
                     ibcap = .x[[5]], 
                     periods=50,
                     seed = 42)
    pp <- set_params(p, globals, dandp) 
    bp <- calc_rule_params(globals, params=pp$p, draws=1000)
    calc_sim_dust(bp, globals$model, history=pp$h) |>
      mutate(country = .x[[1]],
             loss_t = .x[[2]], 
             og_dep = .x[[3]], 
             dette = .x[[4]], 
             ib_cap = .x[[5]], 
             ec = .x[[6]], 
             r_dette = .x[[7]],
             e_dstar = effective_dstar)},
    options=furrr_options(seed=TRUE))
})

plot_sim <- sim |> 
  filter(variable%in% c("dettep", "ci", "tdep", "og", "ib", "tdeppp", "tdepppp")) |> 
  group_by(country, og_dep, loss_t, dette, ib_cap, ec, r_dette) |> 
  mutate(ok = near(q0.5[variable=="dettep"&year==2023+loss_t+10], e_dstar, tol=0.2)) |> 
  ungroup() |> 
  filter(ok) |>
  arrange(year) |> 
  group_by(country, og_dep, loss_t, dette, ib_cap, ec, variable, r_dette) |> 
  mutate(
    q0.5_sm = slider::slide_dbl(q0.5, mean, .before = 1, .after = 1),
    q0.5_sm = if_else(year<=2023, q0.5, q0.5_sm)
  ) |> 
  ungroup()

plot_sim |> group_by(loss_t, dette, og_dep, ib_cap, ec, r_dette) |>  summarize(n_distinct(country)) |> print(n=80)

(simulations <- imap(
  c(dettep = "Dette publique", og = "Ecart de production", tdep = "Dépenses publiques", ci = "Charge d intérêts"),
  ~ggplot(data=plot_sim |> filter(variable == .y, og_dep==0.7), mapping=aes(x=year, y=q0.5))+
    geom_line(data = ~filter(.x, year>=2023), aes(color=country), size =0.5, linetype="dotted") + 
    geom_line(data = ~filter(.x, year<=2023), aes(color=country), size = 0.5) + 
    geom_text_repel(data = ~filter(.x, year==2007),
                    aes(label = country, color=country),
                    direction="both",
                    size=1.5,
                    min.segment.length=0,
                    xlim=c(NA, 2007),
                    segment.size=0.1,
                    show.legend = FALSE,
                    max.overlaps=15) +
    # geom_ribbon(aes( x=year, ymin=q0.025, ymax=q0.975, fill=country), alpha = 0.1, show.legend=FALSE) +
    theme_minimal(base_family = "Nunito", base_size = 8) +
    theme(
      strip.text = element_text(size=9, hjust=0.5, face="bold"),
      legend.text = element_text(size=6)) +
    labs(subtitle = "en % du PIB", col = "Pays", caption = "AMECO 11/2021, OFCE, simulation Debtwatch à partir de 2023")+
    xlab("")+
    ylab("")+
    labs(title = .x)+
    facet_grid(rows=vars(dette), cols=vars(loss_t))+
    # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
    scale_y_continuous(labels = label_percent(1))+
    scale_x_continuous(limits = c(2000, 2050), breaks = c(2007, 2023, seq(2010,2050,10)))
))

ggplot(data=plot_sim |> 
         filter(country=="MLT",variable == "tdep", loss_t==20, is.na(ec)) |>
         mutate(og_dep = factor(og_dep)), mapping=aes(x=year, y=q0.5_sm, color = og_dep, group=og_dep))+
  geom_line(data = ~filter(.x, year>=2023), size =0.5, linetype="dotted") + 
  geom_line(data = ~filter(.x, year<=2023), size = 0.5) + 
  geom_text_repel(data = ~filter(.x, year==2007),
                  aes(label = country, color=og_dep),
                  direction="both",
                  size=1.5,
                  min.segment.length=0,
                  xlim=c(NA, 2007),
                  segment.size=0.1,
                  show.legend = FALSE,
                  max.overlaps=15) +
  # geom_ribbon(aes( x=year, ymin=q0.025, ymax=q0.975, fill=country), alpha = 0.1, show.legend=FALSE) +
  theme_minimal(base_family = "Nunito", base_size = 8) +
  theme(
    strip.text = element_text(size=9, hjust=0.5, face="bold"),
    legend.text = element_text(size=6)) +
  labs(subtitle = "en % du PIB", col = "Multiplicateur", caption = "AMECO 11/2021, OFCE, simulation Debtwatch à partir de 2023")+
  xlab("")+
  ylab("")+
  labs(title = "Dépenses")+
  facet_grid(vars(dette), vars(ib_cap))+
  # scale_x_continuous(limits = c(2023, 2050), breaks =pretty_breaks(5) ) + 
  scale_y_continuous(labels = label_percent(1))+
  scale_color_discrete()+
  scale_x_continuous(limits = c(2000, 2050), breaks = c(2007, 2023, seq(2010,2050,10)))

patchwork::wrap_plots(simulations)+plot_layout(guides="collect")

ggsave(plot=simulations, filename="pb/XT/simulations.svg", width = 180, height = 140, units = "mm")

ddep <- plot_sim |>
  select(-q0.025, -q0.975) |> 
  pivot_wider(names_from=variable, values_from = c(q0.5, q0.5_sm), names_glue="{variable}{str_remove(.value, 'q0.5')}") |> 
  group_by(country, loss_t, dette, ib_cap, og_dep, ec, r_dette) |>  
  arrange(year) |> 
  mutate(d_dep = tdep-lag(tdep),
         is_austerity = ib <= -0.001,
         tdep_a = tdep*(1+og)) |> 
  summarize(
    tdep_min = min(tdep_a[between(year,2023, 2070)]),
    duree_aus = which.min(is_austerity[year>2023])-1,
    tdep_wm = tdep_a[year==2023+duree_aus],
    tdep_ref = tdep_a[year==2023],
    tdep_10s = mean(tdep_a[between(year, 2009, 2019)]),
    tdep_lt = tdeppp[year==2023+loss_t+5]) |>
  ungroup() |> 
  mutate(ddep =  - tdep_ref + tdep_min,
         ddep_lt = - tdep_ref + tdep_lt,
         duree_aus)

(table_ec0 <- ddep |> 
    filter(loss_t == 40, ib_cap==0.005, ec==0, og_dep==1.2, r_dette==0.0) |>  
    transmute( dette,
               country =countrycode(country,"iso3c", "un.name.fr"),
               ddep, 
               ddep_lt,
               duree_aus) |> 
    pivot_wider(names_from = dette, values_from=c(ddep, ddep_lt, duree_aus)) |> 
    ungroup() |> 
    relocate(starts_with("duree"), .after=ddep_4) |> 
    arrange(ddep_0.6) |> 
    mutate(across(starts_with("ddep"), ~ifelse(.x>=-0.0005, NA_real_, .x)),
           across(starts_with("duree"), ~ifelse(.x<=1, NA_real_, .x))) |> 
    gt(rowname_col = "country") |> 
    tab_spanner(label = "dette", columns = -country) |> 
    fmt_percent(columns = starts_with("ddep"), decimals=0) |> 
    fmt_number(columns = starts_with("duree"), decimals=0, pattern="{x}a") |> 
    cols_label(ddep_0.6 = "60%",
               ddep_0.9 = "90%",
               ddep_4 = "stable",
               duree_aus_0.6 = "60%",
               duree_aus_0.9 = "90%",
               duree_aus_4 = "stable",
               ddep_lt_0.6 = "60%",
               ddep_lt_0.9 = "90%",
               ddep_lt_4 = "stable") |>
    tab_spanner(label = "moyen terme",
                c(ddep_0.6, ddep_0.9, ddep_4)) |> 
    tab_spanner(label = "long terme",
                c(ddep_lt_0.6, ddep_lt_0.9, ddep_lt_4)) |> 
    tab_spanner(label = "durée consolidation",
                c(duree_aus_0.6, duree_aus_0.9, duree_aus_4)) |> 
    fmt_missing(columns=starts_with("ddep"),missing_text = "-") |> 
    fmt_missing(columns=starts_with("duree"),missing_text = "-") |>
    opt_row_striping() |> 
    opt_table_font(font = google_font("Source Serif Pro")) |> 
    tab_style(style = cell_borders(sides = c("top", "bottom"), color="black", weight = px(1), style="solid"),
              location = list(cells_body(), cells_stub(), cells_row_groups())) |> 
    tab_options(row.striping.include_stub = TRUE, 
                stub.border.style = "none",
                heading.border.bottom.color = "#FFFFFF",
                table.border.top.color = "#FFFFFF",
                column_labels.border.bottom.color = "#FFFFFF",
                column_labels.border.top.color = "#FFFFFF",
                table_body.border.bottom.color = "#FFFFFF",
                heading.border.bottom.width = px(1),
                column_labels.border.top.width = px(1),
                column_labels.border.bottom.width = px(1),
                table_body.border.bottom.width = px(1),
                table_body.border.top.width = px(1),
                table.border.top.width = px(1) ))

(table_ecstable <- ddep |> 
    filter(loss_t == 20, ib_cap==0.005, is.na(ec), og_dep==1.2, r_dette==0.025) |>  
    transmute( dette,
               country =countrycode(country,"iso3c", "un.name.fr"),
               ddep, 
               ddep_lt,
               duree_aus) |> 
    pivot_wider(names_from = dette, values_from=c(ddep, ddep_lt, duree_aus)) |> 
    ungroup() |> 
    relocate(starts_with("duree"), .after=ddep_4) |> 
    arrange(ddep_0.6) |> 
    mutate(across(starts_with("ddep"), ~ifelse(.x>=-0.0005, NA_real_, .x)),
           across(starts_with("duree"), ~ifelse(.x<=1, NA_real_, .x))) |> 
    gt(rowname_col = "country") |> 
    tab_spanner(label = "dette", columns = -country) |> 
    fmt_percent(columns = starts_with("ddep"), decimals=0) |> 
    fmt_number(columns = starts_with("duree"), decimals=0, pattern="{x}a") |> 
    cols_label(ddep_0.6 = "60%",
               ddep_0.9 = "90%",
               ddep_4 = "stable",
               duree_aus_0.6 = "60%",
               duree_aus_0.9 = "90%",
               duree_aus_4 = "stable",
               ddep_lt_0.6 = "60%",
               ddep_lt_0.9 = "90%",
               ddep_lt_4 = "stable") |>
    tab_spanner(label = "moyen terme",
                c(ddep_0.6, ddep_0.9, ddep_4)) |> 
    tab_spanner(label = "long terme",
                c(ddep_lt_0.6, ddep_lt_0.9, ddep_lt_4)) |> 
    tab_spanner(label = "durée consolidation",
                c(duree_aus_0.6, duree_aus_0.9, duree_aus_4)) |> 
    fmt_missing(columns=starts_with("ddep"),missing_text = "-") |> 
    fmt_missing(columns=starts_with("duree"),missing_text = "-") |>
    opt_row_striping() |> 
    opt_table_font(font = google_font("Source Serif Pro")) |> 
    tab_style(style = cell_borders(sides = c("top", "bottom"), color="black", weight = px(1), style="solid"),
              location = list(cells_body(), cells_stub(), cells_row_groups())) |> 
    tab_options(row.striping.include_stub = TRUE, 
                stub.border.style = "none",
                heading.border.bottom.color = "#FFFFFF",
                table.border.top.color = "#FFFFFF",
                column_labels.border.bottom.color = "#FFFFFF",
                column_labels.border.top.color = "#FFFFFF",
                table_body.border.bottom.color = "#FFFFFF",
                heading.border.bottom.width = px(1),
                column_labels.border.top.width = px(1),
                column_labels.border.bottom.width = px(1),
                table_body.border.bottom.width = px(1),
                table_body.border.top.width = px(1),
                table.border.top.width = px(1) ))

gtsave(table_ecstable, "ecstable.rtf")
gtsave(table_ec0, "ec0.rtf")

cc1 <- ddep |> 
  filter(loss_t == 20, ib_cap==0.005, is.na(ec), og_dep==1.2) |>  
  transmute( dette,
             country,
             ddep, 
             ddep_lt,
             duree_aus) |> 
  pivot_wider(names_from = dette, values_from=c(ddep, ddep_lt, duree_aus)) |> 
  ungroup() |> 
  relocate(starts_with("duree"), .after=ddep_4) |> 
  arrange(ddep_0.6) |> 
  filter(duree_aus_0.6 > 10) |> pull(country)
eurozone |> filter(iso3c%in%cc1) |> pull(Relative_GNI) |> str_remove("%") |> as.numeric() |> sum()


ccnec <- ddep |> 
  filter(loss_t == 20, ib_cap==0.005, is.na(ec), og_dep==1.2) |>  
  transmute( dette,
             country,
             ddep, 
             ddep_lt,
             duree_aus) |> 
  pivot_wider(names_from = dette, values_from=c(ddep, ddep_lt, duree_aus)) |> 
  ungroup() |> 
  relocate(starts_with("duree"), .after=ddep_4)
ccecn <- ddep |> 
  filter(loss_t == 20, ib_cap==0.005, ec==0, og_dep==1.2) |>  
  transmute( dette,
             country,
             ddep, 
             ddep_lt,
             duree_aus) |> 
  pivot_wider(names_from = dette, values_from=c(ddep, ddep_lt, duree_aus)) |> 
  ungroup() |> 
  relocate(starts_with("duree"), .after=ddep_4)