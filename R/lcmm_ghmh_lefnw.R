library(tidyverse)
require(lcmm)

lefnw <- read_rds("../HILDA/data/quarterly events long.rds") |> 
  filter(event == "lefnw") |> 
  select(year, xwaveid, qtr, val)

swb <- read_rds("data/SWB.rds") |> 
  select(year = wave, xwaveid, ghmh, age, sex) |> 
  filter(xwaveid %in% lefnw$xwaveid)


df <- left_join(swb, lefnw, by = join_by(xwaveid, year)) |> 
  mutate(
    xwaveid = as.integer(xwaveid),
    year = parse_number(year),
    age = c(scale(age)),
    val = replace_na(val, 0),
    qtr = recode(qtr, "1" = 0, "2" = 4/12, "3" = 7/12, "4" = 10/12,
                 .missing = 0)) |> 
  arrange(xwaveid, year) |> 
  group_by(xwaveid) |> 
  mutate(cval = cumsum(val)) |> 
  filter(cval < 2) |> 
  mutate(
    event_year = year[which(val == 1)],
    year_pst = year - event_year,
    time = year_pst + qtr
  ) |> 
  filter(!is.na(ghmh)) |> 
  filter(between(time, -3, 3)) |> 
  filter(min(time) == -3) |> 
  # mutate(ghmh = ghmh - mean(ghmh, na.rm=T)) |> 
  select(xwaveid, time, ghmh, age, sex) |> 
  ungroup()

fdf <- df |> 
  # filter(xwaveid %in% sample(unique(df$xwaveid), 1000)) |>
  as.data.frame()
  

ghmh.m1 <- hlme(ghmh ~ time + I(time^2) + I(time^3) + sex,
           random =~ time + I(time^2) + I(time^3),
           subject = 'xwaveid', 
           ng = 1,
           data = fdf)

write_rds(ghmh.m1, "results/ghmh_lefnw_1group.rds")

# Estimation considering 2 classes : 
ghmh.m2 <- gridsearch(
  hlme(ghmh ~ time + I(time^2) + I(time^3) + sex,
           random =~ time + I(time^2) + I(time^3),
           subject = 'xwaveid', 
           data = fdf, 
           ng = 2, 
           nproc = 2,
           mixture =~time + I(time^2) + I(time^3)),
  rep=100, maxiter=30, minit=ghmh.m1
)

write_rds(ghmh.m2, "results/ghmh_lefnw_2group.rds")
postprob(ghmh.m2)           
plot(ghmh.m2, which="fit", var.time="time", marg=FALSE, shades = TRUE)  


# Estimation considering 3 classes : 
ghmh.m3 <- gridsearch(
  hlme(ghmh ~ time + I(time^2) + I(time^3) + sex,
           random =~ time + I(time^2) + I(time^3),
           subject = 'xwaveid', 
           data = fdf, 
           ng = 3, 
           nproc = 3,
           mixture =~time + I(time^2) + I(time^3)),
  rep=100, maxiter=30, minit=ghmh.m1
)

write_rds(ghmh.m3, "results/ghmh_lefnw_3group.rds")
postprob(ghmh.m3)           
plot(ghmh.m3, which="fit", var.time="time", marg=FALSE, shades = TRUE)  


# Estimation considering 4 classes : 
ghmh.m4 <- gridsearch(
  hlme(ghmh ~ time + I(time^2) + I(time^3) + sex,
           random =~ time + I(time^2) + I(time^3),
           subject = 'xwaveid', 
           data = fdf, 
           ng = 4, 
           nproc = 4, 
           mixture =~time + I(time^2) + I(time^3)),
  rep=100, maxiter=30, minit=ghmh.m1
)

write_rds(ghmh.m4, "results/ghmh_lefnw_4group.rds")
postprob(ghmh.m4)           
plot(ghmh.m4, which="fit", var.time="time", marg=FALSE, shades = TRUE)  


summarytable(ghmh.m1, ghmh.m2, ghmh.m3, ghmh.m4,
  which = c("G", "loglik", "conv", "npm", "AIC", "BIC", "SABIC", "entropy",
            "ICL", "%class")
)

summaryplot(ghmh.m1, ghmh.m2, ghmh.m3, ghmh.m4, 
            which = c("BIC", "entropy","ICL"))
             

             