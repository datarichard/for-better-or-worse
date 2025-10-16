library(tidyverse)
require(lcmm)

lefnw <- read_rds("../HILDA/data/quarterly events long.rds") |> 
  filter(event == "lefnw") |> 
  select(year, xwaveid, qtr, val) |> 
  mutate(year = parse_number(year))

swb <- read_rds("../HILDA/data/SWB.rds") |> 
  select(wave, xwaveid, ghmh, age, sex, SEIFA, chronic, edu) |>
  filter(xwaveid %in% lefnw$xwaveid) |> 
  mutate(year = parse_number(wave))

other_events <- read_rds("../HILDA/data/events.rds") |> 
  select(-Bankruptcy) |> 
  filter(xwaveid %in% lefnw$xwaveid) |> 
  mutate(year = year - 2000) |> 
  rename_with(.fn = str_to_lower) |> 
  rename_with(.fn = make.names)

df <- left_join(select(swb, xwaveid, year, ghmh), lefnw,
                by = join_by(xwaveid, year)) |> 
  mutate(
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
  select(xwaveid, time, ghmh, year) |> 
  ungroup()

cov <- left_join(df, select(swb, -ghmh, -wave),
                 by = join_by(xwaveid, year)) |> 
  group_by(xwaveid) |> 
  summarise(
    edu = max(edu, na.rm=T),
    age = mean(age, na.rm=T),
    SEIFA = mean(SEIFA, na.rm=T),
    period = mean(year),
    female = as.numeric(any(sex %in% "Female")),
    chronic = mean(chronic)
  ) |> 
  ungroup() |> 
  mutate(across(c(age, SEIFA, period, chronic), ~c(scale(.x)))) 

jdf <- left_join(df, cov, by = join_by(xwaveid)) |> 
  left_join(other_events, by = join_by(xwaveid, year)) |> 
  mutate(across(where(is.double), ~replace_na(.x, 0)))

f <- formula(
  paste("ghmh ~",
        paste(c("time", "I(time^2)", "I(time^3)", "female",
                colnames(other_events)[c(-1, -2)]), 
              collapse= " + "))
)


fjdf <- jdf |> 
  # filter(xwaveid %in% sample(unique(jdf$xwaveid), 100)) |>
  mutate(xwaveid = parse_integer(xwaveid)) |> 
  as.data.frame()

# Estimate base model
lcmm1 <- hlme(fixed = f, # ghmh ~ time + I(time^2) + I(time^3) + female,
              random = ~1 + time,
              subject = 'xwaveid', 
              ng = 1,
              data = fjdf)


# Estimation considering 3 classes : 
lcmm3 <- hlme(ghmh ~ time + I(time^2) + I(time^3) + female,
       random =~ time + I(time^2) + I(time^3),
       subject = 'xwaveid', 
       data = fjdf, 
       classmb =~ SEIFA + chronic + edu + age + period, # dropped
       ng = 3, 
       nproc = 3,
       mixture =~time + I(time^2) + I(time^3),
       B = random(lcmm1))
lcmm3
summary(lcmm3)
postprob(lcmm3)
plot(lcmm3, which = "fit", var.time="time", marg=FALSE, shades = TRUE)


lcmm3 <- gridsearch(
  hlme(fixed = f, # ghmh ~ time + I(time^2) + I(time^3) + female,
       random = ~1 + time,
       subject = 'xwaveid', 
       data = fjdf, 
       classmb =~ chronic + edu + SEIFA + age + period,
       ng = 3, 
       nproc = 3,
       mixture =~time + I(time^2) + I(time^3)),
  rep=100, maxiter=30, minit=lcmm1
)
postprob(lcmm3)
plot(lcmm3, which = "fit", var.time="time", marg=FALSE, shades = TRUE)

write_rds(lcmm3, "results/ghmh_lefnw_3group_cov.rds")


predictY(lcmm3, 
         newdata = expand(fjdf, time, female, SEIFA, chronic, edu),
         var.time = "time") |> 
  plot(col=c("red","navy", "olive"), lty=1, lwd=5, legend=NULL)


