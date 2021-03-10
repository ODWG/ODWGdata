library(tidyverse)
library(lubridate)
library(wqpr)


flat.test = wqp_result_details() %>%
  filter(
    cdec_code == "HUN",
    analyte_name == "Specific Conductance",
    reading_type_name == "Time Series"
  ) %>%
  mutate(data = wqp_result_data(result_id,
    "2021-01-25", "2021-02-05", version = 1L)) %>%
  select(cdec_code, analyte_name, unit_name, data) %>%
  unnest(data) %>%
  select(-result_id, - version, -qaqc_flag_id, -row)

ggplot(flat.test) +
  aes(x = time, y = value) +
  geom_line() +
  scale_y_continuous("Specific Conductance (µS/cm)",
    labels = scales::comma) +
  xlab(NULL)
#write.csv(flat.test, "smb-flat-example.csv", row.names = FALSE)
#ggsave("smb-flat-example.png", width = 9.5, height = 6.5)


rate.test = wqp_result_details() %>%
  filter(
    cdec_code == "PEL",
    analyte_name == "Stage",
    reading_type_name == "Time Series"
  ) %>%
  mutate(data = wqp_result_data(result_id,
    "2020-04-01", "2020-04-10", version = 1L)) %>%
  select(cdec_code, analyte_name, unit_name, data) %>%
  unnest(data) %>%
  select(-result_id, - version, -qaqc_flag_id, -row)

ggplot(rate.test) +
  aes(x = time, y = value) +
  geom_line() +
  scale_y_continuous("Stage (ft)",
    labels = scales::comma) +
  xlab(NULL)
#write.csv(rate.test, "smb-rate-example.csv", row.names = FALSE)
#ggsave("smb-rate-example.png", width = 9.5, height = 6.5)


# spike/dip
spike.test = wqp_result_details() %>%
  filter(
    cdec_code == "HUN",
    analyte_name == "Specific Conductance",
    reading_type_name == "Time Series"
  )  %>%
  mutate(data = wqp_result_data(result_id,
    "2021-02-15", "2021-02-18", version = 1L)) %>%
  select(cdec_code, analyte_name, unit_name, data) %>%
  unnest(data) %>%
  select(-result_id, - version, -qaqc_flag_id, -row)

ggplot(spike.test) +
  aes(x = time, y = value) +
  geom_line() +
  scale_y_continuous("Specific Conductance (µS/cm)",
    labels = scales::comma) +
  xlab(NULL)
#write.csv(spike.test, "smb-spike-example.csv", row.names = FALSE)
#ggsave("smb-spike-example.png", width = 9.5, height = 6.5)


# anomaly/noise
anomaly.test = wqp_result_details() %>%
  filter(
    cdec_code == "HUN",
    analyte_name == "Specific Conductance",
    reading_type_name == "Time Series"
  )   %>%
  mutate(data = wqp_result_data(result_id,
    "2021-02-12", "2021-03-12", version = 1L)) %>%
  select(cdec_code, analyte_name, unit_name, data) %>%
  unnest(data) %>%
  select(-result_id, - version, -qaqc_flag_id, -row)]

ggplot(anomaly.test) +
  aes(x = time, y = value) +
  geom_line() +
  scale_y_continuous("Specific Conductance (µS/cm)",
    labels = scales::comma) +
  xlab(NULL)
#write.csv(anomaly.test, "smb-anomaly-example.csv", row.names = FALSE)
#ggsave("smb-anomaly-example.png", width = 9.5, height = 6.5)
