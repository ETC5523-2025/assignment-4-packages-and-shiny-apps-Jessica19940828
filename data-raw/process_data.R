# 載入套件
library(readr)
library(dplyr)
library(usethis)
library(here)
library(tidyr) # <-- 這個套件現在是關鍵

# --- 1. 讀取「年度」資料 (這些是正確的) ---
fwi_data <- readr::read_table(
  here::here("data-raw", "fwi_data.txt"),
  col_names = c("year", "fwi_sm")
)

msr_data <- readr::read_table(
  here::here("data-raw", "msr_data.txt"),
  col_names = c("year", "msr_sm")
)

tmax_data <- readr::read_table(
  here::here("data-raw", "tmax_data.txt"),
  col_names = c("year", "tmax")
)

# --- 2. 讀取「寬格式月資料」(這是修正) ---

# 定義 13 個欄位的名稱
month_col_names <- c("year", "Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# 使用 13 個欄位名稱來讀取檔案
iod_data_wide <- readr::read_table(
  here::here("data-raw", "iod_data.txt"),
  col_names = month_col_names
)

sam_data_wide <- readr::read_table(
  here::here("data-raw", "sam_data.txt"),
  col_names = month_col_names
)

precip_data_wide <- readr::read_table(
  here::here("data-raw", "precip_data.txt"),
  col_names = month_col_names
)

area_data_wide <- readr::read_table(
  here::here("data-raw", "area_data.txt"),
  col_names = month_col_names
)

# --- 3. 將寬資料轉換為長資料 ---

# 這是一個輔助函數，用來將 13 欄轉為 3 欄
pivot_monthly_data <- function(wide_data, value_col_name) {
  wide_data %>%
    tidyr::pivot_longer(
      cols = -year, # 旋轉 "year" 以外的所有欄位
      names_to = "month_name",
      values_to = value_col_name
    ) %>%
    # 將 "Jan", "Feb" 轉為 1, 2, ...
    mutate(month = match(month_name, month.abb))
}

# 轉換所有月資料
iod_data_long <- pivot_monthly_data(iod_data_wide, "iod")
sam_data_long <- pivot_monthly_data(sam_data_wide, "sam")
precip_data_long <- pivot_monthly_data(precip_data_wide, "precip")
area_data_long <- pivot_monthly_data(area_data_wide, "area_burned")

# --- 4. 處理季節性資料 (這部分不變) ---

precip_season <- precip_data_long %>%
  mutate(fire_season_year = if_else(month <= 2, year - 1, year)) %>%
  filter(month %in% c(1, 2, 9, 10, 11, 12)) %>% # 火災季 Sep-Feb
  group_by(fire_season_year) %>%
  summarise(precip_total = sum(precip, na.rm = TRUE)) %>%
  rename(year = fire_season_year)

iod_season <- iod_data_long %>%
  filter(month >= 9 & month <= 12) %>% # 論文關注 Sep-Dec
  group_by(year) %>%
  summarise(iod_mean = mean(iod, na.rm = TRUE))

sam_season <- sam_data_long %>%
  filter(month >= 9 & month <= 12) %>% # 論文關注 Sep-Dec
  group_by(year) %>%
  summarise(sam_mean = mean(sam, na.rm = TRUE))

area_season <- area_data_long %>%
  mutate(fire_season_year = if_else(month <= 2, year - 1, year)) %>%
  filter(month %in% c(1, 2, 9, 10, 11, 12)) %>% # 火災季 Sep-Feb
  group_by(fire_season_year) %>%
  summarise(area_burned = sum(area_burned, na.rm = TRUE)) %>%
  rename(year = fire_season_year)

# --- 5. 合併並儲存 (這部分不變) ---
all_data <- fwi_data %>%
  full_join(msr_data, by = "year") %>%
  full_join(tmax_data, by = "year") %>%
  full_join(precip_season, by = "year") %>%
  full_join(iod_season, by = "year") %>%
  full_join(sam_season, by = "year") %>%
  full_join(area_season, by = "year")

ausbushfire_data <- all_data %>%
  filter(year >= 1979 & year <= 2019)

usethis::use_data(ausbushfire_data, overwrite = TRUE)

