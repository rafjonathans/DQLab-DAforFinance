library(tidyverse)
library(scales)
library(lubridate)


df_loan <- read.csv('https://dqlab-dataset.s3-ap-southeast-1.amazonaws.com/loan_disbursement.csv', stringsAsFactors = F)
head(df_loan)

df_loan_mei <- df_loan %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31')  %>% 
  group_by(cabang) %>% 
  summarise(total_amount = sum(amount))
df_loan_mei

# Top 5 branches
df_loan_mei %>% 
  arrange(desc(total_amount)) %>% 
  mutate(total_amount = comma(total_amount)) %>% 
  head(5)
  

# Bottom 5 branches
df_loan_mei %>% 
  arrange(total_amount) %>% 
  mutate(total_amount = comma(total_amount)) %>% 
  head(5)

# Branch age 
df_cabang_umur <- df_loan %>%
  group_by(cabang) %>% 
  summarise(pertama_cair = min(tanggal_cair)) %>% 
  mutate(umur = as.numeric(as.Date('2020-05-15') - as.Date(pertama_cair)) %/% 30) 
df_cabang_umur


# Merge tables
df_loan_mei_umur <- df_cabang_umur %>%
  inner_join(df_loan_mei, by = 'cabang')
df_loan_mei_umur


# Plot age to performance
ggplot(df_loan_mei_umur, aes(x = umur, y = total_amount)) +
  geom_point() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Semakin berumur, performa cabang akan semakin membaik",
       x = "Umur (bulan)",
       y = "Total Amount")


# Add performance status
df_loan_mei_flag <- df_loan_mei_umur %>% 
  group_by(umur) %>% 
  mutate(Q1 = quantile(total_amount, 0.25),
         Q3 = quantile(total_amount, 0.75),
         IQR = (Q3-Q1)) %>%  
  mutate(flag = ifelse(total_amount < (Q1 - IQR), 'rendah','baik'))

df_loan_mei_flag %>% 
  filter(flag == 'rendah') %>% 
  mutate_if(is.numeric, funs(comma))


# Plot age to performance with performance status 
ggplot(df_loan_mei_flag, aes(x = umur, y = total_amount)) +
  geom_point(aes(color = flag)) +
  scale_color_manual(breaks = c("baik", "rendah"),
                     values=c("blue", "red")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Ada cabang berpeforma rendah padahal tidak termasuk bottom 5 nasional",
       color = "",
       x = "Umur(bulan)",
       y = "Total Amount")

# Get tables that summarize performance at same age
df_loan_mei_flag %>% 
  filter(umur == 3) %>% 
  inner_join(df_loan, by = 'cabang') %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31') %>% 
  group_by(cabang, flag) %>% 
  summarise(jumlah_hari = n_distinct(tanggal_cair),
            agen_aktif = n_distinct(agen),
            total_loan_cair = n_distinct(loan_id),
            avg_amount = mean(amount), 
            total_amount = sum(amount)) %>% 
  arrange(total_amount) %>% 
  mutate_if(is.numeric, funs(comma))


# Check branch with low performance
df_loan_mei_flag %>% 
  filter(umur == 3, flag == 'rendah') %>% 
  inner_join(df_loan, by = 'cabang') %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31') %>% 
  group_by(cabang, agen) %>% 
  summarise(jumlah_hari = n_distinct(tanggal_cair),
            total_loan_cair = n_distinct(loan_id),
            avg_amount = mean(amount), 
            total_amount = sum(amount)) %>% 
  arrange(total_amount) %>% 
  mutate_if(is.numeric, funs(comma))


# Check branch
df_loan %>% 
  filter(cabang == 'AH') %>% 
  filter(tanggal_cair >= '2020-05-01', tanggal_cair <= '2020-05-31') %>% 
  group_by(cabang, agen) %>% 
  summarise(jumlah_hari = n_distinct(tanggal_cair),
            total_loan_cair = n_distinct(loan_id),
            avg_amount = mean(amount), 
            total_amount = sum(amount)) %>% 
  arrange(total_amount) %>% 
  mutate_if(is.numeric, funs(comma))



# Finance part
df_event <- read.csv('https://dqlab-dataset.s3-ap-southeast-1.amazonaws.com/event.csv', stringsAsFactors = F)
glimpse(df_event)


# Date format
df_event$created_at <- ymd_hms(df_event$created_at)
glimpse(df_event)


# Summarize by event 
df_event %>% 
  group_by(nama_event) %>% 
  summarise(jumlah_event = n(),
            loan = n_distinct(loan_id),
            investor = n_distinct(investor_id))


# Investor register
df_marketplace <- df_event %>% 
  filter(nama_event == "loan_to_marketplace") %>% 
  select(loan_id, marketplace = created_at)
head(df_marketplace)
  

# Investor view loan
df_view_loan  <- df_event %>% 
  filter(nama_event == "investor_view_loan") %>% 
  group_by(loan_id, investor_id) %>% 
  summarise(jumlah_view = n_distinct(created_at),
            pertama_view = min(created_at),
            terakhir_view = max(created_at))
head(df_view_loan)


# Investor order and pay loan
df_order_pay <- df_event %>% 
  filter(nama_event %in% c("investor_order_loan", "investor_pay_loan")) %>% 
  spread(key = nama_event, value = created_at) %>%
  select(loan_id,
         investor_id,
         order = investor_order_loan,
         pay = investor_pay_loan)
head(df_order_pay)


# Merge 
df_loan_invest <- df_marketplace %>% 
  left_join(df_view_loan, by = "loan_id") %>%
  left_join(df_order_pay, by = c("loan_id", "investor_id"))

colSums(is.na(df_loan_invest))


# View to order conversion rate
df_loan_invest %>%
  mutate(status_order = ifelse(is.na(order), 'not_order','order')) %>% 
  count(jumlah_view, status_order) %>% 
  spread(key = status_order, value = n, fill = 0) %>%
  mutate(persen_order = scales::percent(order/(not_order + order)))


# view to order required time
df_loan_invest %>%
  filter(!is.na(order)) %>% 
  mutate(lama_order_view = as.numeric(difftime(order, pertama_view, units = "mins"))) %>% 
  group_by(jumlah_view) %>% 
  summarise_at(vars(lama_order_view), funs(total = n(), min, median, mean, max)) %>% 
  mutate_if(is.numeric, funs(round(.,2)))
  
  
# order time since loan uploaded
df_lama_order_per_minggu <- df_loan_invest %>% 
  filter(!is.na(order)) %>%
  mutate(tanggal = floor_date(marketplace, 'weeks'),
         lama_order = as.numeric(difftime(order, marketplace, 'hours'))) %>%
  group_by(tanggal) %>%
  summarise(lama_order = median(lama_order)) 

ggplot(df_lama_order_per_minggu) +
  geom_line(aes(x = tanggal, y = lama_order)) +
  theme_bw() + 
  labs(title = 'Rata-rata lama order pada tahun 2020 lebih lama daripada 2019', 
       x = 'Tanggal', 
       y = 'waktu di marketplce sampai di-pesan (jam)')


# Order to pay conversion rate
df_bayar_per_minggu <- df_loan_invest %>% 
  filter(!is.na(order)) %>%
  mutate(tanggal = floor_date(marketplace, 'weeks')) %>% 
  group_by(tanggal) %>%
  summarise(persen_bayar = mean(!is.na(pay))) 
head(df_bayar_per_minggu)

ggplot(df_bayar_per_minggu) +
  geom_line(aes(x = tanggal, y = persen_bayar)) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() + 
  labs(title = 'Sekitar 95% membayar pesanannya. Di akhir Mei ada outlier karena lebaran',
       x = 'Tanggal', 
       y = 'Pesanan yang dibayar')


# Order to pay conversion time
df_lama_bayar_per_minggu <- df_loan_invest %>% 
  filter(!is.na(pay)) %>%
  mutate(tanggal = floor_date(order, 'week'),
         lama_bayar = as.numeric(difftime(pay, order, units = 'hours'))) %>% 
  group_by(tanggal) %>%
  summarise(lama_bayar = median(lama_bayar)) 
head(df_lama_bayar_per_minggu)

ggplot(df_lama_bayar_per_minggu) +
  geom_line(aes(x = tanggal, y = lama_bayar)) +
  theme_bw() + 
  labs(title = 'Waktu pembayaran trennya cenderung memburuk, 2x lebih lama dari sebelumnya', 
       x = 'Tanggal', 
       y = 'waktu di marketplce sampai di-pesan (jam)')


# New investor register trend
unique(df_event$nama_event)
df_investor_register <- df_event %>%
  filter(nama_event == 'investor_register') %>%
  mutate(tanggal = floor_date(created_at, 'week')) %>% 
  group_by(tanggal) %>%
  summarise(investor = n()) 

ggplot(df_investor_register) +
  geom_line(aes(x = tanggal, y = investor)) +
  theme_bw() + 
  labs(title = 'Investor register sempat naik di awal 2020 namun sudah turun lagi', 
       x = 'Tanggal', 
       y = 'Investor Register')


# First invest time required
df_investor_pertama_invest <- df_event %>%
  filter(nama_event == 'investor_pay_loan') %>%
  group_by(investor_id) %>% 
  summarise(pertama_invest = min(created_at)) %>% 
  mutate(tanggal = floor_date(pertama_invest, 'week')) %>% 
  group_by(tanggal) %>% 
  summarise(investor = n()) 

ggplot(df_investor_pertama_invest) +
  geom_line(aes(x = tanggal, y = investor)) +
  theme_bw() + 
  labs(title = 'Ada tren kenaikan jumlah investor invest, namun turun drastis mulai Maret 2020', 
       x = 'Tanggal', 
       y = 'Investor Pertama Invest')



# Cohort first investment
df_register_per_investor <- df_event %>%
  filter(nama_event == 'investor_register') %>% 
  rename(tanggal_register = created_at) %>%  
  mutate(bulan_register = floor_date(tanggal_register, 'month'))  %>%  
  select(investor_id, tanggal_register, bulan_register) 

df_pertama_invest_per_investor <- df_event %>%
  filter(nama_event == 'investor_pay_loan') %>% 
  group_by(investor_id) %>% 
  summarise(pertama_invest = min(created_at)) 


df_register_per_investor %>% 
  left_join(df_pertama_invest_per_investor, by = 'investor_id') %>% 
  mutate(lama_invest = as.numeric(difftime(pertama_invest, tanggal_register, units = 'day')) %/% 30) %>%  
  group_by(bulan_register, lama_invest) %>% 
  summarise(investor_per_bulan = n()) %>%
  group_by(bulan_register) %>% 
  mutate(register = sum(investor_per_bulan)) %>% 
  filter(!is.na(lama_invest)) %>%  
  mutate(invest = sum(investor_per_bulan)) %>%
  mutate(persen_invest = scales::percent(investor_per_bulan/invest)) %>% 
  mutate(breakdown_persen_invest = scales::percent(invest/register)) %>%
  select(-investor_per_bulan) %>%  
  spread(lama_invest, persen_invest) 


# Cohort retention investment
df_investasi_per_investor <- df_event %>%
  filter(nama_event == 'investor_pay_loan') %>%
  rename(tanggal_invest = created_at) %>% 
  select(investor_id, tanggal_invest)

df_pertama_invest_per_investor %>% 
  mutate(bulan_pertama_invest = floor_date(pertama_invest, 'month'))  %>% 
  inner_join(df_investasi_per_investor, by = 'investor_id') %>%
  mutate(jarak_invest = as.numeric(difftime(tanggal_invest, bulan_pertama_invest, units = 'day')) %/% 30) %>% 
  group_by(bulan_pertama_invest, jarak_invest) %>%
  summarise(investor_per_bulan = n_distinct(investor_id)) %>%
  group_by(bulan_pertama_invest) %>%
  mutate(investor = max(investor_per_bulan)) %>% 
  mutate(breakdown_persen_invest = scales::percent(investor_per_bulan/investor)) %>%
  select(-investor_per_bulan) %>%
  spread(jarak_invest, breakdown_persen_invest) %>% 
  select(-`0`) %>% view()
