# Exercise 3.1 Wrangling Data

# 3.1.0 Loading dataset into R
df = read.xls(("refine.xlsx"), sheet = 1, header = TRUE)

# 3.1.1 Fixing company names
df2$company <- sub("^P.*$|^p.*$|^f.*$", "philips", df2$company)
df2$company <- sub("^A.*|^a.*", "akzo", df2$company)
df2$company <- sub("^V.*|^v.*", "van houten", df2$company)
df2$company <- sub("^U.*|^u.*", "unilever", df2$company)

df2 <- df

# 3.1.2 Separating product code and product number
df_tib <- separate(df_tib, Product.code...number., into = c("product_code", "product_number"))

# 3.1.3 Create new column for product_category

df_tib <- df_tib %>% 
  mutate(product_category = ifelse(product_code == "p", "Smartphone", 
                              ifelse(product_code == "x", "Laptop", 
                                ifelse(product_code == "v", "TV", 
                                  ifelse(product_code == "q", "Tablet", product_code)))))

# 3.1.4 Add full address for geocoding

df_tib <- unite(df_tib2, address, city, country, col = "full_address", sep = ",")

# 3.1.5 Create dummy variables for company and product category

df_tib <- df_tib %>% mutate(company_philips = ifelse(company == "philips", 1, 0))
df_tib <- df_tib %>% mutate(company_akzo = ifelse(company == "akzo", 1, 0))
df_tib <- df_tib %>% mutate(company_van_houten = ifelse(company == "van houten", 1, 0))
df_tib <- df_tib %>% mutate(company_unilever = ifelse(company == "unilever", 1, 0))
df_tib <- mutate(df_tib, company_philips = as.factor(company_philips))
df_tib <- mutate(df_tib, company_akzo = as.factor(company_akzo))
df_tib <- mutate(df_tib, company_van_houten = as.factor(company_van_houten))
df_tib <- mutate(df_tib, company_unilever = as.factor(company_unilever))

df_tib <- df_tib %>% mutate(product_smartphone = ifelse(product_category == "Smartphone", 1, 0))
df_tib <- df_tib %>% mutate(company_tv = ifelse(product_category == "TV", 1, 0))
df_tib <- df_tib %>% mutate(company_laptop = ifelse(product_category == "Laptop", 1, 0))
df_tib <- df_tib %>% mutate(company_tablet = ifelse(product_category == "Tablet", 1, 0))
df_tib <- mutate(df_tib, product_smartphone = as.factor(product_smartphone))
df_tib <- mutate(df_tib, company_tv = as.factor(company_tv))
df_tib <- mutate(df_tib, company_laptop = as.factor(company_laptop))
df_tib <- mutate(df_tib, company_tablet = as.factor(company_tablet))

