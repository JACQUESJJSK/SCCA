# 上海商业精英论文 - 申报总商会系统网络分析（精简版）
# 基于申报对上海总商会系统的网络分析
# 2026年1月2日 

# ============================================================================
# 环境设置
# ============================================================================

# 加载必要的包
library(histtext)
library(lubridate)
library(dplyr)
library(stringi)
library(readr)
library(igraph)
library(ggplot2)
library(ggraph)
library(purrr)
library(tidyr)
library(stringr)
library(showtext)
library(tidygraph)
library(ggraph)
library(ggforce)
library(concaveman)

# 设定工作文件夹
setwd("~/Downloads/R_documents/SCE/精简版") # 根据电脑不同，酌情修改文件夹位置

# ============================================================================
# 一、数据检索与基础处理
# ============================================================================

# 检索包含总商会系统的报道
zsh_chamber_all <- search_documents(
  '"上海商業會議公所"|"商業會議公所"|"上海商務公所"|"商務公所"|"上海商務總會"|"商務總會"|"上海總商會"|"總商會"',
  "shunpao-revised"
) # 得30432条

zsh_chamber_all <- unique(zsh_chamber_all)  # 去重，得30432条

# 获取报道全文
zsh_chamber_ft <- get_documents(zsh_chamber_all, "shunpao-revised") # 得30432篇

# 导出数据
write_csv(zsh_chamber_all, "zsh_chamber_all.csv")
write_csv(zsh_chamber_ft, "zsh_chamber_ft.csv")

# 生成文章长度列
zsh_chamber_ft <- zsh_chamber_ft %>% mutate(Length = nchar(Text))

# 文章长度分类统计图
zsh_chamber_ft %>%
  mutate(Length_Category = cut(Length, 
                               breaks = c(0, 500, 2000, 5000, Inf),
                               labels = c("Short(<500)", "Medium(500-2000)", 
                                          "Long(2000-5000)", "Very Long(>5000)"))) %>%
  count(Length_Category) %>%
  ggplot(aes(x = Length_Category, y = n, fill = Length_Category)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  theme_minimal() +
  labs(x = "Article Type", y = "Count", title = "Distribution of Article Length Categories")

# 提取年代分布
zsh_chamber_temp <- zsh_chamber_ft %>%
  mutate(Year = as.integer(str_sub(Date, 1, 4))) %>%
  mutate(N = 1) %>%
  group_by(Year) %>%
  summarise(N = sum(N))

# 年代分布柱状图（1902-1929）
ggplot(zsh_chamber_temp, aes(x = Year, y = N)) +
  geom_col(fill = "blue") +
  labs(title = "ZSH in Shenbao", subtitle = "Number of mentions",
       x = "Year", y = "Number of articles") +
  xlim(1902, 1929) +
  theme(panel.background = element_rect(fill = "lightgrey"))

# ============================================================================
# 二、共现词检索与清洗
# ============================================================================

# 搜索总商会系统的共现词表（context_size = 100）
zsh_sbconc <- search_concordance(
  '"上海商業會議公所"|"商業會議公所"|"上海商務公所"|"商務公所"|"上海商務總會"|"商務總會"|"上海總商會"|"總商會"',
  corpus = "shunpao-revised", 
  context_size = 100
) # 得56231条

# 合并concordance列
zsh_sbconc <- zsh_sbconc %>% 
  mutate(Text = paste(Before, Matched, After)) %>%
  relocate(Text, .before = "Source")

write.csv(zsh_sbconc, "zsh_sbconc.csv") 

# 提取1902-1929年数据
# 先生成Year列，再进行筛选
zsh_sbconc_use <- zsh_sbconc %>% 
  mutate(Year = as.numeric(str_sub(DocId, 5, 8))) %>%
  filter(between(Year, 1902, 1929)) # 53982条

# 数据清洗：删除其他地区总商会
zsh_names <- c("上海商業會議公所", "上海商務公所", "上海商務總會", "上海總商會")

# 第一步：提取Matched为总商会名称的行
zsh_sbconc_1 <- zsh_sbconc_use %>% filter(Matched %in% zsh_names)

# 第二步：提取Before列最右侧两个字符并过滤
zsh_sbconc_temp <- zsh_sbconc_use %>%
  filter(!Matched %in% zsh_names) %>%
  mutate(Before_temp = str_sub(Before, -2, -1))

# 提取Before列最右侧两个字符为空的所有行
# 等同于总商会系统
zsh_sbconc_2 <- zsh_sbconc_temp %>% 
  filter(Before_temp == "" | is.na(Before_temp))

# 提取Before列最右侧两个字符不为空的所有行
# 用于继续清洗
zsh_sbconc_temp_2 <- zsh_sbconc_temp %>% 
  filter(Before_temp != "" & !is.na(Before_temp))  # 40910条

# 统计Before_temp
Statis_before <- zsh_sbconc_temp_2 %>%
  count(Before_temp, name = "n") %>%
  arrange(desc(n))

# 删除省市简称
search_pattern <- "蘇|杭|寧|甯|京|平|粵|漢|廈"
Statis_before_1 <- Statis_before %>%
  filter(str_detect(Before_temp, search_pattern))

zsh_sbconc_temp_3 <- zsh_sbconc_temp_2 %>%
  anti_join(Statis_before_1, by = "Before_temp")  # 35343条

# 删除历史省份
historical_province_pattern <- "北京|天津|河北|山西|蒙古|遼寧|吉林|龍江|上海|江蘇|浙江|安徽|福建|江西|山東|河南|湖北|湖南|廣東|廣西|海南|重慶|四川|貴州|雲南|西藏|陝西|甘肅|青海|寧夏|新疆|香港|澳門|臺灣|熱河|察哈爾|綏遠|西康"

Statis_before_2 <- zsh_sbconc_temp_3 %>%
  count(Before_temp, name = "n") %>%
  arrange(desc(n))

Statis_before_3 <- Statis_before_2 %>%
  filter(str_detect(Before_temp, historical_province_pattern))

zsh_sbconc_temp_4 <- zsh_sbconc_temp_3 %>%
  anti_join(Statis_before_3, by = "Before_temp")  # 34355条

# 删除其他地区标识
search_pattern_2 <- "中華|廣|津|蕪|寗|全國|江寗|省"
Statis_before_4 <- zsh_sbconc_temp_4 %>%
  count(Before_temp, name = "n") %>%
  arrange(desc(n))

Statis_before_5 <- Statis_before_4 %>%
  filter(str_detect(Before_temp, search_pattern_2))

zsh_sbconc_temp_5 <- zsh_sbconc_temp_4 %>%
  anti_join(Statis_before_5, by = "Before_temp")  # 29888条

write.csv(zsh_sbconc_temp_5, "zsh_sbconc_temp_5.csv")

# 导入人工清洗后的数据
zsh_sbconc_temp_5_Ed <- read_csv("zsh_sbconc_temp_5_Ed.csv")  


# 合并清洗后的数据
zsh_sbconc_all <- bind_rows(
  zsh_sbconc_1 %>% mutate(Date = as.Date(Date)),
  zsh_sbconc_2 %>% select(-Before_temp) %>% mutate(Date = as.Date(Date)),
  zsh_sbconc_temp_5_Ed %>% select(-Before_temp) %>% mutate(Date = as.Date(Date))
)  

write.csv(zsh_sbconc_all, "zsh_sbconc_all.csv")


# ============================================================================
# 三、命名实体识别（NER）
# ============================================================================

# NER处理
zsh_sbconc_ner_use <- histtext::ner_on_corpus(
  zsh_sbconc_use, 
  corpus = "shunpao-revised", 
  only_precomputed = TRUE
)  # 13775722条

write_csv(zsh_sbconc_ner_use, "zsh_sbconc_ner_use.csv")


# 提取实体对应年代
zsh_sbconc_ner_use <- zsh_sbconc_ner_use  %>% 
  mutate(Year = as.numeric(str_sub(DocId, 5, 8))) # 经检测，均为1902-1929之间数据

# 提取PERSON、ORG和EVENT实体
zsh_sbconc_ner_2 <- zsh_sbconc_ner_use %>%
  filter(str_detect(Type, "PERSON|ORG|EVENT"))  # 5072365条

# 删除非汉字、空格，清理数据
zsh_sbconc_ner_2 <- zsh_sbconc_ner_2 %>%
  mutate(Text = str_replace_all(Text, "[^[\\p{Han}]]", " ")) %>%
  mutate(Text = str_squish(Text)) %>%
  filter(Text != "") %>%
  filter(nchar(Text) > 1) %>%
  filter(Confidence >= 0.6)  # 4383601条

write_csv(zsh_sbconc_ner_2, "zsh_sbconc_ner_2.csv")

# 处理Type冲突
conflicting_rows <- zsh_sbconc_ner_2 %>%
  group_by(DocId, Text) %>%
  filter(n_distinct(Type) > 1) %>%
  arrange(DocId, Text, Type) %>%
  ungroup()  # 16994条

write.csv(conflicting_rows, "conflicting_rows.csv")

# 删除冲突行，导入人工清洗后的数据
zsh_sbconc_ner_3 <- zsh_sbconc_ner_2 %>%
  anti_join(conflicting_rows, by = c("DocId", "Text"))  # 4286923条

conflicting_rows_Ed <- read_csv("conflicting_rows_Ed.csv")

zsh_sbconc_ner_4 <- bind_rows(zsh_sbconc_ner_3, conflicting_rows_Ed) %>%
  distinct()  # 1848246条

write.csv(zsh_sbconc_ner_4, "zsh_sbconc_ner_4.csv")

# 继续清洗：删除空数据、敬语
zsh_sbconc_ner_5 <- zsh_sbconc_ner_4 %>%
  filter(!is.na(Text)) %>%
  filter(Text != "") %>%
  filter(!Text %in% c("云云", "鈞鑒"))

# 分离合并的实体
zsh_sbconc_ner_6 <- zsh_sbconc_ner_5 %>%
  separate_rows(Text, sep = "\\s+") %>%
  filter(Text != "") %>%
  mutate(Length = nchar(Text)) %>%
  filter(Length > 1)  # 1852050条

write_csv(zsh_sbconc_ner_6, "zsh_sbconc_ner_6.csv")


# 统计频次分布
text_counts_entity <- zsh_sbconc_ner_6 %>%
  group_by(Text) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq)) %>%
  ungroup()

# 频次分布图
freq_distribution <- text_counts_entity %>%
  mutate(freq_range = case_when(
    Freq >= 1 & Freq <= 4 ~ "1-4",
    Freq == 5 ~ "5",
    Freq >= 6 & Freq <= 10 ~ "6-10",
    Freq >= 11 & Freq <= 20 ~ "11-20",
    Freq >= 21 & Freq <= 50 ~ "21-50",
    Freq >= 51 & Freq <= 100 ~ "51-100",
    Freq >= 101 & Freq <= 500 ~ "101-500",
    Freq > 500 ~ "500+",
    TRUE ~ as.character(Freq)
  )) %>%
  group_by(freq_range) %>%
  summarise(entity_count = n()) %>%
  ungroup()

freq_distribution$freq_range <- factor(
  freq_distribution$freq_range,
  levels = c("1-4", "5", "6-10", "11-20", "21-50", "51-100", "101-500", "500+")
)

# 出图
ggplot(freq_distribution, aes(x = freq_range, y = entity_count)) +
  geom_bar(stat = "identity", fill = "#667eea", alpha = 0.8) +
  geom_text(aes(label = entity_count), vjust = -0.5, size = 4) +
  labs(
    # title = "Organization Frequency Distribution - Long Tail Effect",
    # subtitle = "Following Long Tail Theory: Few high-frequency entities, many low-frequency entities",
    x = "Frequency Range", 
    y = "Entity Count"
  ) +
  theme_minimal(base_size = 14)
