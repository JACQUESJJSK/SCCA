# ============================================================================
# O2O网络（机构-机构网络）
# ============================================================================

# 提取ORG实体并限制在总商会15字符范围内
zsh_OrgStrt <- zsh_sbconc_ner_6 %>% 
  filter(Type == "ORG") %>%
  select(-7) %>%
  unique()  # 810342条

# 提取总商会关键词
zsh_zsh <- zsh_OrgStrt %>%
  filter(str_detect(Text, '上海商業會議公所|商業會議公所|上海商務公所|商務公所|上海商務總會|商務總會|上海總商會|總商會'))

zsh_OrgStrt <- bind_rows(zsh_OrgStrt, zsh_zsh) %>% unique()

write_csv(zsh_OrgStrt, "zsh_OrgStrt.csv")

# 提取总商会附近15字符的ORG实体
zsh_data <- zsh_OrgStrt %>%
  filter(Text %in% c("上海商業會議公所", "上海商務公所", "上海商務總會", "上海總商會",
                     "商業會議公所", "商務公所", "商務總會", "總商會"))

org_data <- zsh_OrgStrt[zsh_OrgStrt$Type == "ORG", ]

results <- list()
for (i in 1:nrow(zsh_data)) {
  current_entry <- zsh_data[i, ]
  same_doc_orgs <- org_data[org_data$DocId == current_entry$DocId, ]
  close_orgs <- same_doc_orgs[
    abs(same_doc_orgs$Start - current_entry$End) <= 15 |
    abs(same_doc_orgs$End - current_entry$Start) <= 15, 
  ]
  if (nrow(close_orgs) > 0) {
    results[[length(results) + 1]] <- close_orgs
  }
}

final_results <- do.call(rbind, results) %>% unique() # 得43839条

# 删除频次<5的实体
text_counts_Org2 <- final_results %>%
  group_by(Text) %>%
  summarise(Freq = n()) %>%
  arrange(desc(Freq))

entities_to_keep <- text_counts_Org2 %>%
  filter(Freq >= 5) %>%
  pull(Text)

final_results <- final_results %>% filter(Text %in% entities_to_keep) # 得34229条
write.csv(final_results, "final_results.csv")

# 机构名标准化
final_results_standardized <- final_results %>%
  mutate(Text_Standard = case_when(
    Text %in% c("上海總商會", "總商會") ~ "上海總商會",
    Text %in% c("上海商務總會", "商務總會") ~ "上海商務總會",
    Text %in% c("上海縣敎育會", "縣敎育會") ~ "上海縣敎育會",
    Text %in% c("農商部", "農工商部") ~ "農商部",
    Text %in% c("全國商聯會", "全國商會聯合會", "全國商會聨合會", 
                "全國商會聯合會總事務所", "商聯會") ~ "全國商會聯合會",
    Text %in% c("縣商會", "上海縣商會", "南市縣商會", "南巿縣商會") ~ "上海縣商會",
    Text %in% c("銀行公會", "上海銀行公會") ~ "上海銀行公會",
    Text %in% c("錢業公會", "上海錢業公會", "銀行公會錢業公會") ~ "上海錢業公會",
    Text %in% c("閘北商會", "上寳閘北商會", "上寶閘北商會") ~ "閘北商會",
    Text %in% c("江蘇省敎育會", "江蘇省教育會", "省敎育會") ~ "江蘇省教育會",
    Text %in% c("國民政府", "南京國民政府", "國府") ~ "國民政府",
    Text %in% c("農商部", "北京農商部") ~ "農商部",
    Text %in% c("財政部", "財部") ~ "財政部",
    Text %in% c("外交部", "北京外交部", "外務部") ~ "外交部",
    Text %in% c("工部局", "公共租界工部局", "上海工部局") ~ "工部局",
    Text %in% c("總工會", "上海總工會") ~ "上海總工會",
    Text %in% c("國務院", "北京國務院") ~ "國務院",
    Text %in% c("縣公署", "上海縣公署") ~ "上海縣公署",
    Text %in% c("上海特别市黨部", "市黨部") ~ "市黨部",
    Text %in% c("交涉公署", "江蘇交涉公署", "交涉署") ~ "江蘇交涉公署",
    Text %in% c("中華國貨維持會", "國貨維持會") ~ "中華國貨維持會",
    TRUE ~ Text
  ))

# 统计统一后的名称
Statis_final_results_standardized<- final_results_standardized %>%
  count(Text_Standard, name = "n") %>%
  arrange(desc(n)) # 得549条
write.csv(Statis_final_results_standardized, "Statis_final_results_standardized.csv")


# 生成O2O边和节点
edges_org <- final_results_standardized %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(combinations = list(combn(Text_Standard, 2, simplify = FALSE)),
            .groups = 'drop') %>%
  unnest(combinations) %>%
  mutate(source = map_chr(combinations, 1),
         target = map_chr(combinations, 2)) %>%
  select(source, target, DocId)

edges_weighted_org <- edges_org %>%
  group_by(source, target) %>%
  summarise(weight = n(), doc_ids = list(DocId), .groups = 'drop')

nodes_org <- final_results_standardized %>%
  distinct(Text_Standard) %>%
  rename(id = Text_Standard) %>%
  mutate(degree = map_int(id, ~sum(final_results_standardized$Text_Standard == .x)),
         docs = map(id, ~unique(final_results_standardized$DocId[final_results_standardized$Text_Standard == .x])))

# 过滤权重>40的边
edges_filtered <- edges_weighted_org %>% filter(weight > 40)

nodes_in_edges <- unique(c(edges_filtered$source, edges_filtered$target))
nodes_filtered <- nodes_org %>% filter(id %in% nodes_in_edges)


# 找出哪些列是列表类型
list_columns <- sapply(nodes_filtered, is.list)

# 只导出非列表列
write.csv(nodes_filtered[, !list_columns], 
          "nodes_filtered_no_lists.csv", 
          row.names = FALSE)

# 导入处理后的表格nodes_filtered_Ed.csv
nodes_filtered_Ed <- read_csv("nodes_filtered_Ed.csv")


# 基于处理后的节点，从原始边数据重新过滤
edges_consistent <- edges_weighted_org %>%
  filter(weight > 40) %>%
  filter(source %in% nodes_filtered_Ed$id & 
           target %in% nodes_filtered_Ed$id)

# 确保没有孤立节点
nodes_in_edges <- unique(c(edges_consistent$source, edges_consistent$target))
nodes_final <- nodes_filtered_Ed %>% filter(id %in% nodes_in_edges)

# 构建图
# 重新构建图并确保计算度中心性
g_tidy <- tbl_graph(
  nodes = nodes_final,
  edges = edges_consistent %>% rename(from = source, to = target),
  directed = FALSE
) %>%
  activate(nodes) %>%
  mutate(
    degree = centrality_degree(),  # 计算度中心性
    betweenness = centrality_betweenness(),
    closeness = centrality_closeness()
  ) %>%
  activate(edges) %>%
  mutate(weight = weight)  # 确保权重列存在



# 创建静态图
p_static_enhanced <- ggraph(g_tidy, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.3, color = 'gray60') +
  geom_node_point(aes(size = degree, color = Attribute_En), alpha = 0.8) +
  geom_node_text(aes(label = id_Pinyin), repel = TRUE, size = 4, max.overlaps = 30) +  # 增大到4
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(3, 15)) +  # 增大节点大小范围
  scale_color_brewer(palette = "Set1") +
  theme_graph(base_size = 12) +  # 增加基础字体大小
  labs(
    size = 'Degree Centrality',
    color = 'Institution Type',
    # edge_width = 'Co-occurrence Frequency'
  ) +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)  # 增加边距
  )

print(p_static_enhanced)



# 保存高清大图
ggsave("simplified_network.png", p_static_enhanced, 
       width = 14,    # 增加宽度
       height = 10,   # 增加高度
       dpi = 300,     # 合适的DPI
       bg = "white")  # 白色背景


# ===============================
# k-core 圈层结构 + 组织属性（Attribute）
# 论文主图版本
# 成功
# ===============================

# -------------------------------
# 1. tidygraph → igraph
# -------------------------------
g_igraph <- as.igraph(g_tidy)

# -------------------------------
# 2. k-core coreness（非加权）
# -------------------------------
core <- igraph::coreness(g_igraph)

# -------------------------------
# 3. 回写 coreness，并划分圈层
# -------------------------------
g_tidy_kcore <- g_tidy %>%
  activate(nodes) %>%
  mutate(
    coreness  = as.numeric(core),
    node_size = coreness,
    layer = case_when(
      coreness >= quantile(coreness, 0.8, na.rm = TRUE) ~ "Core Layer",
      coreness >= quantile(coreness, 0.5, na.rm = TRUE) ~ "Middle Layer",
      TRUE ~ "Peripheral Layer"
    )
  )

# -------------------------------
# 4. 绘图
# -------------------------------
p_kcore <- ggraph(g_tidy_kcore, layout = "fr") +
  
  # 圈层凸包（仍然按 layer）
  geom_mark_hull(
    aes(x, y, fill = layer),
    concavity = 4,
    expand = unit(4, "mm"),
    alpha = 0.12,
    show.legend = FALSE
  ) +
  
  # 边
  geom_edge_link(
    aes(width = weight),
    color = "gray65",
    alpha = 0.4,
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.3, 2.2), guide = "none") +
  
  # 节点：颜色 = Attribute，形状 = layer
  geom_node_point(
    aes(
      size  = degree,
      color = `Attribute_En`,
      shape = layer
    ),
    alpha = 0.9,
    stroke = 1.1
  )+
  scale_size_continuous(range = c(3, 14), guide = "none") +
  
  # 标签
  geom_node_text(
    aes(label = id_Pinyin),
    repel = TRUE,
    size = 2.6,
    alpha = 0.9
  ) +
  
  # 组织属性颜色（你可按史学含义微调）
  scale_color_brewer(
    name = "Organization Type",
    palette = "Set1"
  ) +
  
  # 圈层填充
  scale_fill_manual(
    values = c(
      "Core Layer" = alpha("red", 0.15),
      "Middle Layer" = alpha("orange", 0.15),
      "Peripheral Layer" = alpha("gray80", 0.15)
    )
  ) +
  
  # 圈层形状
  scale_shape_manual(
    name = "Network Layer",
    values = c(
      "Core Layer" = 19,      # 实心圆
      "Middle Layer" = 17,    # 三角
      "Peripheral Layer" = 1  # 空心圆
    )
  ) +
  
  theme_graph() +
  labs(
    title = "The Organization–Organization Network Structure",
    subtitle = "Layered Configuration Based on k-core Decomposition"
  ) +
  
  guides(
    fill  = "none",
    size  = "none",
    color = guide_legend(order = 1),
    shape = guide_legend(order = 2)
  )

print(p_kcore)



# ==============================================================================
# ===============================
# 1. 去除“上海总商会”
# ===============================
final_results_no_center <- final_results_standardized %>%
  filter(Text_Standard != "上海總商會")

# ===============================
# 2. 构建共现边
# ===============================
edges_weighted_no_center <- final_results_no_center %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(
    pairs = list(combn(Text_Standard, 2, simplify = FALSE)),
    .groups = "drop"
  ) %>%
  unnest(pairs) %>%
  mutate(
    source = map_chr(pairs, 1),
    target = map_chr(pairs, 2)
  ) %>%
  group_by(source, target) %>%
  summarise(
    weight = n(),
    doc_ids = list(DocId),
    .groups = "drop"
  )

# ===============================
# 3. 初始节点表（仅用于导出人工清洗）
# ===============================
nodes_filtered_no_center <- final_results_no_center %>%
  group_by(Text_Standard) %>%
  summarise(
    n_docs = n_distinct(DocId),
    .groups = "drop"
  ) %>%
  rename(id = Text_Standard)

# 只导出非 list 列
list_columns <- sapply(nodes_filtered_no_center, is.list)

write.csv(
  nodes_filtered_no_center[, !list_columns],
  "nodes_filtered_no_center_list.csv",
  row.names = FALSE
)

# ===============================
# 4. 读回人工清洗后的节点表（核心节点集）
# ===============================
nodes_filtered_no_center_Ed <- read_csv("nodes_filtered_no_center_Ed.csv")

# ===============================
# 5. 边表过滤（与节点表对齐 + 权重阈值）
# ===============================
edges_filtered_no_center <- edges_weighted_no_center %>%
  filter(
    source %in% nodes_filtered_no_center_Ed$id,
    target %in% nodes_filtered_no_center_Ed$id,
    weight > 8
  )

# 再保险：去掉孤立节点
nodes_in_edges <- unique(c(
  edges_filtered_no_center$source,
  edges_filtered_no_center$target
))

nodes_final_no_center <- nodes_filtered_no_center_Ed %>%
  filter(id %in% nodes_in_edges)

# ===============================
# 6. 输出最终网络数据（供复现 / 存档）
# ===============================
write_csv(edges_filtered_no_center, "edges_final_no_center.csv")
write_csv(nodes_final_no_center,    "nodes_final_no_center.csv")

# ===============================
# 7. 构建网络 + 中心性
# ===============================
g_no_center <- tbl_graph(
  nodes = nodes_final_no_center,
  edges = edges_filtered_no_center,
  directed = FALSE
) %>%
  activate(nodes) %>%
  mutate(
    degree_network = centrality_degree(),
    betweenness    = centrality_betweenness(),
    closeness      = centrality_closeness()
  )

# ===============================
# 8. 可视化
# ===============================
p_no_center <- ggraph(g_no_center, layout = "fr",layout = "stress") +
  geom_edge_link(
    aes(width = weight),
    color = "gray60",
    alpha = 0.3,
    show.legend = FALSE
  ) +
  geom_node_point(
    aes(
      size  = degree_network,   # 仍用于视觉
      color = betweenness
    ),
    alpha = 0.8
  ) +
  geom_node_text(
    aes(label = id_Pinyin),     # ← 改这里：统一使用 id_Pinyin
    repel = TRUE,
    size = 3,
    max.overlaps = 20
  ) +
  scale_edge_width(range = c(0.5, 3)) +
  
  # ⚠️ 关键：size 图例关闭
  scale_size_continuous(
    range = c(2, 12),
    guide = "none"
  ) +
  
  scale_color_gradient(
    low = "lightblue",
    high = "darkred"
  ) +
  
  theme_graph() +
  labs(
    title = 'The Organizational Network after De-centering',
    subtitle = 'Edge weight > 8',
    color = 'Betweenness'
  ) +
  
  guides(
    size = "none"   # 再加一道保险，确保 degree_network 不进图例
  )

print(p_no_center)
