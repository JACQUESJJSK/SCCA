# ============================================================================
# 六、E2E网络（事件-事件网络）
# ============================================================================

# 提取EVENT实体
zsh_EveStrt <- zsh_sbconc_ner_6 %>% 
  filter(Type == "EVENT") %>%
  select(-7) %>%
  unique()  # 40761条

write_csv(zsh_EveStrt, "zsh_EveStrt.csv")

event_data <- zsh_EveStrt[zsh_EveStrt$Type == "EVENT", ] %>% unique()

# 事件标准化
final_results_event_1 <- event_data %>%
  mutate(Text_Standard = case_when(
    str_detect(Text, "五卅") ~ "五卅运动",
    str_detect(Text, "第三次全國代表大會|三全大會") ~ "第三次全國代表大會",
    str_detect(Text, "濟南慘案|濟案|五三慘案") ~ "濟南慘案",
    str_detect(Text, "南京路慘案|上海慘案|滬案|滬慘案|上海慘殺案") ~ "五卅运动",
    str_detect(Text, "五四") ~ "五四运动",
    str_detect(Text, "六三運動|六三") ~ "五四运动",
    str_detect(Text, "江浙戰事|江浙戰|江浙戰爭|江浙之戰|東南戰事|浙戰|第二次江浙戰|東南戰") ~ "江浙戰爭",
    str_detect(Text, "國民革命|北伐勝利大會|北伐勝利|北伐將士大會|中國國民革命|北伐") ~ "北伐",
    str_detect(Text, "遠東運動會|遠東運動大會|萬國運動會") ~ "遠東運動會",
    str_detect(Text, "中華國貨展覽會|國貨運動大會|國貨展覽會") ~ "國貨運動",
    str_detect(Text, "歐洲大戰|世界大戰|歐戰|第一次世界大戰") ~ "第一次世界大戰",
    str_detect(Text, "華府會議|華盛頓會議|太平洋會議|平洋會議|華會") ~ "華盛頓會議",
    str_detect(Text, "庚申赭亂|申赭亂") ~ "庚申赭乱",
    str_detect(Text, "臨城刦案|臨城案|臨城事件|臨城刼案|臨城匪案") ~ "臨城大刼案",
    str_detect(Text, "九六|九六公債") ~ "九六公債",
    str_detect(Text, "漢案|漢口慘案") ~ "漢口慘案",
    str_detect(Text, "國恥紀念會|五九國恥|國恥|五九") ~ "國恥紀念會",
    str_detect(Text, "中華民國八團體國是會議|上海國是會議") ~ "上海國是會議",
    TRUE ~ Text
  )) %>%
  add_count(Text_Standard, name = "freq") %>%
  arrange(desc(freq), Text_Standard)

write.csv(final_results_event_1, "final_results_event_1.csv")

# 生成E2E边和节点
edges_event <- final_results_event_1 %>%
  group_by(DocId) %>%
  distinct(Text_Standard) %>%
  filter(n() >= 2) %>%
  summarise(combinations = list(combn(Text_Standard, 2, simplify = FALSE)),
            .groups = 'drop') %>%
  unnest(combinations) %>%
  mutate(source = map_chr(combinations, 1),
         target = map_chr(combinations, 2)) %>%
  select(source, target, DocId)

edges_weighted_event <- edges_event %>%
  group_by(source, target) %>%
  summarise(weight = n(), doc_ids = list(DocId), .groups = 'drop')

nodes_event <- final_results_event_1 %>%
  distinct(Text_Standard) %>%
  rename(id = Text_Standard) %>%
  mutate(degree = map_int(id, ~sum(final_results_event_1$Text_Standard == .x)),
         docs = map(id, ~unique(final_results_event_1$DocId[final_results_event_1$Text_Standard == .x])))

# 权重阈值分析
weight_threshold <- 10
edges_filtered_event <- edges_weighted_event %>% filter(weight > weight_threshold)

nodes_in_edges_event <- unique(c(edges_filtered_event$source, 
                                  edges_filtered_event$target))
nodes_filtered_event <- nodes_event %>% filter(id %in% nodes_in_edges_event)


# 只导出非 list 列
list_columns <- sapply(nodes_filtered_event, is.list)

write.csv(
  nodes_filtered_event[, !list_columns],
  "nodes_filtered_event.csv",
  row.names = FALSE
)

# ===============================
# 读回人工清洗后的节点表（核心节点集）
# ===============================
nodes_filtered_event_Ed<- read_csv("nodes_filtered_event_Ed.csv")

# 边数据对齐
edges_filtered_event <- edges_weighted_event %>%
  filter(
    source %in% nodes_filtered_event_Ed$id,
    target %in% nodes_filtered_event_Ed$id,
    weight > weight_threshold
  )

nodes_in_edges_event <- unique(c(
  edges_filtered_event$source,
  edges_filtered_event$target
))

nodes_final_event <- nodes_filtered_event_Ed %>%
  filter(id %in% nodes_in_edges_event)



# ==========================================================================



# 创建E2E网络
g_event <- tbl_graph(nodes = nodes_filtered_event_Ed,
                     edges = edges_filtered_event,
                     directed = FALSE) %>%
  activate(nodes) %>%
  mutate(degree_network = centrality_degree(),
         betweenness = centrality_betweenness(),
         closeness = centrality_closeness())

# 可视化E2E网络
set.seed(123)

p_event_basic <- ggraph(g_event, layout = 'kk') +
  geom_edge_link(aes(width = weight), alpha = 0.3, color = 'gray60') +
  geom_node_point(aes(size = degree_network), color = 'forestgreen', alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  theme_graph() +
  labs(title = '事件共现网络 (E2E Network)',
       subtitle = paste('Weight >', weight_threshold),
       size = 'Network Degree')

print(p_event_basic)
ggsave("E2E_basic.png", p_event_basic, width = 12, height = 8, dpi = 500)

# 介数中心性分析
p_event_betweenness <- ggraph(g_event, layout = 'fr') +
  geom_edge_link(aes(width = weight), alpha = 0.2, color = 'gray70') +
  geom_node_point(aes(size = degree_network, color = betweenness), alpha = 0.8) +
  geom_node_text(aes(label = id), repel = TRUE, size = 3, max.overlaps = 20) +
  scale_edge_width(range = c(0.5, 3)) +
  scale_size_continuous(range = c(2, 12)) +
  scale_color_gradient(low = 'lightgreen', high = 'darkred', name = 'Betweenness\n(桥梁作用)') +
  theme_graph() +
  labs(title = '事件网络 - Betweenness分析',
       subtitle = '红色 = 连接不同事件群的关键事件',
       size = 'Degree')

print(p_event_betweenness)
ggsave("E2E_betweenness.png", p_event_betweenness, width = 12, height = 8, dpi = 500)

library(dplyr)
library(tidygraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(ggforce)

# ===============================
# 1. Leiden 社区检测
# ===============================
communities_event <- cluster_leiden(
  as.igraph(g_event),
  resolution_parameter = 0.5
)

# ===============================
# 2. k-core
# ===============================
core_vals <- coreness(as.igraph(g_event))

# ===============================
# 3. 写回 tidygraph
# ===============================
g_event_community <- g_event %>%
  activate(nodes) %>%
  mutate(
    degree_network = centrality_degree(),
    community = as.factor(membership(communities_event)),
    coreness  = core_vals,
    k_layer = case_when(
      coreness >= quantile(coreness, 0.75, na.rm = TRUE) ~ "Core",
      coreness >= quantile(coreness, 0.40, na.rm = TRUE) ~ "Middle",
      TRUE                                              ~ "Peripheral"
    )
  )

# ===============================
# 4. 可视化：Leiden × k-core（双凸包）
# ===============================
p_event_community <- ggraph(g_event_community, layout = "stress") +
  
  # —— Leiden 社区凸包（叙事结构）——
  geom_mark_hull(
    aes(x, y, fill = community, group = community),
    concavity = 6,
    expand = unit(4, "mm"),
    alpha = 0.20,
    color = NA,
    show.legend = TRUE
  ) +
  
  # —— k-core 圈层凸包（结构层级）——
  geom_mark_hull(
    aes(x, y, group = k_layer),
    concavity = 8,
    expand = unit(6, "mm"),
    fill = NA,
    color = "black",
    linetype = "dashed",
    linewidth = 0.6,
    show.legend = FALSE
  ) +
  
  # —— 边（不显示 weight 图例）——
  geom_edge_link(
    aes(width = weight),
    alpha = 0.25,
    color = "gray65",
    show.legend = FALSE
  ) +
  scale_edge_width(range = c(0.4, 2.8)) +
  
  # —— 节点：大小 = degree，颜色 = 社区，形状 = k-core —— 
  geom_node_point(
    aes(
      size  = degree_network,
      color = community,
      shape = k_layer
    ),
    alpha = 0.95
  ) +
  
  # —— 关闭 size 图例（degree 不显示）——
  scale_size_continuous(range = c(2.5, 11), guide = "none") +
  
  # —— 标签 —— 
  geom_node_text(
    aes(label = id_Pinyin),
    repel = TRUE,
    size = 2.6,
    max.overlaps = 15
  ) +
  
  # —— 图例：只保留社区 & k-core —— 
  scale_color_brewer(
    palette = "Set2",
    name = "Leiden Community"
  ) +
  scale_fill_brewer(
    palette = "Set2",
    name = "Leiden Community"
  ) +
  scale_shape_manual(
    values = c(
      "Core"       = 19,
      "Middle"     = 17,
      "Peripheral" = 1
    ),
    name = "k-core Layer"
  ) +
  
  guides(
    size = "none",
    edge_width = "none"
  ) +
  
  theme_graph(base_family = "serif") +
  labs(
    # title = "Event Network Structure in Shenbao",
    # subtitle = "Narrative communities (Leiden) and structural layers (k-core)",
    caption = "Colored hulls indicate narrative communities; dashed hulls indicate k-core structural layers"
  )

print(p_event_community)


ggsave("E2E_community.png", p_event_community, width = 12, height = 8, dpi = 300)

# 导出E2E结果
write.csv(edges_filtered_event, 'edges_event_filtered.csv', row.names = FALSE)
write.csv(g_event %>% activate(nodes) %>% as_tibble(), 
          'nodes_event_filtered.csv', row.names = FALSE)
write.csv(g_event_community %>% activate(nodes) %>% as_tibble() %>%
            select(id, degree, degree_network, betweenness, community) %>%
            arrange(community, desc(degree_network)),
          'event_community_info.csv', row.names = FALSE)
