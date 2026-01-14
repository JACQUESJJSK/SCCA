# Research Design and Methodology_2

**A Historical Network Analysis of the Shanghai General Chamber of Commerce Based on *Shenbao* (1902–1929)**

---

## 1. Research Rationale and Overall Design

This study reconstructs the structural position of the Shanghai General Chamber of Commerce (SGCC) within the public discourse and institutional landscape of modern China through a systematic analysis of newspaper texts. Rather than treating the Chamber merely as a formal institution or narrating its development through selected case studies, this article conceptualizes the SGCC as a **relational node embedded in multiple layers of historical networks**.

Specifically, the study seeks to answer the following questions:

1. Which institutions, individuals, and events were most frequently associated with the Shanghai General Chamber of Commerce in the public discourse of the early twentieth century?
2. Do these associations exhibit stable structural hierarchies and core–periphery patterns?
3. Does the surrounding network retain structural coherence when the SGCC is removed as a central node?

To address these questions, the article develops a multi-step analytical framework that transforms historical texts into networked relational structures through entity extraction and co-occurrence modeling.

---

## 2. Sources and Corpus Construction

The primary source for this study is the full-text corpus of *Shenbao* from 1902 to 1929. This period encompasses the institutional transformation of Shanghai’s commercial organizations—from early commercial councils to the formal establishment of the General Chamber of Commerce—and coincides with major political and social upheavals in modern China.

Text retrieval was conducted using a set of historically inclusive keywords reflecting institutional name changes over time, including:

> “Shanghai Commercial Council,” “Shanghai Chamber of Commerce,” “Shanghai General Chamber of Commerce,” and their abbreviated or variant forms.

After deduplication, approximately 30,000 articles related to the Shanghai chamber system were identified, forming the base corpus for analysis.

---

## 3. Contextual Co-occurrence Windows and Discursive Delimitation

To avoid situating the SGCC within an excessively broad textual environment, the study applies **localized contextual windows**. For each occurrence of a chamber-related keyword, a window of ±100 Chinese characters was extracted as the basic analytical unit.

This procedure reflects a historical assumption rather than a purely technical choice:
**Actors co-occurring within a limited textual context are more likely to belong to the same historical event, institutional interaction, or discursive moment.**

Further filtering rules and manual verification were applied to exclude references to chambers in other regions, administrative place names, and generic institutional terms. The resulting corpus thus represents a **discursively coherent field centered on the Shanghai General Chamber of Commerce**.

---

## 4. Named Entity Recognition and Historical Normalization

Named Entity Recognition (NER) was then applied to the contextualized corpus, focusing on three categories of historical actors:

* **Organizations (ORG)**
* **Persons (PERSON)**
* **Events (EVENT)**

To ensure interpretability for historical analysis, the extracted entities underwent a three-stage cleaning process:

1. Removal of low-confidence entities based on recognition scores;
2. Elimination of non-semantic noise such as honorifics and formatting artifacts;
3. Manual normalization of variant names, abbreviations, and period-specific expressions.

Through these procedures, narrative texts were transformed into a structured set of historically identifiable actors.

---

## 5. Co-occurrence Modeling and Network Construction

Relationships between entities were modeled using a combined criterion of **document-level co-occurrence and character-distance constraint**. Two entities were considered connected if they appeared within the same article and within a distance of no more than 15 characters.

Based on this rule, three types of networks were constructed:

1. **Organization–Organization (O2O) networks**, capturing institutional embeddedness;
2. **Person–Person (P2P) networks**, identifying elite structures and brokerage roles;
3. **Event–Event (E2E) networks**, revealing the narrative and historical contexts in which the SGCC appeared.

Edge weights were defined by co-occurrence frequency, reflecting the stability and intensity of relationships.

---

## 6. Network Analysis Methods

The study applies a range of structural network metrics, including:

* **Degree centrality**, measuring the intensity of connections;
* **Betweenness centrality**, identifying brokerage and bridging nodes;
* **k-core decomposition**, revealing hierarchical layering;
* **Community detection algorithms** (Louvain / Leiden), identifying narrative or institutional clusters.

In the O2O network, a **de-centering experiment** was conducted by removing the Shanghai General Chamber of Commerce as a node, in order to test whether it functioned as a unique center or as part of a resilient multi-centered structure.

---

## 7. Methodological Summary

Through the above procedures, this study establishes a reproducible analytical pipeline that transforms historical newspaper texts into multi-layered relational networks. This approach enables a structural interpretation of the Shanghai General Chamber of Commerce’s position within modern Chinese society that complements traditional institutional and narrative histories.

---

# Interpreting the Three Network Types in Historical Terms

## 1. Organization–Organization (O2O) Network: Institutional Embeddedness

The organizational network demonstrates that:

1. The Shanghai General Chamber of Commerce occupied a position of high centrality but did not constitute a monopolistic hub;
2. Its core institutional layer consisted primarily of banking associations, native-place chambers, trade guilds, and government agencies;
3. After removal of the SGCC node, the remaining network retained coherence and exhibited a polycentric structure.

**Historical implication:**
The SGCC functioned less as a command center than as an institutional broker embedded within a broader commercial–administrative ecology.

---

## 2. Person–Person (P2P) Network: Elite Structure and Brokerage

The person network reveals that:

* A small number of individuals display exceptionally high betweenness centrality;
* These figures connect commercial organizations with political authorities and social associations;
* The person network exhibits greater stability than the organizational network.

**Historical implication:**
The Chamber was sustained not merely by formal rules but by a network of cross-sector elites who mediated between commerce, state, and society.

---

## 3. Event–Event (E2E) Network: Narrative and Historical Contexts

The event network shows that:

* The Shanghai General Chamber of Commerce was deeply embedded in major national and social events, such as the May Thirtieth Movement, the Northern Expedition, and the National Products Movement;
* Leiden communities correspond to distinct narrative clusters;
* k-core analysis reveals layered structures of core and peripheral events.

**Historical implication:**
The Chamber’s public presence extended well beyond economic affairs, positioning it as an actor within national political and social mobilization.

---

# Technical Workflow Description (for Figure Caption)

**Figure 1. Analytical Workflow of the Historical Network Analysis**

*This figure illustrates the analytical pipeline employed in the study, from text retrieval in* Shenbao *(1902–1929), through contextual delimitation and named entity recognition, to co-occurrence modeling and the construction of organization, person, and event networks, followed by structural network analysis.*

```
Shenbao Full Text (1902–1929)
          │
          ▼
Chamber-related Text Retrieval
          │
          ▼
Contextual Windows (±100 characters)
          │
          ▼
Named Entity Recognition (ORG / PERSON / EVENT)
          │
          ▼
Entity Normalization and Cleaning
          │
          ▼
Co-occurrence Modeling (same article + distance ≤15)
          │
          ▼
 ┌──────────────┬──────────────┬──────────────┐
 │ O2O Network  │ P2P Network  │ E2E Network  │
 │ Org–Org     │ Person–Person│ Event–Event  │
 └──────────────┴──────────────┴──────────────┘
          │
          ▼
Centrality / k-core / Community Detection
          │
          ▼
Historical Interpretation of the SGCC
```

